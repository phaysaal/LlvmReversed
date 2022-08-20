module L = Llvm
module O = L.Opcode
module F = Ftools
module VK = L.ValueKind
module SDep = Map.Make(String)
module Deps = Set.Make(String)

exception OPC of string
            
let is_ptr_element lli =
  let vt = L.classify_value lli in
  match vt with
    VK.GlobalVariable ->
     false
  | VK.ConstantExpr ->
     true
  | VK.Instruction opcode ->
     opcode = O.GetElementPtr
  | _ -> raise (OPC "@is_ptr_element")
;;

let is_ptr_element_exp lli =
  L.classify_value lli = L.ValueKind.ConstantDataArray
;;

let is_const_exp lli =
  L.classify_value lli = L.ValueKind.ConstantExpr
;;

let is_const_int lli =
  L.classify_value lli = L.ValueKind.ConstantInt
;;

let is_null lli =
  let vk = L.classify_value lli in
  vk = L.ValueKind.NullValue || vk = ConstantPointerNull
;;


let rec is_array llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> false
  | Llvm.TypeKind.Function -> false
  | Llvm.TypeKind.Array    -> true
  | Llvm.TypeKind.Pointer  -> is_array (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> is_array (Llvm.element_type llty)
  | _                      -> false
;;

let rec is_struct llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Struct  -> true
  | Llvm.TypeKind.Function -> false
  | Llvm.TypeKind.Array    -> false
  | Llvm.TypeKind.Pointer  -> is_struct (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> false
  | _                      -> false
;;

let is_const = L.is_constant ;;

let is_pointer lli = L.classify_type (L.type_of lli) = L.TypeKind.Pointer ;;

let get_deps_instr lli =
  try
    let opcode = L.instr_opcode lli in
    match opcode with
    | O.Br ->
       begin match L.get_branch lli with
         Some (L.(`Unconditional blk)) ->
          Deps.singleton @@ Print.get_blk_name blk
       | Some L.(`Conditional (cond, tr_blk, fl_blk)) ->
          Deps.add (Print.get_blk_name tr_blk) (Deps.singleton @@ Print.get_blk_name fl_blk)
       | _ -> Deps.empty
       end
    | _ -> Deps.empty
  with
    _ -> Deps.empty
;;
        
let get_deps_blk blk =
  Llvm.fold_left_instrs
      (fun acc lli ->
        let l = get_deps_instr lli in
        Deps.union l acc
      ) Deps.empty blk
            
let get_deps_function name f =
  let blk_arr = Llvm.basic_blocks f in
  Array.fold_left (fun acc blk ->
      let deps = get_deps_blk blk in
      let blk_name : string = Print.get_blk_name blk in
      SDep.add blk_name deps acc
    ) SDep.empty blk_arr


let one_step_traverse graph =
  let new_graph =
    SDep.map (fun callees ->
      Deps.fold (fun callee callees ->
          let callees_of_callee = SDep.find callee graph in
          Deps.union callees_of_callee callees
        ) callees callees
      ) graph in
  let is_fixpoint = SDep.equal (fun a b -> Deps.equal a b) new_graph graph in
  is_fixpoint, new_graph

let get_all_deps name f =
  let graph = get_deps_function name f in
  F.lfp one_step_traverse graph

let get_mv_instr lli =
  try
    let opcode = L.instr_opcode lli in
    match opcode with
    |	Store ->
       let dest = L.operand lli 1 in
       let dest_v = L.value_name dest in
       Deps.singleton dest_v  
    | _ -> Deps.empty
  with
    _ -> Deps.empty
        
        
let get_mv_blk blk =
  Llvm.fold_left_instrs
      (fun acc lli ->
        let l = get_mv_instr lli in
        Deps.union l acc
      ) Deps.empty blk
            
let get_mv_function name f =
  let blk_arr = Llvm.basic_blocks f in
  Array.fold_left (fun acc blk ->
      let deps = get_mv_blk blk in
      let blk_name : string = Print.get_blk_name blk in
      SDep.add blk_name deps acc
    ) SDep.empty blk_arr


let build_call_mv calls mvs =
  SDep.mapi (fun blk_name calls ->
      Deps.fold (fun c acc ->
          Deps.union acc @@ SDep.find c mvs
        ) calls @@ SDep.find blk_name mvs
    ) calls
  
let get_all_mvs name f =
  (* let call_graph = get_all_deps name f in *)
  (* SDep.iter (fun f cs ->
      F.pw f; F.pw " ->"; F.iterS F.pw "," (Deps.elements cs); F.pn ""
    ) call_graph; *)
  let mvs = get_mv_function name f in
  (* SDep.iter (fun f cs ->
      F.pw f; F.pw " ->"; F.iterS F.pw "," (Deps.elements cs); F.pn ""
    ) mvs; *)
  (* let r = build_call_mv call_graph mvs in *)
  (* SDep.iter (fun f cs ->
      F.pw f; F.pw " ->"; F.iterS F.pw "," (Deps.elements cs); F.pn ""
    ) r; *)
  mvs
  
