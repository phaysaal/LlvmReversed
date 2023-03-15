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

let get_call allfuncs lli =
  try
    let opcode = L.instr_opcode lli in
    match opcode with
      L.Opcode.Call
    | L.Opcode.Invoke ->
       let argn = L.num_operands lli in
       let op2 = L.operand lli (argn-1) in
       let vk = L.classify_value op2 in
       let fname = L.value_name op2 in
       
       if vk = VK.Function && Deps.mem fname allfuncs then
         Deps.singleton fname
       else
         if vk = VK.GlobalAlias && L.num_operands op2 = 1 then
           let fname' = L.value_name (L.operand op2 0) in
           if Deps.mem fname' allfuncs then
               Deps.singleton fname'
           else
             Deps.empty
         else 
           Deps.empty
    | _ -> Deps.empty
  with  _ -> Deps.empty

let is_assert_fail lli =
  try
    let opcode = L.instr_opcode lli in
    match opcode with
      L.Opcode.Call ->
       let argn = L.num_operands lli in
       let op2 = L.operand lli (argn-1) in
       let vk = L.classify_value op2 in
       if vk = VK.Function then
         begin
           let fname = L.value_name op2 in
           fname = "__assert_fail"
         end
       else
         false
    | _ -> false
  with  _ -> false
;;

  
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
  
let get_deps_function f =
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

let get_all_deps f =
  let graph = get_deps_function f in
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
  let mvs = get_mv_function name f in
  mvs

let construct_blk_by_br vars blk_arr blk =
  let rec aux blk_arr1 blk =
    let last_inst = L.instr_end blk in
    match last_inst with
      L.After inst ->
       begin
         match L.classify_value inst with
           VK.Instruction O.Resume ->
            blk_arr1
         | VK.Instruction O.Ret ->
            blk_arr1
         | VK.Instruction O.Unreachable ->
            blk_arr1
         | VK.Instruction O.Br ->
            begin
              match L.get_branch inst with
                Some lbl ->
                 begin
                   match lbl with
                     `Conditional (_, tblk, fblk) ->
                      (* F.dbgf "IF" "Br %s -> %s, %s" (P.get_blk_name blk) (P.get_blk_name tblk) (P.get_blk_name fblk); *)
                      let blk_arr2 =
                        if List.mem tblk blk_arr1 then
                          blk_arr1
                        else
                          let blk_arr1' = blk_arr1@[tblk] in
                          aux blk_arr1' tblk
                      in
                      let blk_arr3 =
                        if List.mem fblk blk_arr2 then
                          blk_arr2
                        else
                          let blk_arr1' = blk_arr2@[fblk] in
                          aux blk_arr1' fblk
                      in
                      blk_arr3
                   | `Unconditional (blk1) ->
                      (* F.dbgf "IF" "Invoke %s -> %s" (P.get_blk_name blk) (P.get_blk_name blk1); *)
                      if List.mem blk1 blk_arr1 then
                        blk_arr1
                      else
                        let blk_arr1' = blk_arr1@[blk1] in
                        aux blk_arr1' blk1
                 end
              | None ->
                 blk_arr1
            end
         | VK.Instruction O.Invoke ->
            let on = L.num_operands inst in
            let tblk = L.block_of_value @@ L.operand inst (on-3) in
            let fblk = L.block_of_value @@ L.operand inst (on-2) in
            
            (* F.dbgf "IF" "Invoke %s -> %s, %s" (P.get_blk_name blk) (P.get_blk_name tblk) (P.get_blk_name fblk); *)
            let blk_arr2 =
              if List.mem tblk blk_arr1 then
                blk_arr1
              else
                let blk_arr1' = blk_arr1@[tblk] in
                aux blk_arr1' tblk
            in
            (* F.dbgf "IF" "True side: %a" (F.fstrL F.fstrId ",") (List.map P.get_blk_name blk_arr2); *)
            let blk_arr3 =
              if List.mem fblk blk_arr2 then
                blk_arr2
              else
                let blk_arr1' = blk_arr2@[fblk] in
                aux blk_arr1' fblk
            in
            blk_arr3
         | _ ->
            blk_arr1
       end
    | _ -> blk_arr1
  in
  aux [blk] blk 
;;

let return_at_end blk =
  let last_inst = L.instr_end blk in
  match last_inst with
    L.After inst ->
     begin
       match L.classify_value inst with
         L.ValueKind.Instruction L.Opcode.Ret ->
          true
       | _ -> false
     end
  | _ -> false
;;

let has_return blk_arr = List.exists return_at_end blk_arr
;;

let get_calls_in_blk allfuncs blk =
  Llvm.fold_left_instrs (fun acc ins ->
      Deps.union acc @@ get_call allfuncs ins
    ) Deps.empty blk
;;

let get_calls_in_blk_arr allfuncs blk_arr =
  Array.fold_left (fun acc blk -> Deps.union acc @@ get_calls_in_blk allfuncs blk) Deps.empty blk_arr
;;

let get_calls_in_func allfuncs f =
  let blk_arr = Llvm.basic_blocks f in
  get_calls_in_blk_arr allfuncs blk_arr
;;

let get_call_edges allfuncs llm =
  L.fold_left_functions
    (fun acc f ->
      let calls = get_calls_in_func allfuncs f in
      SDep.add (L.value_name f) calls acc
    ) SDep.empty llm

let has_assert_fail_in_blk blk =
  Llvm.fold_left_instrs (fun acc ins ->
      if acc then acc else is_assert_fail ins
    ) false blk

let has_assert_fail blk_arr = Array.exists has_assert_fail_in_blk blk_arr
;;                         

let has_func_assert_fail f =
  let blk_arr = Llvm.basic_blocks f in
  has_assert_fail blk_arr
;;

let get_args allfuncs lli =
  let n = L.num_operands lli in
  let rec get_params i n acc =
    if i<n-1 then
      begin
        let p = L.operand lli i in
        let ps = L.value_name p in
        if Deps.mem ps allfuncs then
          begin
            get_params (i+1) n (ps::acc)
          end
        else
          get_params (i+1) n acc
      end
    else
      acc in
  get_params 0 n []

let funs_as_fp_in_lli allfuncs lli =
  try
    let opcode = L.instr_opcode lli in
    match opcode with
      L.Opcode.Call
    | L.Opcode.Invoke ->
       
       let argn = L.num_operands lli in
       let op2 = L.operand lli (argn-1) in
       let vk = L.classify_value op2 in
       let fname = L.value_name op2 in
       
       if vk = VK.Function && Deps.mem fname allfuncs then
         get_args allfuncs lli
       else
         []
    | _ -> []
  with  _ -> []
           
let funs_as_fp_in_blk allfuncs blk =
  Llvm.fold_left_instrs (fun acc ins ->
      let fps = funs_as_fp_in_lli allfuncs ins in
      acc @ fps
    ) [] blk
;;

let funs_as_fp_in_blocks all_funcs blk_arr =
  Array.fold_left (fun acc blk ->
      let fps = funs_as_fp_in_blk all_funcs blk in
      acc @ fps
    ) [] blk_arr

let funs_as_fp_in_func all_funcs f =
  let blk_arr = Llvm.basic_blocks f in
  funs_as_fp_in_blocks all_funcs blk_arr

  
let funs_as_fp_in_llm all_funcs llm =
  let fps =
    L.fold_left_functions
      (fun acc f ->
        let calls = funs_as_fp_in_func all_funcs f in
        acc @ calls 
    ) [] llm in
  fps
  

let construct_assert_map llm =
  let all_funcs = L.fold_left_functions (fun acc f ->
                      let fname = L.value_name f in
                      Deps.add fname acc) Deps.empty llm in
  let func_with_assert_fails =
    L.fold_left_functions
      (fun acc f ->
        let fname = L.value_name f in
        let s = has_func_assert_fail f in 
        if s then
          acc@[fname]
        else
          acc
    ) [] llm in
  let call_edges = get_call_edges all_funcs llm in
  SDep.iter (fun k v ->
      if not (Deps.is_empty v) && k="main" then F.dbgf "CALLGRAPH" "main in Call_edges\n%s -> %a\n" k (F.fstrL F.fstrId ",") (Deps.elements v) 
    ) call_edges;
  
  let call_graph = F.lfp one_step_traverse call_edges in
  SDep.iter (fun k v ->
      if not (Deps.is_empty v) && k="main" then F.dbgf "CALLGRAPH" "main in Call_graph\n%s -> %a\n" k (F.fstrL F.fstrId ",") (Deps.elements v) 
    ) call_graph;

  
  let funs_as_fp : string list = funs_as_fp_in_llm all_funcs llm in

  let assert_map =
    SDep.fold
      (fun k v acc ->
        if List.mem k func_with_assert_fails
           || Deps.exists (fun v' -> List.mem v' func_with_assert_fails) v
           || List.mem k funs_as_fp
        then
          Deps.add k acc
        else
          acc)
      call_graph Deps.empty in
  (* Deps.iter (fun a -> F.pw a) assert_map;
  F.pn ""; *)
  assert_map
    
