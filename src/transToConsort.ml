module L = Llvm
module VK = L.ValueKind
module O = L.Opcode
module I = L.Icmp
(* module C = ConsortAst
module F = Ftools
module VR = Map.Make(String)
module P = Print
 *)
         
exception Err of string
exception OPC of string

(*
let func_dir = ref "";;

let pf = Printf.printf ;;
let pn = F.pn ;;
let stv = L.string_of_llvalue;;

let temps = ref [];;

let structures : (string * L.lltype list) list ref = ref [];;

let add_struct lli =
  let module LT = L.TypeKind in
  let rec is_struct llt =
    match L.classify_type llt with
      LT.Struct ->
      Some llt
    | LT.Integer ->
       None
    | LT.Function ->
       None
    | LT.Array
      | LT.Pointer
      | LT.Vector
      ->
       is_struct @@ L.element_type llt
    | _ -> None
  in
  let llt = L.type_of lli in

  match is_struct llt with
    None -> ()
  | Some llt ->
     let nm = L.struct_name llt in
     let flds = L.struct_element_types llt in
     let l_flds = Array.to_list flds in
     match nm with
       Some snm ->
       structures := !structures @ [(snm, l_flds)]
     | None ->
        ()
;;

let get_structures () : int VR.t =
  let structures =
    List.fold_left (fun acc (s, flds) ->
        VR.add s (List.length flds) acc
      ) VR.empty !structures in
  structures
;;
                     
let vars = ref VR.empty;;


let get_attr is_strict vn =
  try
    VR.find vn !vars
  with
    Not_found ->
     if is_strict then
       raise Not_found
     else
       []
;;

let var v = C.(`OVar v);;
let const c = C.(`OInt c);;
let add x y = C.(`BinOp (x, "+", y));;

let string_of_lhs = function
    `OVar s -> s
  | 


let get_var  ?(is_global=false) lli =
  let vn = L.value_name lli in
  F.dbg "DEB" "vn:" F.p vn;  
  var vn
;;

let declared = ref [];;

let is_declared v =
  List.mem v !declared
;;

let fresh = ref 0;;

let fresh_var () =
  let v = string_of_int !fresh in
  let r = var v  in
  fresh := !fresh + 1;
  r
;;

let write_func fname filename params body =
  if !func_dir <> "" then
    begin
      let file = F.make_path !func_dir filename fname in
      let out = open_out file in
      let pr = (fname, params, body) in
      Marshal.to_channel out pr [];
      close_out out
    end
  else
    begin
      raise (Err "Func dir is blank\n")
    end
;;

(*
let rec print_while_t ind = function
  | WBLK ts ->
     Printf.printf "WBLK%s{\n" (P.space ind);
     List.iter (fun t -> print_while_t (ind+1) t) ts;
     Printf.printf "%s}\n" (P.space ind);
  | WINST lli ->
     Printf.printf "WINST%s%s\n" (P.space ind) (Llvm.string_of_llvalue lli)
  | WPROG s ->
     Printf.printf "WPROG"; B.pprint ind s
  | WWHILE (b, t) ->
     Printf.printf "WWHIL%swhile(%s)\n" (P.space ind) (V.BExp.fstr () b);
     print_while_t ind t
  | WIF (b, t1, t2) ->
     Printf.printf "WIF %sif(%s){\n" (P.space ind) (V.BExp.fstr () b);
     print_while_t (ind + 1) t1;
     Printf.printf "%s} else {\n" (P.space ind);
     print_while_t (ind + 1) t2;
     Printf.printf "%s}" (P.space ind);
;;
 *)

(*
let rec to_while_t = function
  | WBLK ts ->
     let ts' = List.map to_while_t ts in
     let t' = B.join_progs ts' in
     B.BLOCK (t', B.SKIP, B.dl)
  | WINST lli ->
     raise (Err ("Please translate llvm code " ^ (L.string_of_llvalue lli) ^ " first"))
  | WPROG s ->
     s
  | WWHILE (b, t) ->
     B.WHILE (b, [], to_while_t t, [], B.SKIP, B.dl)
  | WIF (b, t1, t2) ->
     B.IF (b, to_while_t t1, to_while_t t2, B.SKIP, B.dl)
;;
 *)

let is_ptr_element lli =
  let opcode = (try L.instr_opcode lli with _ -> raise (OPC "@is_ptr_element")) in
  opcode = O.GetElementPtr
;;

let is_ptr_element_exp lli =
  let vk = L.classify_value lli in
  F.pn_s "DEB" (Print.print_value_kind vk);
      
  vk = L.ValueKind.ConstantDataArray
;;

let is_const_exp lli =
  let vk = L.classify_value lli in    
  vk = L.ValueKind.ConstantExpr
;;

let is_const_int lli =
  let vk = L.classify_value lli in    
  vk = L.ValueKind.ConstantInt
;;

let is_null lli =
  let vk = L.classify_value lli in    
  vk = L.ValueKind.NullValue || vk = ConstantPointerNull
;;

(* Get Constant int or var *)
let rec get_exp_from_llvalue lli : C.lhs =
  let v =
    if is_const_int lli then
      let r =
        match L.int64_of_const lli with
          Some s -> (Int64.to_int s)
        | None ->
           begin
             F.pn_s "DEB" (L.string_of_llvalue lli);
             raise (Err "Not an integer (get_exp_from_llvalue)")
           end
      in 
      const r
    else if is_const_exp lli then
      begin
        get_const_expr lli
      end
    else if is_null lli then
      begin
        const 0
      end
    else
      begin
        let s = L.value_name lli in
        
        if s = "" then
          get_exp_from_ref lli
        else
          get_var lli
      end
  in
  v

(* Get exp from 'load' *)
and get_exp_from_ref lli =
  let opcode = try L.instr_opcode lli with e -> Printf.printf "%s\n" (L.string_of_llvalue lli); raise e in
  match opcode with
    O.Load
  | O.BitCast
  |	ExtractValue ->
     begin
       let opc = L.operand lli 0 in
       try
         get_exp_from_llvalue opc
       with
         e ->
         Printf.printf "get_exp_from_ref %s\n" (L.string_of_llvalue lli);
         raise e
     end
  | LandingPad ->
     const 0
  | Alloca ->
     raise (Err "NotSupported but should be supported")     
  | _ ->
     Printf.printf "get_exp_from_ref %s\n" (L.string_of_llvalue lli);
     raise (Err "Other opcode in get_exp_from_ref")

and get_ptr_element lli =
  let arr = L.operand lli 0 in
  let e_arr = get_var arr in
  let arr0 = L.operand arr 0 in
  let ty = L.type_of arr0 in
  let tp = ty |> L.classify_type in
  match tp with
  | L.TypeKind.Array ->
      let idx = L.operand lli 2 in
      let e_idx = get_exp_from_llvalue idx in
      let s_fld = "*" in
      let ptr = add e_arr e_idx in
      ptr, s_fld
  | Pointer ->
     let idx = L.operand lli 1 in
     let e_idx = get_exp_from_llvalue idx in
     let s_fld = "-" in
     let ptr = add e_arr e_idx in
     ptr, s_fld     
  | _ ->
      let idx = L.operand lli 1 in
      let fld = L.operand lli 2 in
      
      let e_idx = get_exp_from_llvalue idx in
      let e_fld = get_exp_from_llvalue fld in
      let s_fld = string_of_lhs e_fld in
      let ptr = add e_arr e_idx in
      ptr, s_fld

and get_const_expr lli =
  get_exp_from_llvalue (L.operand lli 0)
;;

(* Get BExp.UNIT  *)
let rec get_bexp_from_comp lli =
  let opcode = L.instr_opcode lli in
  match opcode with
    O.ICmp ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let e_arg1 = try get_exp_from_llvalue arg1 with e -> Printf.printf "arg1\n"; raise e in
     let e_arg2 = try get_exp_from_llvalue arg2 with e -> Printf.printf "arg2\n"; raise e in
     let pred = L.icmp_predicate lli in
     (* pf "ICmp:\narg1:%s\narg2:%s\n" (Base.Exp.fstr () e_arg1) (Base.Exp.fstr () e_arg2); *)
     begin
       match pred with
         Some L.Icmp.Slt
       | Some L.Icmp.Ult ->
          V.BExp.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2)
       | Some L.Icmp.Sgt
       | Some L.Icmp.Ugt ->
          V.BExp.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1)
       | Some L.Icmp.Sle
       | Some L.Icmp.Ule ->
          V.BExp.OP (
          V.BExp.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2), V.Op.OR,
          V.BExp.UNIT (B._T e_arg1, V.Op.EQ, B._T e_arg2))
       | Some L.Icmp.Sge
       | Some L.Icmp.Uge ->
          V.BExp.OP (
          V.BExp.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1), V.Op.OR,
          V.BExp.UNIT (B._T e_arg2, V.Op.EQ, B._T e_arg1))
       | Some L.Icmp.Eq ->
          V.BExp.UNIT (B._T e_arg2, V.Op.EQ, B._T e_arg1)
       | Some L.Icmp.Ne ->
          V.BExp.UNIT (B._T e_arg2, V.Op.NE, B._T e_arg1)
       | _ -> raise (Err "Not compatible yet.")
     end
  | Trunc ->
     get_bexp_from_comp @@ L.operand lli 0
  | Load ->
     let op1 = L.operand lli 0 in
     let v = get_exp_from_llvalue op1 in
     V.BExp.UNIT (B._T v, V.Op.NE, B._T (B.const 0))
  | Or ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let e_arg1 = try get_bexp_from_comp arg1 with e -> Printf.printf "arg1\n"; raise e in
     let e_arg2 = try get_bexp_from_comp arg2 with e -> Printf.printf "arg2\n"; raise e in
     V.BExp.OP (e_arg1, V.Op.OR, e_arg2)
  | And ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let e_arg1 = try get_bexp_from_comp arg1 with e -> Printf.printf "arg1\n"; raise e in
     let e_arg2 = try get_bexp_from_comp arg2 with e -> Printf.printf "arg2\n"; raise e in
     V.BExp.OP (e_arg1, V.Op.AND, e_arg2)
  | _ ->
     pn "BExp Exception in";
     pn @@ stv lli;
     raise (Err "Not supported opcode in bexp")

let get_casted_from lli = L.operand lli 0 ;;

let is_pointer lli = L.classify_type (L.type_of lli) = L.TypeKind.Pointer ;;

let is_cast lli = (try L.instr_opcode lli with _ -> raise (OPC "is_cast")) = O.BitCast ;;
  
let is_fcall lli =
  try
    let opcode = L.instr_opcode lli in
    if opcode = O.Call then
      true
    else
      let operand0 = L.operand lli 0 in
      let opcode2 = L.instr_opcode operand0 in
      (opcode = O.BitCast && opcode2 = O.Call)
  with
    _ -> false
;;

let is_indirect lli =
  let opcode = (try L.instr_opcode lli with _ -> raise (OPC "is_indorect")) in
  opcode = O.Load && is_pointer lli;;

let is_const = L.is_constant ;;

let get_call lli =
  if is_cast lli then
    let casted_from = get_casted_from lli in
    casted_from
  else
    lli
  
let get_called_fname lli =
  let argn = L.num_operands lli in
  let op2 = L.operand lli (argn-1) in
  let fname = get_var op2 in
  fname
;;


let is_load lli =
  let opcode = (try L.instr_opcode lli with _ -> raise (OPC "@is_load")) in
  opcode = O.Load
;;

let is_array_type tp =
  L.classify_type tp = L.TypeKind.Array
;;


let get_parameters lli =
  let n = L.num_operands lli in
  let rec get_params i n acc =
    if i<n-3 then
      begin
        let p = L.operand lli i in

        let v = get_exp_from_llvalue p |> B._T in
        get_params (i+1) n (acc @ [v])
      end
    else
      acc in
  get_params 0 n []
;;

let get_invoke_parameters lli =
  let n = L.num_operands lli in
  let rec get_params i n acc =
    if i<n-1 then
      begin
        let p = L.operand lli i in

        let v = get_exp_from_llvalue p |> B._T in
        get_params (i+1) n (acc @ [v])
      end
    else
      acc in
  get_params 0 n []
;;

let mk_assign_maybe lli o =
  let vname = L.value_name lli in
  let t_operand i = let opc = L.operand lli i in
                    let opv = try get_exp_from_llvalue opc with e -> Printf.printf "mk_assign_maybe\n"; raise e in
                    let t_opc = B._T opv in
                    t_opc in

  if vname = "" then
    B.SKIP
  else
    let v_vname = get_var lli in
    let bin op =
      let opr1 = t_operand 0 in
      let opr2 = t_operand 1 in
      let t = B.T.op opr1 opr2 op in
      B.assign v_vname t in
    match o with
      O.BitCast
    | O.Trunc ->
       let t_opc = t_operand 0 in
       B.assign v_vname t_opc
    | O.Add ->
       bin V.Op.ADD
    | O.Sub ->
       bin V.Op.SUB
    | O.Or
      | O.And
    | _ -> B.SKIP

let rec to_while_t_inst lli =
  let st l = L.string_of_llvalue l in
  let str = st lli in
  F.dbg "DEB" "Current Instr:" F.p str;
  let opcode = try L.instr_opcode lli with  _ -> pf "OPCode - 1"; raise (OPC "OpCode -1") in
  let r =
    match opcode with
    | O.Invalid -> B.SKIP
    | O.Ret ->
       if L.num_operands lli = 0 then
         B.return (B._T @@ V.Exp.CONST 0)
       else
         begin
           let rt = L.operand lli 0 in
           let ty = L.type_of rt in
           let tv = L.classify_type ty in
           match tv with
             L.TypeKind.Void ->
              raise (Err "N/S Void")
            |	Integer ->
               if L.is_constant rt then
                 let e = try get_exp_from_llvalue rt with e -> Printf.printf "to_while_t_inst(Ret)\n"; raise e in
                 B.return (B._T e)
               else
                 if is_load rt then
                   let dt = L.operand rt 0 in
                   if is_ptr_element dt then
                     let e_vname = get_var dt in
                     let t_vname = B._T e_vname in
                     let p2 = B.return t_vname in
                     let ptr, fld = get_ptr_element dt in
                     let p1 = B.lookup e_vname ptr fld in
                     let p = B.join_at_last p2 p1 in
                     p
                   else
                     begin
                       let e_dt = get_exp_from_llvalue dt in
                       let t_dt = B._T e_dt in
                       B.return t_dt

                     end
                 else
                   raise (Err "Not a load in Return")
            | X86_amx ->
               B.return (B._T @@ V.Exp.CONST 0)
            | _ ->
               pf "Other kinds of return: %d %s\n" (L.num_operands lli) (L.string_of_llvalue lli);
               B.SKIP
         end 
    | O.Br ->
       B.SKIP
    | O.Switch ->
       raise (Err "Switch - Not understood yet")
    | O.IndirectBr ->
       raise (Err "Switch - Not understood yet")
    |	O.Add ->
       mk_assign_maybe lli O.Add
    |	Or ->
       mk_assign_maybe lli O.Or
    |	O.Alloca ->
       let op = L.operand lli 0 in
       F.pn_s "DEB" (st op);
       F.pn_s "DEB" (P.print_value_kind (L.classify_value op));
       if L.is_constant op then
         begin
           let size = 
             begin
               match L.int64_of_const op with
                 Some s ->
                  (Int64.to_int s)
               | None ->
                  begin
                    pf "\n%s\n" (L.string_of_llvalue lli);
                    raise (Err "Not an integer")
                  end
             end in
           add_struct lli;
           let r = B.decl (get_var lli) size in
           r
         end
       else
         begin
           let r = B.decl (get_var lli) 1 in
           r
         end
    |	Load | ExtractValue ->
       let lli_name = L.value_name lli in
       if lli_name = "" then
         B.SKIP
       else
         let src = L.operand lli 0 in
         let e_dest = get_var lli in
         let t_src = get_exp_from_llvalue src |> B._T in
         let p = B.assign e_dest t_src in
         p
    |	Store ->
       let src = L.operand lli 0 in
       let dest = L.operand lli 1 in
       let dest_v = get_var dest in
       if is_indirect dest then
         begin
           let destv' = L.operand dest 0 |> get_var in
           let e_src = try get_exp_from_llvalue src with e -> pf "STore\n"; raise e in
           B.mutation (B._T destv') "*" (B._T e_src)
         end
       else if is_ptr_element dest then
         begin
           let e_src = try B._T (get_exp_from_llvalue src) with e ->
                         pn str;
                         pf "Store array element B\n"; raise e in
           let ptr, fld = get_ptr_element dest in
           B.mutation ptr fld e_src
         end
       else if is_const src then
         begin
           let e_src = try get_exp_from_llvalue src with e -> pf "Store array element C\n"; raise e in
           B.assign dest_v (B._T e_src)
         end
       else if is_fcall src then
         begin
           try
             let called_fun = get_call src in
             let fname = get_called_fname called_fun in
             let sname = fst @@ E.decode fname in
             if sname = "malloc" then
               begin
                 let op1 = L.operand called_fun 0 in
                 let size = try get_exp_from_llvalue op1 with e -> pf "Store array element D\n"; raise e in
                 B.malloc dest_v size
               end
             else
               begin
                 
                 let params = get_parameters called_fun in
                 let t_fname = B._T @@ fname in
                 let p1 = B.call t_fname params in
                 let p2 = B.assign dest_v (B.ret (get_attr false sname)) in
                 B.join_at_last p2 p1
               end
           with
             e ->
             pf "exception at fcall";
             pf "%s" str;
             raise e
         end
       else if is_pointer src then
         (* let src_name = L.value_name src in
       if src_name = "" then
         begin
           
         end
       else *)
         begin
           let t_src = B._T @@ get_exp_from_llvalue src in
           let v_dest = get_exp_from_llvalue dest in
           B.assign v_dest t_src
         end
       else
         begin
           let t_src = B._T @@ get_exp_from_llvalue src in
           let v_dest = get_exp_from_llvalue dest in
           B.assign v_dest t_src
         end
    |	GetElementPtr ->
       let vn = L.value_name lli in
       if vn = "" then
         B.SKIP
       else
         begin
           try
             let e_vname = get_var lli in
             let ptr, fld = get_ptr_element lli in
             if fld = "-" then
               B.assign e_vname ptr
             else
               B.lookup e_vname ptr fld
           with
             e ->
             F.pn "Exception at GetElementPtr";
             raise e
         end
    |	ZExt
      |	Trunc ->
       mk_assign_maybe lli O.Trunc
    |	SExt ->
       (* PERHAPS IT IS SAME AS STORE *)
       let src = L.operand lli 0 in
       let dest_v = get_var lli in
       if is_fcall src then
         begin
           try
             let called_fun = get_call src in
             let fname = get_called_fname called_fun in
             let sname = fst @@ E.decode fname in
             if sname = "malloc" then
               begin
                 let op1 = L.operand called_fun 0 in
                 let size = try get_exp_from_llvalue op1 with e -> pf "Store array element D\n"; raise e in
                 B.malloc dest_v size
               end
             else
               begin
                 (* pf "%s || %s || %d\n" (L.value_name src) fname (L.num_operands called_fun); *)
                 let params = get_parameters called_fun in
                 let t_fname = B._T @@ get_var called_fun in
                 let p1 = B.call t_fname params in
                 let p2 = B.assign dest_v (B.ret @@ get_attr false sname) in
                 B.join_at_last p2 p1
               end
           with
             e ->
             pf "exception at fcall";
             pf "%s" str;
             raise e
         end
       else
         let src_exp = B._T @@ get_exp_from_llvalue src in
         B.assign dest_v src_exp  
    |	BitCast ->
       mk_assign_maybe lli O.BitCast
    |	O.ICmp ->
       B.SKIP
    |	Call ->
       let vv = L.value_name lli in
       if vv = "" then     
         let called_fun = get_call lli in
         let fname = get_called_fname called_fun in
         let params = get_parameters called_fun in
         let t_fname = B._T fname in (* @@ get_var called_fun in *)
         let p1 = B.call t_fname params in
         p1
       else
         B.SKIP
    | Invoke ->
       (* pn @@ stv lli;
     pf "#1.%s \n#2.%s \n#3.%s \n#4.%s \n#5.%s \n#6.%s \n"
       (L.operand lli 0 |> stv)
       (L.operand lli 1 |> stv)
       (L.operand lli 2 |> stv)
       (L.operand lli 3 |> stv)
       (L.operand lli 4 |> stv)
       (L.operand lli 5 |> stv); *)
       let vv = L.value_name lli in
       if vv = "" then     
         let called_fun = get_call lli in
         let fname = get_called_fname called_fun in
         let params = get_invoke_parameters called_fun in
         let t_fname = B._T fname in (* @@ get_var called_fun in *)
         let p0 = B.call t_fname params in
         let on = L.num_operands lli in
         let p1_blk = L.block_of_value @@ L.operand lli (on-3) in
         let p2_blk = L.block_of_value @@ L.operand lli (on-2) in
         let p1 = L.fold_right_instrs (fun i acc -> B.join_at_last acc (to_while_t_inst i)) p1_blk B.SKIP in
         let p2 = L.fold_right_instrs (fun i acc -> B.join_at_last acc (to_while_t_inst i)) p2_blk B.SKIP in
         let b = V.BExp.non_det in
         let p012 = B.mk_if b p1 p2 in
         B.join_at_last p012 p0
       else
         B.SKIP
    |	LandingPad->
       let lli_name = L.value_name lli in
       if lli_name = "" then
         B.SKIP
       else
         let e_dest = get_var lli in
         let t_src = B._T @@ B.const 0 in
         let p = B.assign e_dest t_src in
         p
         
         
    |	InsertValue
      -> B.SKIP
    |	Resume
      -> B.SKIP
    |	Sub
      |	And
      -> 
       mk_assign_maybe lli O.Sub
    (* |	Select
    |	UserOp1
    |	UserOp2
    |	VAArg
    |	ExtractElement
    |	InsertElement
    |	ShuffleVector
    |	Fence
    |	AtomicCmpXchg
    |	AtomicRMW
    
     raise (Err str) *)
    (*** 
    |	Invalid2
    |	Unreachable
    |	FAdd	(*	
Standard Binary Operat  ors
     *)
    |	FSub
    |	Mul
    |	FMul
    |	UDiv
    |	SDiv
    |	FDiv
    |	URem
    |	SRem
    |	FRem
    |	Shl	(*	
Logical Operators     
     *)
    |	LShr
    |	AShr
    |	Xor -> raise (Err "Br - Not implemented yet")
       (*	
Other    Operators         
     *)
    |	FCmp
    |	PHI
    |	FPToUI
    |	FPToSI
    |	UIToFP
    |	SIToFP
    |	FPTrunc
    |	FPExt
    |	PtrToInt
    |	IntToPtr
    
     *)

    | _ ->
       raise (Err ("\nUnexpected ValueKind " ^ str ^ "\n"))
  in
  if List.length !temps > 0 then
    List.fold_left (fun acc i -> B.join_at_last r i) r !temps 
  else
    r
         
and to_while_t_blk blk =
  let ps = Llvm.fold_left_instrs
    (fun acc lli ->
      let l = to_while_t_inst lli in
      (* B.pprint 2 l ; *)
      match l with
        B.SKIP ->
         acc
      | _ ->
         acc @ [WPROG l]
    (* acc @ [WINST lli] *)
    ) []
    blk in
  let ps' = List.filter (function
               WPROG B.SKIP -> false
             | WPROG (B.BLOCK (B.SKIP, _, _)) -> false
             | _ -> true
            ) ps in
  match ps' with
    [] -> WPROG B.SKIP
  | _ when List.for_all (function WPROG B.SKIP -> true | _ -> false) ps -> WPROG B.SKIP
  | _ ->
    WBLK ps
;;
            
let rec to_while_t_blks i j blk_arr acc =
  if i <= j then
    begin
      let llbb = Array.get blk_arr i in
      let p = to_while_t_blk llbb in
      
      match p with
        WPROG B.SKIP ->
        to_while_t_blks (i+1) j blk_arr acc
      | _ ->
         to_while_t_blks (i+1) j blk_arr (acc@[p])
    end
  else
    match acc with
      [WBLK acc'] ->
       WBLK acc'
    | _ ->
       WBLK acc  
;;

            
let get_last_br (inst : L.llvalue) =
  match L.classify_value inst with
    L.ValueKind.Instruction (L.Opcode.Br) ->
     let is_cond = L.is_conditional inst in
     if is_cond then
       None
     else
       begin
         match L.get_branch inst with
           Some lbl ->
            begin
              match lbl with
                `Unconditional blk ->
                 let lbl_name = P.get_blk_name blk in
                 Some lbl_name
              | _ ->
                 None
            end
         | None ->
            None
       end
  | _ -> None
;;

let parse_unconditional blk =
  let last_inst = L.instr_end blk in
  match last_inst with
    L.After inst ->
     begin
       match get_last_br inst with
         Some _L1 ->
          Some (inst, _L1)
       | _ -> None
     end
  | _ -> None
;;  

let get_last_cond_br (inst : L.llvalue) =
  match L.classify_value inst with
    L.ValueKind.Instruction (L.Opcode.Br) ->
     let is_cond = L.is_conditional inst in
     if is_cond then
       begin
         match L.get_branch inst with
           Some lbl ->
            begin
              match lbl with
                `Conditional (cond, tblk, fblk) ->
                 let tlbl_name = P.get_blk_name tblk in
                 let flbl_name = P.get_blk_name fblk in
                 Some (cond, tlbl_name, flbl_name)
              | _ ->
                 None
            end
         | None ->
            None
       end
     else
       None
  | _ -> None
;;

let parse_conditional blk =
  let last_inst = L.instr_end blk in
  match last_inst with
    L.After inst ->
     begin
       match get_last_cond_br inst with
         Some (_A, _L2, _L3) ->
          Some (blk, inst, _A, _L2, _L3)
       | _ -> None
     end
  | _ -> None
;;  

let rec search_label _L1 i n blk_arr =
  if i>=n then
    None
  else
    let p1_blk = Array.get blk_arr i in
    match parse_unconditional p1_blk with
      Some (inst, _L1') when _L1'=_L1 ->
      Some (i, inst)
    | _ ->
       search_label _L1 (i+1) n blk_arr
;;

let rec search_block _L1 i n blk_arr =
  if i>n then
    None
  else
    let blk = Array.get blk_arr i in
    let _L1' = P.get_blk_name blk in
    if _L1'=_L1 then
      Some i
    else
      search_block _L1 (i+1) n blk_arr
;;

  
let rec try_parse_while ind k i n (i',n') blk_arr =
  F.dbg "DEB" "Started While Pattern Search from:" F.pl i;
  F.dbg "DEB" "Started While Pattern Search until:" F.pl n;
  if i+2 >= n then
    begin
      try_parse_if ind k i' n' blk_arr
    end
  else
    let status, p =
      begin
        F.dbg "DEB" "Beginning Pattern Search:" F.pl i;
        let p1_blk = Array.get blk_arr i in
        match parse_unconditional p1_blk with
          Some (inst1, _L1) ->
           begin
             F.dbg "DEB" "First Pattern found:" F.p _L1;
             let p2_blk = Array.get blk_arr (i+1) in
             let _L1' = P.get_blk_name p2_blk in
             if _L1 = _L1' then
               let _P2_A_L2_L3 = parse_conditional p2_blk in
               match _P2_A_L2_L3 with
                 Some (_P2, inst2, _A, _L2, _L3) ->
                  F.dbg "DEB" "Second Pattern found:" F.p _L2;
                  let _A = get_bexp_from_comp _A in
                  let p3_blk = Array.get blk_arr (i+2) in
                  let _L2' = P.get_blk_name p3_blk in
                  if _L2=_L2' then
                    match search_label _L1 (i+2) n blk_arr with
                      Some (j, inst3) ->
                       F.dbg "DEB" "Third Pattern found:" F.p _L2;
                       let p4_blk = Array.get blk_arr (j+1) in
                       let _L3' = P.get_blk_name p4_blk in
                       if _L3=_L3' then
                         begin
                           F.dbg "DEB" "Third Pattern Completed:" F.p _L3;
                           L.delete_instruction inst1;
                           L.delete_instruction inst2;
                           L.delete_instruction inst3;
                           F.pn_s "DEB" "Instructions deleted";
                           let _WP1 = to_while_t_blks k (i+1) blk_arr [] in
                           F.dbg "DEB" "While block finished for body:" F.pl i;
                           let _WP21 =
                             if j-i> 2 then
                               begin
                                 F.dbg "DEB" "Next pattern search:" F.pl (i+2);
                                 try_parse_while (ind+1) (i+2) (i+2) j (i+2,j) blk_arr
                               end
                             else
                               begin
                                 F.dbg "DEB" "Translate started from:" F.pl (i+2);
                                 to_while_t_blks (i+2) j blk_arr []
                               end
                           in                           
                           let _WP22 = to_while_t_blk _P2 in
                           let _WP2 = WBLK [_WP21;_WP22] in
                           let _WP3 = try_parse_while ind (j+1) (j+1) n (j+1,n) blk_arr in
                           let _WP = WBLK [_WP1; WWHILE (_A, _WP2); _WP3] in
                           true, _WP
                         end
                       else
                         false, WBLK []
                    | _ ->
                       false, WBLK []
                  else
                    false, WBLK []
               | _ -> false, WBLK []
             else
               false, WBLK []
           end
        | _ ->
           false, WBLK []
      end in
    if status then
      begin
        p
      end
    else
      try_parse_while ind k (i+1) n (i',n') blk_arr

and try_parse_if ind k i n blk_arr =
  F.dbg "DEB" "Started If Pattern Search from:" F.pl i;
  F.dbg "DEB" "Started If Pattern Search until:" F.pl n;
  if i+2 >= n then
    begin
      to_while_t_blks k n blk_arr []
    end
  else
    let status, p =
      begin
        F.dbg "DEB" "Beginning Pattern Search:" F.pl i;
        let p0_blk = Array.get blk_arr i in
        let _P0_A_L1_L2 = parse_conditional p0_blk in
        match _P0_A_L1_L2 with
          Some (_, inst1, _A, _L1, _L2) ->
           F.dbg "DEB" "First Pattern:" F.pl i;
           let p1_blk = Array.get blk_arr (i+1) in
           let _L1' = P.get_blk_name p1_blk in
           if _L1=_L1' then
             let somej = search_block _L2 i n blk_arr in
             match somej with
               Some j ->
                begin
                  F.dbg "DEB" "Second Pattern:" F.pl i;
                 let j' = j - 1 in
                 let p2_blk = Array.get blk_arr j' in
                 let p2lbl = parse_unconditional p2_blk in
                 match p2lbl with
                   Some (inst2, _L4) ->
                    F.dbg "DEB" "Third Pattern:" F.pl i;
                    begin
                      let somek = search_block _L4 j n blk_arr in
                      match somek with
                        Some l ->
                         begin
                           F.dbg "DEB" "Fourth Pattern:" F.pl i;
                           let l' = l - 1 in
                           let p3_blk = Array.get blk_arr l' in
                           let p3lbl = parse_unconditional p3_blk in
                           match p3lbl with
                             Some (inst3, _L4') ->
                              F.dbgf "DEB" "%s\n" (L.string_of_llvalue inst1);
                              F.dbgf "DEB" "%s\n" (L.string_of_llvalue inst2);
                              F.dbgf "DEB" "%s\n" (L.string_of_llvalue inst3);
                              F.dbg "DEB" "Fifth Pattern:" F.pl i;
                              if _L4' = _L4 then
                                begin
                                  let _A = get_bexp_from_comp _A in
                                  L.delete_instruction inst1;
                                  if inst2 = inst3 then
                                    begin
                                      L.delete_instruction inst2;
                                      let _WP0 = to_while_t_blks k i blk_arr [] in
                                      let _WP1 = try_parse_while ind (i+1) (i+1) j' (i+1,j') blk_arr in
                                      let _WP2 = WPROG B.SKIP in
                                      let _WP3 = try_parse_while ind l l n (l,n) blk_arr in
                                      true, WBLK [_WP0; WIF (_A, _WP1, _WP2); _WP3]
                                    end
                                  else
                                    begin
                                      L.delete_instruction inst2;
                                      L.delete_instruction inst3;
                                      let _WP0 = to_while_t_blks k i blk_arr [] in
                                      let _WP1 = try_parse_while ind (i+1) (i+1) j' (i+1,j') blk_arr in
                                      let _WP2 = try_parse_while ind j j l' (j,l') blk_arr in
                                      let _WP3 = try_parse_while ind l l n (l,n) blk_arr in
                                      true, WBLK [_WP0; WIF (_A, _WP1, _WP2); _WP3]
                                    end
                                end
                              else
                                false, WBLK []
                             | None -> false, WBLK []
                         end
                      | None -> false, WBLK []
                    end
                 | None -> false, WBLK []
               end
             | None -> false, WBLK []
           else
             false, WBLK []
        | None -> false, WBLK []
      end in
    if status then
      begin
        p
      end
    else
      try_parse_if ind k (i+1) n blk_arr
     
;;

let translate_llvm_fun f =
  let blk_arr = Llvm.basic_blocks f in
  
  let name = get_var f in
  F.pn_s "PRO" ("Translation Begins for " ^ (E.toStr name) );

  let n = (Array.length blk_arr - 1) in
  F.dbg "DEB" "Number of Parameters:" F.pl n;
  let b = try_parse_while 0 0 0 n (0,n) blk_arr |> to_while_t in
  F.pn_s "DEB" ("Translation is done: " ^ (E.toStr name));
  let prms = L.params f
             |> Array.map get_var
             |> Array.to_list in
  let p = Procedure.mk_procedure name prms b in
  p
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

let translate_structures gs =
  let g_structures : G.t list =
    List.map (fun (snm, flds) ->
        let e_flds = List.mapi (fun i fld_ty ->
                         let n = string_of_int i in
                         let v = B.var n (Types.get_types fld_ty) in
                         v, B.T.NULL
                       ) flds in
        G.STRUCT ((snm, e_flds, None), B.dl)
      ) !structures in
  gs @ g_structures
;;

let translate_global g =
  let nm = get_var ~is_global:true g in
  let opn = L.num_operands g in
  let r =
    if opn > 0 then
      begin
        let tp = L.operand g 0 |> L.type_of in
        if is_array_type tp then
          begin
            let size = L.array_length tp in
            G.STATEMENT (B.decl nm size)
          end
        else
          G.STATEMENT (B.decl nm 1)
      end
    else
      G.STATEMENT (B.decl nm 1)
  in
  r
;;


let translate file =
  F.p_opt := ["PRO";"DEB"];
  vars := VR.empty;
  F.pn_s "PRO" ("Translating " ^ file);
  
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file file in
  let llm   = Llvm_bitreader.parse_bitcode llctx llmem in

  let gs1   = L.fold_left_globals (fun acc g -> acc @ [translate_global g]) [] llm in
  F.pn_s "DEB" "Globals are translated";

  Printf.printf "*** basic blocks/instructions ***\n" ;
  (* Llvm.iter_functions Print.print_fun llm ; *)
  Printf.printf "*** Translation ***\n" ;
  let fs    = L.fold_left_functions (fun acc f -> acc @[translate_llvm_fun f]) [] llm  in
  
  let gs2   = List.map (fun (func_name, params, body) ->
                  write_func func_name file params body) fs in
  let gs    = translate_structures (gs1@gs2) in
  (* List.iter (fun g -> Global.pprint g) gs; *)
  gs

 *)
