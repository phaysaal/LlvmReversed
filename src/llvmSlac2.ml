module L = Llvm
module VK = L.ValueKind
module O = L.Opcode
module I = L.Icmp
module B = Block
module V = Base
         
exception Err of string
exception OPC of string
               (*
type t =
    | SKIP
    (* | ASSERT of Formula.t * t * Locs.t *)
    | ASSIGN of string (* Exp.t * Term.t * t * Locs.t *)
    | IF of string * t * t (* BExp.t * t * t * t * Locs.t *)
    | WHILE of string (* BExp.t * BExp.t list * t * Formula.t * t * Locs.t *)
    | PROCCALL of string (* Term.t * Term.t list * int * t * Locs.t *)
    | CONS of string (* Exp.t * (Exp.t * Term.t) list * t * Locs.t  (** Name, fields, rest *) *)
    | MALLOC of string (* Exp.t * Exp.t * t * Locs.t (** Name, Length, rest *) *)
    | SARRAY of string (* Exp.t * Term.t * (Exp.t * Term.t) list * t * Locs.t (** Name, Length, rest *) *)
    | MUTATION of string (* Term.t * Field.t * Term.t * t * Locs.t *)
    | LOOKUP of string (* Exp.t * Term.t * Field.t * t * Locs.t *)
    | DISPOSE of string (* Term.t * t * Locs.t *)
    | MAPS of string (* Exp.t * Exp.t * t * Locs.t *)
    | PARALLEL of string (* t * t * t * Locs.t *)
    | BLOCK of string (* t * t * Locs.t *)
    | DECL of string (* Exp.t * Exp.t list * init * t * Locs.t *)
    | RETURN of string (* Term.t * t * Locs.t *)
    | LABEL of string (* string * string list * t * Locs.t *)
    | BREAK of string (* t * Locs.t *)
    | CONTINUE of string (* t * Locs.t *)
    | FAIL
  ;;
                *)
             
  
type while_t =
  | WBLK of while_t list
  | WINST of L.llvalue
  | WPROG of B.t
  | WWHILE of V.BExp.t * while_t
  | WIF of L.llvalue * while_t * while_t

let pf = Printf.printf ;;

let get_exp_from_llvalue lli =
  let v =
    if L.is_constant lli then
      let r =
        match L.int64_of_const lli with
          Some s -> (Int64.to_int s)
        | None -> raise (Err "Not an integer") in 
      B.const r
    else
      B.var (L.value_name lli)
  in
  v
;;

let get_exp_from_ref lli =
  let opcode = L.instr_opcode lli in
  match opcode with
    O.Load ->
     let opc = L.operand lli 0 in
     get_exp_from_llvalue opc
  | _ ->
     raise (Err "Other opcode in get_exp_from_ref")
;;

let get_bexp_from_comp lli =
  let opcode = L.instr_opcode lli in
  match opcode with
    O.ICmp ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let e_arg1 = get_exp_from_ref arg1 in
     let e_arg2 = get_exp_from_ref arg2 in
     let pred = L.icmp_predicate lli in
     pf "ICmp:\narg1:%s\narg2:%s\n" (Base.Exp.fstr () e_arg1) (Base.Exp.fstr () e_arg2);
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
  | _ ->
     raise (Err "Not supported opcode in bexp")

let is_pointer lli = L.classify_type (L.type_of lli) = L.TypeKind.Pointer ;;

let is_cast lli = (try L.instr_opcode lli with _ -> raise (OPC "is_cast")) = O.BitCast ;;
let get_casted_from lli = L.operand lli 0 ;;
  
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
  let fname = L.value_name op2 in
  fname
;;

let is_array_element lli =
  let opcode = (try L.instr_opcode lli with _ -> raise (OPC "is_indorect")) in
  opcode = O.GetElementPtr
;;

let mk_assign_maybe lli o =
  let vname = L.value_name lli in
  let t_operand i = let opc = L.operand lli i in
                    let opv = get_exp_from_llvalue opc in
                    let t_opc = B._T opv in
                    t_opc in
  if vname = "" then
    B.SKIP
  else
    let v_vname = B.var vname in
    match o with
      O.BitCast
    | O.Trunc ->
       let t_opc = t_operand 0 in
       B.assign v_vname t_opc
    | O.Add ->
       (* pf "Add %s %d\n" (L.string_of_llvalue lli) (L.num_operands lli); *)
       let opr1 = t_operand 0 in
       let opr2 = t_operand 1 in
       let t = B.T.op opr1 opr2 V.Op.ADD in
       B.assign v_vname t 
    | O.Or
    | _ -> B.SKIP

let to_while_t_inst lli =
  let st l = L.string_of_llvalue l in
  let opcode = try L.instr_opcode lli with _ -> pf "OPCode - 1"; raise (OPC "OpCode -1") in
  let str = st lli in
  (* Printf.printf "inst %s [%s]\n" str (L.type_of lli |> L.string_of_lltype); *)
  
  match opcode with
  | O.Invalid -> B.SKIP
  | O.Ret ->
     let v = L.operand lli 0 in
     B.return (B._T @@ get_exp_from_llvalue v)
  | O.Br ->
     raise (Err "Br - Not implemented yet")
  | O.Switch ->
     raise (Err "Switch - Not understood yet")
  | O.IndirectBr ->
     raise (Err "Switch - Not understood yet")
  (*** |	Invoke
    |	Invalid2
    |	Unreachable
    |	FAdd	(*	
Standard Binary Operat  ors
   *) *)
  |	O.Add ->
     mk_assign_maybe lli O.Add
    (* |	Sub
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
    |	And
     *)
  |	Or ->
     mk_assign_maybe lli O.Or
     (* pf "Or %s\n" (L.value_name lli);
     B.SKIP *)
  (*  |	Xor -> raise (Err "Br - Not implemented yet") *)
  |	O.Alloca ->
     let op = L.operand lli 0 in
     if L.is_constant op then
       let size = 
         begin
           match L.int64_of_const op with
             Some s -> (Int64.to_int s)
           | None ->
              begin
                pf "\n%s\n" (L.string_of_llvalue lli);
                raise (Err "Not an integer")
              end
         end in                         
       let x = L.value_name lli in
       (* Todo: Typing should be handled *)
       B.decl (B.var x) size
     else
       begin
         pf "\n%s\n" (L.string_of_llvalue lli);
         raise (Err "Not an integer")
       end
  |	Load ->
     (* Printf.printf "LOAD -> %d\n" (L.num_operands lli); *)
     (* let op1 = L.operand lli 0 |> L.value_name (* |> B.var *) in *)
     (* let op2 = L.operand lli 1 in *)
     
     (* Printf.printf "%s ====== %s [%s]\n " (L.string_of_llvalue lli) op1 (L.value_name lli); *)
     (* let op2' = op2 |> L.value_name |> B.var in *)
     (* B.lookup op2' (B._T op1) "*" *) B.SKIP
  |	Store ->
     let src = L.operand lli 0 in
     let dest = L.operand lli 1 in
     let dest_v = dest |> L.value_name |> B.var in
     if is_indirect dest then
       begin
         (* if is_const src then *)
         let destv' = L.operand dest 0 |> L.value_name |> B.var in
         B.mutation (B._T destv') "*" (B._T (get_exp_from_llvalue src))
       end
     else if is_array_element dest then
       begin
         (* pf "GetElementPtr %d\n" (L.num_operands dest); *)
         let arr = L.operand dest 0 in
         (* let op1 = L.operand dest 1 in *)
         let op2 = L.operand dest 2 in
         let op3 = L.operand op2  0 in
         let op4 = L.operand op3 0 in
         let index = get_exp_from_llvalue op4 in
         (* pf "%s | %s | %s ||\n" (L.value_name op0 ) (L.string_of_llvalue op1) (L.string_of_llvalue op2); *)
         let idx = B.add (L.value_name arr |> B.var) index in
         B.mutation (B._T idx) "*" (B._T (get_exp_from_llvalue src))
       end
     else if is_const src then
       begin
         B.assign dest_v (B._T (get_exp_from_llvalue src))
       end
     else if is_fcall src then
       begin
         try
           let called_fun = get_call src in
           let fname = get_called_fname called_fun in
           if fname = "malloc" then
             begin
               let op1 = L.operand called_fun 0 in
               let size = get_exp_from_llvalue op1 in
               B.malloc dest_v size
             end
           else
             B.SKIP
         with
           e ->
           pf "exception at fcall";
           pf "%s" str;
           raise e
       end
     else if is_pointer src then
       begin
         pf "Pointer it is %s \n" ("");
         B.SKIP
       end
     else
       begin
         pf "OTher kinds %s\n" str;
         pf "src %s\n" (L.string_of_llvalue src);
         pf "dest %s\n" (L.string_of_llvalue dest);
         B.SKIP
       end
  |	GetElementPtr ->
     B.SKIP
  |	Trunc ->
     (* pf "Trunc '%s' in %s: %s\n" (L.value_name lli) str (L.operand lli 0 |> L.value_name); *)
     mk_assign_maybe lli O.Trunc
    (* (*	
Cast Operators      
   *)
    |	ZExt *)
  |	SExt ->
     B.SKIP
(*    |	FPToUI
    |	FPToSI
    |	UIToFP
    |	SIToFP
    |	FPTrunc
    |	FPExt
    |	PtrToInt
    |	IntToPtr *)
  |	BitCast ->
     (* pf "Skipped BitCast '%s' in %s: %s\n" (L.value_name lli) str (L.operand lli 0 |> L.value_name); *)
     mk_assign_maybe lli O.BitCast
       (* B.SKIP *)
  |	O.ICmp ->
     pf "IComp %s\n" (L.value_name lli);
     (* <result> = icmp <cond> <ty> <op1>, <op2>   ; yields i1 or <N x i1>:result *)
     (* begin
       let nop  = L.num_operands lli in
       if nop = 2 then
         let op1 = L.operand lli 0 |> L.value_name in
         let op2 = L.operand lli 1 |> L.value_name in
         Printf.printf "COMP %s ... %s .. %s\n" op1 op2 (L.operand lli 0 |> L.string_of_llvalue); 
         let op x = B.cond_assign (B.var @@ L.value_name lli) (B.var op1) (B.op x) (B.var op2) in
         let icmp = L.icmp_predicate lli in
         match icmp with
         | Some iop ->
            begin
              match iop with
              |	I.Eq	(*	Equal *) ->
                 op "="
              |	I.Ne	(*	Not equal  *) ->
                 op "/="
              |	I.Ugt	(*	Unsigned greater than *) ->
                 op ">"
              |	I.Uge	(*	Unsigned greater or equal *) ->
                 op ">="
              |	I.Ult	(*	Unsigned less than *) ->
                 op "<"
              |	I.Ule	(*	Unsigned less or equal *) ->
                 op "<="
              |	I.Sgt	(*	Signed greater than *) ->
                 op ">"
              |	I.Sge	(*	Signed greater or equal *) ->
                 op ">="
              |	I.Slt	(*	Signed less than *) ->
                 op "<"
              |	I.Sle	(*	Signed less or equal *) ->
                 op "<="
            end
         | None -> raise (Err ("Unsupported inst " ^ str))
       else
         raise (Err ("No two operands of " ^ str))
     end
      *)
     B.SKIP
  (*	
Other    Operators         
   *)
  (* |	FCmp
    |	PHI *)
  |	Call ->
     (* let argn = L.num_operands lli in
     let op2 = L.operand lli (argn-1) in
     let fname = L.value_name op2 in
     let ret = L.value_name lli |> B.var in *)
     (* pf "CALL ==> %s %d | %s | %s\n" (L.value_name lli) (L.num_operands lli) (L.string_of_llvalue op1) (L.value_name op2);
      *)
     (* if fname = "malloc" then
       (* let op1 = L.operand lli 0 in
       let arg = get_exp_from_llvalue op1 in
       B.malloc ret arg *) B.SKIP
     else *)
     pf "Call %s\n" (L.value_name lli);
     B.SKIP
  (* |	Select
    |	UserOp1
    |	UserOp2
    |	VAArg
    |	ExtractElement
    |	InsertElement
    |	ShuffleVector
    |	ExtractValue
    |	InsertValue
    |	Fence
    |	AtomicCmpXchg
    |	AtomicRMW
    |	Resume
    |	LandingPad->
     raise (Err str) *)
  | _ ->
     raise (Err ("\nUnexpected ValueKind " ^ str ^ "\n"))
;;
         
let to_while_t_blk blk =
  let ps = Llvm.fold_left_instrs
    (fun acc lli ->
      acc @ [WPROG (to_while_t_inst lli)]
    (* acc @ [WINST lli] *)
    ) []
    blk in
  WBLK ps
;;
            
let rec to_while_t_blks i j blk_arr acc =
   if i <= j then
     begin
       let llbb = Array.get blk_arr i in
       
       let p = to_while_t_blk llbb in
       to_while_t_blks (i+1) j blk_arr (acc@[p])
    end
  else
    WBLK acc
  
;;

            
let get_blk_name blk = L.value_name (L.value_of_block (blk));;
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
                 let lbl_name = get_blk_name blk in
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
                 let tlbl_name = get_blk_name tblk in
                 let flbl_name = get_blk_name fblk in
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
  if i>=n then
    None
  else
    let blk = Array.get blk_arr i in
    let _L1' = get_blk_name blk in
    if _L1'=_L1 then
      Some i
    else
      search_block _L1 (i+1) n blk_arr
;;

let rec space i = if i = 0 then "" else "    " ^ space (i-1);;

let print_blk ind llbb =
  Printf.printf "%sBLK: %s\n" (space ind) (get_blk_name llbb) ;
  Llvm.iter_instrs
    (fun lli ->
      Printf.printf "%s%s\n" (space ind) (Llvm.string_of_llvalue lli)
    )
    llbb;
  ()
;;

let rec print_blks ind i j blk_arr =
  if i <= j then
    begin
      let llbb = Array.get blk_arr i in
      print_blk ind llbb;
      print_blks ind (i+1) j blk_arr
    end
  else
    ()
  
let rec try_parse_while ind k i n blk_arr =
  (* Printf.printf "Pattern Try with %d %d\n" i n; *)
  if i+2 >= n then
    begin
      to_while_t_blks i n blk_arr []
    end
  else
    let status, p = 
      begin
        (* actual P1 is blk_arr(k to i) *)
        let p1_blk = Array.get blk_arr i in
        match parse_unconditional p1_blk with
          Some (inst1, _L1) ->
           begin
             let p2_blk = Array.get blk_arr (i+1) in
             let _L1' = get_blk_name p2_blk in
             if _L1 = _L1' then
               let _P2_A_L2_L3 = parse_conditional p2_blk in
               match _P2_A_L2_L3 with
                 Some (_P2, inst2, _A, _L2, _L3) ->

                  pf "WHILE CONDITION \n%s\n....................\n" (L.string_of_llvalue _A);
                  pf "%d\n0: %s\n1: %s \n" (L.num_operands _A) (L.operand _A 0 |> L.string_of_llvalue) (L.operand _A 1  |> L.string_of_llvalue);
                  let _A = get_bexp_from_comp _A in
                  let p3_blk = Array.get blk_arr (i+2) in
                  let _L2' = get_blk_name p3_blk in
                  if _L2=_L2' then
                    match search_label _L1 (i+2) n blk_arr with
                      Some (j, inst3) ->
                       (* Actual _P3 is blk_arr(i+2 to j) and _P3 *)
                       let p4_blk = Array.get blk_arr (j+1) in
                       let _L3' = get_blk_name p4_blk in
                       if _L3=_L3' then
                         begin
                           Printf.printf "k:%d, i:%d, j:%d, n:%d\n" k i j n;
                           L.delete_instruction inst1;
                           L.delete_instruction inst2;
                           L.delete_instruction inst3;
                           print_blks ind k (i+1) blk_arr; (* P1 *)
                           let _WP1 = to_while_t_blks k (i+1) blk_arr [] in
                           let _WP21 =
                             if j-i> 2 then
                               try_parse_while (ind+1) (i+2) (i+2) j blk_arr
                             else
                               to_while_t_blks (i+2) j blk_arr []
                           in
                           let _WP22 = to_while_t_blk _P2 in
                           let _WP2 = WBLK [_WP21;_WP22] in
                           let _WP3 = try_parse_while ind (j+1) (j+1) n blk_arr in
                             (* to_while_t_blks (j+1) n blk_arr [] in *)
                           let _WP = WBLK [_WP1; WWHILE (_A, _WP2); _WP3] in
                           Printf.printf "\n%swhile(%s){\n" (space ind) (V.BExp.fstr () _A);
                           print_blks (ind+1) (i+2) j blk_arr;
                           print_blk (ind+1) _P2;
                           Printf.printf "%s}\n\n" (space ind);
                           print_blks ind (j+1) (n-1) blk_arr;
                           Printf.printf "\n";
                           
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
        Printf.printf "Pattern Matched\n";
        p
      end
    else
      try_parse_while ind k (i+1) n blk_arr

and try_parse_if ind k i n blk_arr =
  Printf.printf "Patter Try with %d %d\n" i n;
  if i+2 >= n then
    begin
      Printf.printf "No Pattern Match\n";
      to_while_t_blks k n blk_arr []
    end
  else
    let status, p =
      begin
        let p0_blk = Array.get blk_arr i in
        let _P0_A_L1_L2 = parse_conditional p0_blk in
        match _P0_A_L1_L2 with
          Some (_, inst1, _A, _L1, _L2) -> (* br _A, label _L1, label _L2 *)
           let p1_blk = Array.get blk_arr (i+1) in (* P0 is blk_arr(k to i) *)
           let _L1' = get_blk_name p1_blk in
           if _L1=_L1' then
             let somej = search_block _L2 i n blk_arr in
             match somej with
               Some j ->
               begin
                 let j' = j - 1 in
                 let p2_blk = Array.get blk_arr j' in
                 let p2lbl = parse_unconditional p2_blk in
                 match p2lbl with
                   Some (inst2, _L4) ->
                    begin
                      let somek = search_block _L4 j n blk_arr in
                      match somek with
                        Some l ->
                         begin
                           let l' = l - 1 in
                           let p3_blk = Array.get blk_arr l' in
                           let p3lbl = parse_unconditional p3_blk in
                           match p3lbl with
                             Some (inst3, _L4') ->
                              if _L4' = _L4 then
                                begin
                                  L.delete_instruction inst1;
                                  L.delete_instruction inst2;
                                  L.delete_instruction inst3;
                                  let _WP0 = to_while_t_blks k i blk_arr [] in
                                  let _WP1 = try_parse_while ind (i+1) (i+1) j' blk_arr in
                                  let _WP2 = try_parse_while ind j j l' blk_arr in
                                  let _WP3 = try_parse_while ind l l n blk_arr in
                                  true, WBLK [_WP0; WIF (_A, _WP1, _WP2); _WP3]
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
        Printf.printf "Pattern Matched\n";
        p
      end
    else
      try_parse_if ind k (i+1) n blk_arr
     
;;

let print_llvm_fun fbody =
  pf "@@@@ FUNCTION: %s @@@@\n" (L.value_name fbody);
  let blk_arr = Llvm.basic_blocks fbody in
  (* Printf.printf "Number of Blocks: %d\n" (Array.length blk_arr);
   Llvm.iter_blocks (fun blk ->
      Printf.printf "--------BLOCK--------\n";
      Printf.printf "%s\n" (Llvm.value_name (Llvm.value_of_block (blk)));
    ) fbody;
   *)
  try_parse_while 0 0 0 (Array.length blk_arr - 1) blk_arr
;;

let rec is_array llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> false
  | Llvm.TypeKind.Function -> false
  | Llvm.TypeKind.Array    -> true
  | Llvm.TypeKind.Pointer  -> pf "A Pointer "; is_array (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> pf "A Vector "; is_array (Llvm.element_type llty)
  | _                      -> false
  

let rec print_type llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> Printf.printf "  integer\n"
  | Llvm.TypeKind.Function -> Printf.printf "  function\n"
  | Llvm.TypeKind.Array    -> Printf.printf "  array of" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Pointer  -> Printf.printf "  pointer to" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> Printf.printf "  vector of" ; print_type (Llvm.element_type llty)
  | _                      -> Printf.printf "  other type\n"

let print_val lv =
  let v = L.value_name lv |> B.var in
  (* let op = L.operand lv 0 in *)
  let ty = L.type_of lv in
  let p =
    if is_array ty then
      let len = L.array_length (L.element_type ty) in
      let s = B.decl v len in
      s
    else
      begin
        pf "____________";
        B.SKIP
      end
  in
  B.pprint 0 p;
  (*
  Printf.printf "Value\n" ;
  Printf.printf "  name %s\n" (Llvm.value_name lv) ;
  
  Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
  print_type llty ; *)
  ()

let print_fun lv =
  Llvm.iter_blocks
    (fun llbb ->
      Printf.printf "  bb: %s\n" (Llvm.value_name (Llvm.value_of_block (llbb))) ;
      Llvm.iter_instrs
        (fun lli ->
          Printf.printf "    instr: %s\n" (Llvm.string_of_llvalue lli)
        )
        llbb
    )
    lv

 
  
let _ =
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  (*Llvm.dump_module llm ;*)

  (* Printf.printf "*** lookup_function ***\n" ;
  let opt_lv = Llvm.lookup_function "main" llm in
  begin
  match opt_lv with
  | Some lv -> print_val lv
  | None    -> Printf.printf "'main' function not found\n"
  end ;

  Printf.printf "*** iter_functions ***\n" ;
  Llvm.iter_functions print_val llm ;

  Printf.printf "*** fold_left_functions ***\n" ;
  let count =
    Llvm.fold_left_functions
      (fun acc lv ->
        print_val lv ;
        acc + 1
      )
      0
      llm
  in
  Printf.printf "Functions count: %d\n" count ; *)

  Llvm.iter_globals print_val llm ;

  Printf.printf "*** basic blocks/instructions ***\n" ;
  Llvm.iter_functions print_fun llm ;
  Printf.printf "*** searching while pattern ***\n" ;
  let fs = (* Llvm.iter_functions *) L.fold_left_functions (fun acc f -> acc @[print_llvm_fun f]) [] llm  in
  Printf.printf "*** iter_funcs *** %d\n" (List.length fs);
  

  ()
