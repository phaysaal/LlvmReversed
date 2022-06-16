module L = Llvm
module VK = L.ValueKind
module TK = L.TypeKind
module O = L.Opcode
module I = L.Icmp
module B = Block
module V = Base
module G = Global
module F = Ftools
module FM = V.Formula
module E = V.Exp
module T = V.Term
module BX = V.BExp
module VR = Map.Make(String)
module P = Print
         
exception Err of string
exception OPC of string
exception PHI of L.llvalue
             
let func_dir = ref "";;
let current_func = ref (E.CONST 0);;

let phis : (L.llvalue * BX.t) list ref = ref [] ;;

type while_t =
  | WBLK of while_t list
  | WINST of L.llvalue
  | WPROG of B.t
  | WWHILE of BX.t * while_t
  | WIF of BX.t * while_t * while_t

type seg = int * int
         
type parse_data =
  PRS_LBL of string
| PRS_LBLs of (string * string)
| PRS_DLT of L.llvalue
| PRS_DLTs of L.llvalue list
| PRS_SEG of seg
| PRS_COND of L.llvalue

type interruption =
  BRK | BRK_THEN | BRK_ELSE | CONT | CONT_THEN | CONT_ELSE

type pkg = L.llbasicblock array * (E.attr list) VR.t * L.llvalue
exception Not_parsed
        
let pf = Printf.printf ;;
let pn = F.pn ;;
let stv = L.string_of_llvalue;;
let _C x = E.CONST x;;
let _T x = T.EXP x;;
let _TC x = _T (_C x);;
let break = "__BREAK__";;
let continue = "__CONTINUE__";;

let (++~) x y = B.join_at_last y x;;

let temps = ref [];;
let comps = ref [];;

let structures : (string * L.lltype list) list ref = ref [];;

let rec is_struct llt =
    match L.classify_type llt with
      TK.Struct ->
      Some llt
    | TK.Integer ->
       None
    | TK.Function ->
       None
    | TK.Array
      | TK.Pointer
      | TK.Vector
      ->
       is_struct @@ L.element_type llt
    | _ -> None
;;
    
let add_struct lli =
  
  let llt = L.type_of lli in

  match is_struct llt with
    None -> ()
  | Some llt ->
     let nm = L.struct_name llt in
     let flds = L.struct_element_types llt in
     let l_flds = Array.to_list flds in
     match nm with
       Some snm ->
        if List.exists (function (s,_) -> s=snm) !structures then
          ()
        else
          structures := !structures @ [(snm, l_flds)]
     | None ->
        ()
;;

let get_structures () =
  let module V = Map.Make (String) in
  let convert_fields flds =
    List.mapi (fun i fld ->
        let attr = Types.get_types fld in
        (E.VAR (string_of_int i, attr), T.NULL)) flds
  in
  let find_rec_fld _ = None in (** Need to implement for List, Tree, etc. data structure *)
  let structures = List.fold_left (fun acc (s, flds) ->
                       let flds' = convert_fields flds in
                       let rec_fld = find_rec_fld flds' in
                       let st = (s, flds', rec_fld) in
                       
                       V.add s st acc) V.empty !structures in
  structures
;;

let get_attr vars is_strict vn =
  try
    VR.find vn vars
  with
    Not_found ->
     if is_strict then
       raise Not_found
     else
       []
;;

let rec get_array_type tp =
  match L.classify_type tp with
    TK.Array -> Some tp
  | TK.Pointer
  | TK.Vector
  | TK.Struct ->  get_array_type @@ L.element_type tp
  | _ -> None
;;

let rec get_constant lli =
  match L.int64_of_const lli with
    Some s ->
     Some (Int64.to_int s)
  | None ->
     if L.num_operands lli = 0 then
       None
     else
       get_constant @@ L.operand lli 0
;;

let is_a_struct = List.exists (function E.STRUCT _ -> true | _ -> false)
;;

let rec remove_first_ptr = function
    [] -> []
  | E.PTR::xs -> xs
  | x::xs -> x::remove_first_ptr xs;;

let is_a_func = function
    E.FUNC _ -> true
  | _ -> false

let is_declared v =
  List.mem v !B.declared
;;

let fresh = ref 0;;

let fresh_var attr =
  let v = string_of_int !fresh in
  let r = B.var ("FR_" ^ v) attr in
  fresh := !fresh + 1;
  r
;;

let write_func fname filename params body =
  if !func_dir <> "" then
    begin
      let sname = E.fstr () fname in
      let file = F.make_path !func_dir filename sname in
      let out = open_out file in
      let b = (fname, params, body) in
      let pr = G.PROC (b, V.Formula.empty, V.Formula.trueempty, [], B.dl) in

      Marshal.to_channel out pr [];
      close_out out;
      let fn = Block.__N fname in
      let ffile = G.FFILE (file, fn) in
      ffile
    end
  else
    begin
      raise (Err "Func dir is blank\n")
    end
;;


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
     Printf.printf "WWHIL%swhile(%s)\n" (P.space ind) (BX.fstr () b);
     print_while_t ind t
  | WIF (b, t1, t2) ->
     Printf.printf "WIF %sif(%s){\n" (P.space ind) (BX.fstr () b);
     print_while_t (ind + 1) t1;
     Printf.printf "%s} else {\n" (P.space ind);
     print_while_t (ind + 1) t2;
     Printf.printf "%s}" (P.space ind);
;;

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

let is_ptr_element lli =
  let vt = L.classify_value lli in
  match vt with
    VK.GlobalVariable ->
     false
  | VK.ConstantExpr ->
     true
  | VK.Instruction opcode ->
     (* let opcode = (try L.instr_opcode lli with _ -> raise (OPC "@is_ptr_element")) in *)
     opcode = O.GetElementPtr
  | _ -> raise (OPC "@is_ptr_element")
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

let rec get_exp_from_llvalue ?(addr=false) vars lli =
  let ps, v =
    let vk = L.classify_value lli in
    match vk with
      VK.ConstantInt
      ->                       
      begin                                              F.dbgf "VAL" "%s is Constant_int" (stv lli);
        let r =
          match L.int64_of_const lli with
            Some s ->
             (Int64.to_int s)
          | None ->
             begin
               F.dbgf "VAL" "Error at get_exp_from_llvalue (int)";
               raise (Err "Not an integer (get_exp_from_llvalue)")
             end
        in
        [], B.const r
      end
    | VK.ConstantExpr ->
      begin                                              F.dbgf "VAL" "%s is Constant_exp" (stv lli);
        get_const_expr vars lli
      end
    | VK.NullValue | ConstantPointerNull ->
      begin                                              F.dbgf "VAL" "%s is Null" (stv lli);
        [], B.const 0
      end
    | VK.ConstantFP ->
       begin
         let flv = L.float_of_const lli in
         match flv with
           Some f ->
           [], B.constf f
         | None ->
            begin
              F.dbgf "VAL" "Error at get_exp_from_llvalue (double)";
              raise (Err "Not an integer (get_exp_from_llvalue)")
            end
       end
    | _ ->
      begin                                              F.dbgf "VAL" "%s is %s" (stv lli) (P.print_value_kind @@ L.classify_value lli);
        let s = L.value_name lli in
        if s = "" then
          begin                                          F.dbgf "VAL" "%s is ''" s;
            let r = get_exp_from_ref vars lli in
            r
          end
        else
          begin                                          F.dbgf "VAL" "%s is not ''" s;
            let _, r = get_var vars ~addr:addr lli in
            [], r
          end
      end
  in
  ps, v

and get_exp_from_ref vars lli =
  let opcode = try L.instr_opcode lli with e -> Printf.printf "%s\n" (L.string_of_llvalue lli); raise e in
  match opcode with
    O.Load
  | O.BitCast
    |	ExtractValue
      | ZExt ->
     begin
       F.dbgf "EXP" "lli: %s" (stv lli);
       let opc = L.operand lli 0 in
       F.dbgf "EXP" "opc: %s" (stv opc);
       try
         get_exp_from_llvalue vars opc
       with
         e ->
         Printf.printf "get_exp_from_ref %s\n" (L.string_of_llvalue lli);
         raise e
     end
  | LandingPad ->
     [], B.const 0
  | Alloca ->
     let attr = [] in
     let nv = fresh_var attr in
     temps := [B.decl nv 1];
     [], nv
  | Call ->
     Printf.printf "!!!!\nWARNING: Unsupported %s\n" (L.string_of_llvalue lli);
     let nv = fresh_var [] in
     [], nv
  | PHI ->
     raise (PHI lli)
  | _ ->
     Printf.printf "get_exp_from_ref %s\n" (L.string_of_llvalue lli);
     raise (Err "Other opcode in get_exp_from_ref")

and get_ptr_element vars lli =
  if L.num_operands lli < 0 then
    raise (Err "Number of operand is 0")
  else
    begin      
      let arr = L.operand lli 0 in
      let _, e_arr = get_var vars arr in
      F.dbgf "GEP" "arr: %s {(%a)}" (stv arr) E.fstr e_arr;
      (* let arr0 = L.operand arr 0 in *)
      let ty = L.type_of arr in
      let tp = ty |> L.classify_type in
      (* F.pn (stv lli);
      F.pn (stv arr);
      P.print_type (L.type_of arr);
      F.pn (stv arr0);
      P.print_type ty;
       *)
      if is_array ty then
        begin
          F.dbgf "GEP" "Array in GEP";
         let idx = L.operand lli 2 in
         let ps, e_idx = get_exp_from_llvalue vars idx in
         let s_fld = "*" in
         let ptr = B.add e_arr e_idx |> B._T in
         ps, (ptr, s_fld)
        end
      else
        match tp with
        | L.TypeKind.Array ->
           F.dbgf "GEP" "tp Array";
           let idx = L.operand lli 2 in
           let ps, e_idx = get_exp_from_llvalue vars idx in
           let s_fld = "*" in
           let ptr = B.add e_arr e_idx |> B._T in
           ps, (ptr, s_fld)
        | Pointer ->
           if is_struct ty <> None then
             begin
               F.dbgf "GEP" "tp Pointer & Struct";
               let idx = L.operand lli 2 in
               let ps, e_idx = get_exp_from_llvalue vars idx in
               let s_fld = E.fstr () e_idx in
               let ptr = B._T e_arr in
               ps, (ptr, s_fld)
             end
           else
             begin
               F.dbgf "GEP" "tp Pointer & not Struct";
               let idx = L.operand lli 1 in
               let ps, e_idx = get_exp_from_llvalue vars idx in
               let s_fld = "*" in
               let ptr = B.add e_arr e_idx |> B._T in
               ps, (ptr, s_fld)
               end
        | _ ->
           F.dbgf "GEP" "tp Else";
           let idx = L.operand lli 1 in
           let ps, e_idx = get_exp_from_llvalue vars idx in
           let ptr = B.add e_arr e_idx |> B._T in
           if L.num_operands lli < 3 then
             ps, (ptr, "*")
           else
             begin
               
               let fld = L.operand lli 2 in
               let ps, e_fld = get_exp_from_llvalue vars fld in
               let s_fld = V.Exp.fstr () e_fld in
               ps, (ptr, s_fld)
             end
    end
  
and get_const_expr vars lli =
  let opn = L.num_operands lli in
  if opn = 1 then
    let lli0 = L.operand lli 0 in
    get_exp_from_llvalue vars lli0
  else if opn = 3 then
    begin
      let lli0 = L.operand lli 0 in
      let lli0ty = L.type_of lli0 in
      if is_array lli0ty then
        begin
          let ps, (pt, fld) = get_ptr_element vars lli in
          let ept = T.toExp pt in
          let attr = E.get_attributes ept in
          let attr' = List.filter (function E.GLOBAL | E.ARRAY _ | E.PTR -> false | _ -> true) attr in
          let frv = fresh_var attr' in
          let p1 = B.decl frv 1 in
          let p2 = if fld = "-" then B.assign frv pt else B.lookup frv pt fld in
          let p = B.join_at_last p2 p1 in
          p::ps, frv
        end
      else
        raise (Err "")
    end
  else
    raise (Err ("GetConstExpr| Unsupported Number of Operands " ^ (string_of_int opn)))

and get_var vars ?(is_global=false) ?(is_param=false) ?(is_local=false) ?(addr=false) ?(alloca=false) lli =
  let vn'' = L.value_name lli in
  let vn' = if vn'' = "" then
              get_exp_from_ref vars lli |> snd |> E.toStr
            else
              vn'' in
  let vn = if L.classify_value lli = VK.GlobalVariable then "@" ^ vn' else vn' in            F.dbgf "VAR" "\nGet_Var(%s)|vn: %s" vn vn;
  let vars', v=
    if VR.mem vn vars && not is_param && not is_local then
      begin                                                                                  F.dbgf "VAR" "Get_Var(%s)|lli: %s" vn (stv lli);
        let tp = L.type_of lli in                                                            F.dbgf "VAR" "Get_Var(%s)|type: %s" vn (L.string_of_lltype tp);
        let attr_now = Types.get_types tp in 
        let attr_org = get_attr vars true vn in
        if addr && not(List.exists is_a_func attr_now) && List.mem E.PTR attr_now && not (List.mem E.PTR attr_org) then
          vars, E.ADDR (B.var vn attr_org) 
        else
          vars, B.var vn attr_org 
      end
    else
      begin                                                                                  F.dbgf "VAR" "Get_Var(%s)|lli: %s" vn (stv lli);
        let tp = L.type_of lli in                                                            F.dbgf "VAR" "Get_Var(%s)|type: %s" vn (L.string_of_lltype tp);
                                                                                             F.dbgf "VAR" "Get_Var(%s)|type: %s" vn (P.string_of_type tp);
        let attr = Types.get_types tp in                                                     F.dbgf "VAR" "Get_Var(%s)|type: %s | %a" vn (P.string_of_type tp) (F.fstrL (fun () a -> a) ",") (List.map E.print_attr attr);
        let attr' = if is_global then E.GLOBAL::attr else if is_param then E.PARAM::attr else attr in
        let attr'' =
          if is_param || (not alloca && is_a_struct attr') then (** 6/8: || is_a_struct attr' *)
            attr'
          else
            remove_first_ptr attr' in
        let v = B.var vn attr'' in                                                           F.dbgf "VAR" "|Get_Var(%s)|attr'': %a" vn (F.fstrL (fun () a -> a) ",") (List.map E.print_attr attr'');
        let vars' = VR.add vn attr'' vars in
        vars', v
      end in                                                                                 F.dbgf "VAR" "Get_Var(%s)|var: %a" vn E.fstr v;
  vars', v
;;

let details vars lli =
  F.dbgf "DET" "++++++++++++";
  F.dbgf "DET" "inst: %s" (stv lli);
  F.dbgf "DET" "|op|: %d" (L.num_operands lli);
  let tp = L.type_of lli in
  F.pf_s "DET" P.print_type tp;
  
  F.dbgf "DET" "type: %s" (L.string_of_lltype tp);
  F.dbgf "DET" "typ_class: %s" (P.string_of_type tp);
  begin
    match get_array_type tp with
    Some tp ->
    F.dbgf "DET" "Array type of length %d" (L.array_length tp)
  | None ->
     F.dbgf "DET" "Not an Array type"
  end;
  begin
    match get_constant lli with
      Some s ->
       F.dbgf "DET" "A constant: %d" s
    | None ->
       F.dbgf "DET" "Not a constant"
  end;
  let _, r = get_exp_from_llvalue vars lli in
  F.dbgf "DET" "value: %a" E.fstr r;
  F.dbgf "DET" "-----------";
;;

let mk_i_pred e_arg1 e_arg2 = function
    L.Icmp.Slt
  | L.Icmp.Ult ->
    BX.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2)
  |  L.Icmp.Sgt
     |  L.Icmp.Ugt ->
      BX.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1)
  |  L.Icmp.Sle
     |  L.Icmp.Ule ->
      BX.OP (
          BX.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2), V.Op.OR,
          BX.UNIT (B._T e_arg1, V.Op.EQ, B._T e_arg2))
  |  L.Icmp.Sge
     |  L.Icmp.Uge ->
      BX.OP (
          BX.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1), V.Op.OR,
          BX.UNIT (B._T e_arg2, V.Op.EQ, B._T e_arg1))
  |  L.Icmp.Eq ->
      BX.UNIT (B._T e_arg1, V.Op.EQ, B._T e_arg2)
  |  L.Icmp.Ne ->
      BX.UNIT (B._T e_arg1, V.Op.NE, B._T e_arg2)
;;

let mk_f_pred e_arg1 e_arg2 = function
  (*  L.Fcmp.Slt
  |  L.Icmp.Ult ->
      BX.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2)
  |  L.Icmp.Sgt
     |  L.Icmp.Ugt ->
      BX.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1)
  |  L.Icmp.Sle
     |  L.Icmp.Ule ->
      BX.OP (
          BX.UNIT (B._T e_arg1, V.Op.LE, B._T e_arg2), V.Op.OR,
          BX.UNIT (B._T e_arg1, V.Op.EQ, B._T e_arg2))
  |  L.Icmp.Sge
     |  L.Icmp.Uge ->
      BX.OP (
          BX.UNIT (B._T e_arg2, V.Op.LE, B._T e_arg1), V.Op.OR,
          BX.UNIT (B._T e_arg2, V.Op.EQ, B._T e_arg1)) *)
  |  L.Fcmp.Oeq ->
      BX.UNIT (B._T e_arg1, V.Op.EQ, B._T e_arg2)
  (* |  L.Icmp.Ne ->
      BX.UNIT (B._T e_arg2, V.Op.NE, B._T e_arg1) *)
  | _ -> raise (Err "Not compatible yet.")              
;;


let rec get_bexp_from_comp vars lli =
  let opcode = L.instr_opcode lli in
  match opcode with
    O.ICmp | FCmp ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     F.dbgf "BEXP" "arg1: %s" (stv arg1);
     F.dbgf "BEXP" "arg2: %s" (stv arg2);
     let ps1, e_arg1 = try get_exp_from_llvalue vars arg1 with e -> Printf.printf "arg1\n"; raise e in
     let ps2, e_arg2 = try get_exp_from_llvalue vars arg2 with e -> Printf.printf "arg2\n"; raise e in
     let pred = L.icmp_predicate lli in
     begin
       let res = 
         match pred with
           Some op -> mk_i_pred e_arg1 e_arg2 op
         | None ->
            begin
              let pred = L.fcmp_predicate lli in
              let res = 
                match pred with
                  Some op ->
                   mk_f_pred e_arg1 e_arg2 op
                | None -> raise (Err "Not compatible yet.")
              in
              res
            end
       in
       ps1@ps2, res
     end
  | Trunc ->
     get_bexp_from_comp vars @@ L.operand lli 0
  | Load ->
     let op1 = L.operand lli 0 in
     let ps, v = get_exp_from_llvalue vars op1 in
     ps, BX.UNIT (B._T v, V.Op.NE, B._T (B.const 0))
  | Or ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let ps1, e_arg1 = try get_bexp_from_comp vars arg1 with e -> Printf.printf "arg1\n"; raise e in
     let ps2, e_arg2 = try get_bexp_from_comp vars arg2 with e -> Printf.printf "arg2\n"; raise e in
     ps1@ps2, BX.OP (e_arg1, V.Op.OR, e_arg2)
  | And ->
     let arg1 = L.operand lli 0 in
     let arg2 = L.operand lli 1 in
     let ps1, e_arg1 = try get_bexp_from_comp vars arg1 with e -> Printf.printf "arg1\n"; raise e in
     let ps2, e_arg2 = try get_bexp_from_comp vars arg2 with e -> Printf.printf "arg2\n"; raise e in
     ps1@ps2, BX.OP (e_arg1, V.Op.AND, e_arg2)
  | ZExt ->
     get_bexp_from_comp vars @@ L.operand lli 0
  | _ ->
     pn "BExp Exception in";
     pn @@ stv lli;
     raise (Err "Not supported opcode in bexp")
;;

let get_bexp_from_llvalue vars lli =
  match L.classify_value lli with
    VK.Instruction _ ->
     get_bexp_from_comp vars lli
  | _ ->
     let ps, x = get_exp_from_llvalue vars lli in
     ps, BX.UNIT (V.Term.EXP x, V.Op.NE, V.Term.zero)
     
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
  let vt = L.classify_value lli in
  match vt with
    VK.GlobalVariable ->
     false
  | VK.ConstantExpr ->
     false
  | VK.Instruction opcode ->
      (* let opcode = (try L.instr_opcode lli with _ ->
                  pf "         %s" (stv lli);
                  raise (OPC "is_indirect")) in *)
      opcode = O.Load && is_pointer lli
  | _ ->
     pf "         %s" (stv lli);
     raise (OPC "is_indirect") 
;;

let is_const = L.is_constant ;;

let get_call lli =
  if is_cast lli then
    begin
      let casted_from = get_casted_from lli in
      F.dbgf "CALL" "Casted from: %s" (stv casted_from);
      casted_from
    end
  else
    lli
  
let get_called_fname vars lli =
  let argn = L.num_operands lli in
  F.dbgf "CALL" "Number of operands: %d" argn;
  let op2 = L.operand lli (argn-1) in
  let vk = L.classify_value op2 in 
  if vk = VK.Function then
    begin
      F.dbgf "CALL" "It is a function";
        let fname = get_var vars op2 in
        fname        
    end
  else
    begin
        F.dbgf "CALL" "Call to: %s" (stv op2);
        F.dbgf "CALL" "Not a function: %s" (P.print_value_kind vk);
        if L.num_operands op2 = 1 then
          begin
            let op21 = L.operand op2 0 in
            let fname = get_var vars op21 in
            fname
          end
        else
          raise (Err "Unknown constant expr as call")
    end
;;


let is_load lli =
  let opcode = (try L.instr_opcode lli with _ -> raise (OPC "@is_load")) in
  opcode = O.Load
;;


let get_parameters vars lli =
  let n = L.num_operands lli in
  F.dbgf "CALL" "Operands: %d" (n);
  
  let rec get_params i n acc pss =
    if i<n-1 then
      begin
        let p = L.operand lli i in

        let ps, v' = get_exp_from_llvalue ~addr:true vars p in
        let v = B._T v' in
        get_params (i+1) n (acc @ [v]) (pss@ps)
      end
    else
      pss, acc in
  get_params 0 n [] []
;;

let get_invoke_parameters vars lli =
  let n = L.num_operands lli in
  let rec get_params i n acc pss =
    if i<n-1 then
      begin
        let p = L.operand lli i in

        let ps, v' = get_exp_from_llvalue vars p in
        let v = B._T v' in
        get_params (i+1) n (acc @ [v]) (pss@ps)
      end
    else
      pss, acc in
  get_params 0 n [] []
;;


let add_ps ps p =
  B.join_at_last p (B.join_progs ps)
;;

let mk_assign_maybe vars lli o =
  let vname = L.value_name lli in
  
  let t_operand ?(addr=false) i =
    let opc = L.operand lli i in
    let ps, opv =
      try get_exp_from_llvalue vars ~addr:addr opc with
        Not_found -> raise Not_found
      | Err s -> raise (Err s)
      | e -> raise e
    in
    let pre_prog, t_opc =
      if E.is_ptr opv then
        let attr = E.get_attributes opv |> remove_first_ptr in
        let nv = fresh_var attr in
        let pre_prog = B.lookup (nv) (B._T opv) "*" in
        pre_prog, B._T nv
      else
        B.SKIP, B._T opv in
    ps, pre_prog, t_opc in
  
  if vname = "" then
    B.SKIP
  else
    let _, v_vname = get_var vars lli in
    let bin op =
      let ps1, pre_prog1, opr1 = t_operand 0 in
      let ps2, pre_prog2, opr2 = t_operand ~addr:true 1 in
      
      let t = B.T.bin_op opr1 opr2 op in
      let p2 = B.assign v_vname t in
      let p1 = B.join_at_last p2 pre_prog2 in
      let p =  B.join_at_last p1 pre_prog1 in
      add_ps (ps1@ps2) p in
    match o with
      O.BitCast
    | O.Trunc ->
       let ps3, pre_prog, t_opc = t_operand 0 in
       add_ps (ps3) (B.join_at_last pre_prog @@ B.assign v_vname t_opc)
    | O.Add ->
       bin V.Op.ADD
    | O.Sub ->
       bin V.Op.SUB
    | O.Mul ->
       bin V.Op.MUL
    | O.SDiv ->
       
       let p = bin V.Op.DIV in
       (* B.pprint 0 p; *)
       p
    | O.Or
      | O.And
      | _ -> B.SKIP

let rec indirect_src dest vars lli =
  let nm = L.value_name lli in
  let _, e_src = try get_exp_from_llvalue vars lli with e -> raise e in
  match L.classify_value lli with
    VK.Instruction O.GetElementPtr ->
     let ps, (ptr, fld) = get_ptr_element vars lli in
     if fld = "-" then
       add_ps ps @@ B.assign dest ptr
     else
       add_ps ps @@ B.lookup dest ptr fld
  | VK.Instruction O.Load ->
     if VR.mem nm vars then
       B.assign dest (B._T e_src)
     else
       indirect_src dest vars @@ L.operand lli 0
  | _ ->
     B.assign dest (B._T e_src)
;;

let rec to_while_t_inst vars lli =
  let st l = L.string_of_llvalue l in
  let str = st lli in
  F.dbg "CUR" "Current Instr:" F.p str;
  let opcode = try L.instr_opcode lli with  _ -> pf "OPCode - 1"; raise (OPC "OpCode -1") in
  let vars', r =
    match opcode with
    | O.Invalid -> vars, B.SKIP
    | O.Ret ->
       
       if L.num_operands lli = 0 then
         vars, B.return (B._T @@ V.Exp.CONST 0)
       else
         begin
           F.dbgf "RET" "operand: %d" (L.num_operands lli);
           F.dbgf "RET" "operand: %s" (stv lli);
           let rt = L.operand lli 0 in
           let ty = L.type_of rt in
           let tv = L.classify_type ty in
           let r =
             match tv with
               L.TypeKind.Void ->
                raise (Err "N/S Void")
              |	Integer | Pointer ->
                 if L.is_constant rt then
                   let ps, e = try get_exp_from_llvalue vars rt with e -> Printf.printf "to_while_t_inst(Ret)\n"; raise e in
                   add_ps ps @@ B.return (B._T e)
                 else
                   if is_load rt then
                     let dt = L.operand rt 0 in
                     if is_ptr_element dt then
                       begin
                         F.dbgf "RET" "Is a PTR";
                         let _, e_vname = get_var vars dt in
                         let t_vname = B._T e_vname in
                         let p2 = B.return t_vname in
                         let ps2, (ptr, fld) = get_ptr_element vars dt in
                         let p1 = if fld = "-" then B.assign e_vname ptr else B.lookup e_vname ptr fld in
                         let p = B.join_at_last p2 p1 in
                         add_ps ps2 p
                       end
                     else
                       begin
                         let ps, e_dt = get_exp_from_llvalue vars dt in
                         if (E.is_ptr e_dt && not(E.is_ptr !current_func)) then
                           let attr = E.get_attributes !current_func in
                           let tmp = fresh_var attr in
                           let p1 = B.decl tmp 1 in
                           let p2 = B.lookup tmp (B._T e_dt) "*" in
                           let p3 = B.return (B._T tmp) in
                           B.join_progs (ps@[p1;p2;p3])
                         else
                           let t_dt = B._T e_dt in
                           add_ps ps @@ B.return t_dt  
                       end
                   else
                     begin
                       F.dbgf "RET" "Is not a Load";
                       let ps, e_rt = get_exp_from_llvalue vars rt in
                       let t_rt = B._T e_rt in
                       add_ps ps @@ B.return t_rt
                     end
              | X86_mmx ->
                 B.return (B._T @@ V.Exp.CONST 0)
              | _ ->
                 F.dbgf "RET" "Type: %s\n" (P.string_of_type ty);
                 pf "Other kinds of return: %d %s\n" (L.num_operands lli) (L.string_of_llvalue lli);
                 B.SKIP
           in
           vars, r
         end 
    | O.Br ->
       vars, B.SKIP
    | O.Switch ->
       raise (Err "Switch - Not understood yet")
    | O.IndirectBr ->
       raise (Err "Switch - Not understood yet")
    |	O.Add ->
       vars, mk_assign_maybe vars lli O.Add
    |	Mul ->
       vars, mk_assign_maybe vars lli O.Mul
    |	FAdd ->
       vars, mk_assign_maybe vars lli O.Add
    |	FSub ->
       vars, mk_assign_maybe vars lli O.Sub
    |	FMul ->
       vars, mk_assign_maybe vars lli O.Mul
    |	UDiv ->
       vars, mk_assign_maybe vars lli O.SDiv
    |	SDiv ->
       F.dbgf "SDIV" "%s" (stv lli);
       vars, mk_assign_maybe vars lli O.SDiv
    |	FDiv ->
       vars, mk_assign_maybe vars lli O.SDiv
    |	Or ->
       vars, mk_assign_maybe vars lli O.Or
    |	O.Alloca ->
       begin
         let vars', v = get_var vars ~is_local:true ~alloca:true lli in
         F.dbgf "DEB1" "(%a) %s" E.fstr v (stv lli);
         let tp = L.type_of lli in
         match get_array_type tp with
           Some tp ->
            begin
              add_struct lli;
              let size = L.array_length tp in
              vars', B.decl v size
            end
         | None ->
            let op = L.operand lli 0 in
            let ps, size = get_exp_from_llvalue vars op in
            F.dbgf "DEB1" "(%a) size:  %a" E.fstr v E.fstr size;
            add_struct lli;
            begin
              match size with
                E.CONST sz ->
                 if sz > 1 then
                   begin
                     let r = B.decl v sz in
                     F.pf_s "DEB1" (B.pprint 0) r;
                     F.dbgf "DEB1" "...............";
                     vars', add_ps ps r
                   end
                 else
                   begin
                     F.dbgf "DEB1" "Non-Array";
                     (* let vars', v = get_var vars ~is_local:true lli in *)
                     let r = B.decl v 1 in
                     vars', add_ps ps r
                   end
              | _ ->
                 let p1 = B.decl v 1 in
                 let p2 = B.malloc v size in
                 let p = B.join_at_last p2 p1 in
                 vars, p
            end
       end
    |	Load | ExtractValue ->
       let lli_name = L.value_name lli in
       if lli_name = "" then
         vars, B.SKIP
       else
         let src = L.operand lli 0 in
         let vars', e_dest = get_var vars lli in
         let ps, t_src = get_exp_from_llvalue vars' src in
         let t_src = B._T t_src in
         let p = B.assign e_dest t_src in
         vars', add_ps ps p
    |	Store ->
       let src = L.operand lli 0 in                                            
        
       let ps, e_src = try get_exp_from_llvalue vars src with e -> raise e in  F.dbgf "STO" "Src: %a" E.fstr e_src;
       let dest = L.operand lli 1 in                                           F.dbgf "STO" "Dest:%s" (stv dest); 
       if is_indirect dest then
         begin                                                                 F.dbgf "STO" "Indirect Case";
           let vars'', destv' = L.operand dest 0 |> get_var vars in
           vars'', add_ps ps @@ B.mutation (B._T destv') "*" (B._T e_src)
         end
       else if is_ptr_element dest then
         begin                                                                 F.dbgf "STO" "Ptr_Element Case";
           let ps2, (ptr, fld) = get_ptr_element vars dest in                 F.dbgf "STO" "%s: %a %s" (stv lli) T.fstr ptr fld;
           let p = if fld="-" then
                     B.assign (T.toExp ptr) (B._T e_src)
                   else
                     B.mutation ptr fld (B._T e_src) in
           vars, add_ps (ps@ps2) p
         end
       else if is_const src then
         begin                                                                 F.dbgf "STO" "Const source";
           let vars', dest_v = get_var vars dest in
           vars', add_ps ps @@ B.assign dest_v (B._T e_src)
         end
       else if is_pointer src then
         begin                                                                 F.dbgf "STO" "Pointer Src";
           let ps1, t_src = get_exp_from_llvalue vars src in
           let ps2, v_dest = get_exp_from_llvalue vars dest in
           vars, add_ps (ps1@ps2) @@ B.assign v_dest (B._T t_src)
         end
       else
         begin                                                                 F.dbgf "STO" "Else Src";
           let ps1, e_src = get_exp_from_llvalue vars src in
           let ps2, v_dest = get_exp_from_llvalue vars dest in
           let p = indirect_src v_dest vars src in
           vars, add_ps (ps1@ps2) p
         end
    |	GetElementPtr ->
       let vn = L.value_name lli in
       if vn = "" (* || not (VR.mem vn vars) *) then (** 6/16: || not (VR.mem vn vars) *)
         begin
           F.dbgf "GEP" "vn is '' or vn not in vars";
           vars, B.SKIP
         end
       else
         begin
           try
             F.dbgf "GEP" "vn is not '' and vn in vars";
             let vars', e_vname = get_var vars lli in
             
             let ps, (ptr, fld) = get_ptr_element vars' lli in
             
             if fld = "-" then
               vars', add_ps ps @@ B.assign e_vname ptr
             else
               vars', add_ps ps @@ B.lookup e_vname ptr fld
           with
             e ->
             F.pn "Exception at GetElementPtr";
             raise e
         end
    |	ZExt
      |	Trunc ->
       begin
         try
           vars, mk_assign_maybe vars lli O.Trunc
         with
           PHI instr ->
           F.dbgf "PHI" "Found Phi. Phis: %d" (List.length !phis);
           
           let (_, bexp) = List.find (fun (instr',_) ->
                               F.dbgf "PHI" "%s\n%s" (stv instr) (stv instr');
                               instr'=instr) !phis in
           let vars', v = get_var vars lli in
           let p0 = B.decl v 1 in
           let p1 = B.assign v T.one in
           let p2 = B.assign v T.zero in
           let p = B.join_at_last (B.mk_if bexp p1 p2) p0 in
           vars', p
       end
    |	SExt ->
       let src = L.operand lli 0 in
       let vars', dest_v = get_var vars lli in
       if is_fcall src then
         begin
           try
             let called_fun = get_call src in
             let vars'', fname = get_called_fname vars' called_fun in
             let sname = fst @@ E.decode fname in
             if sname = "malloc" then
               begin
                 let op1 = L.operand called_fun 0 in
                 let ps, size = try get_exp_from_llvalue vars'' op1 with e -> pf "Store array element D\n"; raise e in
                 vars'', add_ps ps @@ B.malloc dest_v size
               end
             else
               begin
                 let ps, params = get_parameters vars'' called_fun in
                 let t_fname = B._T @@ (get_var vars'' called_fun |> snd) in
                 let p1 = add_ps ps @@ B.call ~ret:(Some dest_v) t_fname params in
                 vars'', p1
               end
           with
             e ->
             pf "exception at fcall";
             pf "%s" str;
             raise e
         end
       else
         let ps, src_exp = get_exp_from_llvalue vars' src in
         vars', add_ps ps @@ B.assign dest_v (B._T src_exp)
    |	BitCast ->
       vars, mk_assign_maybe vars lli O.BitCast
    |	O.ICmp
      | FCmp ->
       let vn = L.value_name lli in
       if vn = "" || List.mem lli !comps then
         vars, B.SKIP
       else
         begin
           let vars', v = get_var vars lli in
           let p0 = B.decl v 1 in
           let ps, b = get_bexp_from_comp vars @@ lli in
           let p1 = B.assign v T.one in
           let p2 = B.assign v T.zero in
           let p = B.mk_if b p1 p2 in
           vars', add_ps ps @@ B.join_at_last p p0
         end
    |	Call ->
       begin
         try
           F.dbgf "CALL" "It is a call: %s" (stv lli);

           let called_fun = get_call lli in
           let vars', fname = get_called_fname vars called_fun in
           let sname = fst @@ E.decode fname in
           if sname = "malloc" || sname = "_Znwm" then
             begin
               let op1 = L.operand called_fun 0 in
               let ps1, size = try get_exp_from_llvalue vars' op1 with e -> pf "Store array element D\n"; raise e in
               let ps2, v = get_exp_from_llvalue vars lli in
               vars', add_ps (ps1@ps2) @@ B.malloc v size
             end
           else if sname = "assert" then
             let arg_instr = L.operand lli 0 in
             let ps, tval = get_exp_from_llvalue vars arg_instr in
             let bexp = BX.UNIT (B._T tval, V.Op.NE, T.zero) in
             let p = add_ps ps @@ B.mk_assert bexp in
             vars', p
           else if sname = "__assert_fail" then
             let bexp = BX.UNIT (T.zero, V.Op.NE, T.zero) in
             let p = B.mk_assert bexp in
             vars', p
           else if sname = "llvm_memset_p0i8_i64" then
             vars, B.SKIP
           else
             begin
               
               let ps, params = get_parameters vars' called_fun in
               F.dbgf "CALL" "Parameters: %d" (List.length params);

               let t_fname = B._T @@ fname in
               
               let vv = L.value_name lli in
               if vv = "" then
                 begin
                   let p1 = add_ps ps @@ B.call t_fname params in
                   vars', p1
                 end
               else
                 begin
                   F.dbgf "CALL" "vv:%s" vv;
                   let ps2, v = get_exp_from_llvalue vars lli in
                   let p0 = B.decl v 1 in
                   let p1 = add_ps (ps@ps2) @@ B.call ~ret:(Some v) t_fname params in
                   F.dbg "CALL" "" (B.pprint 0) p1;
                   vars, B.join_at_last p1 p0
                 end 
             end
         with
           e ->
           raise e
       end       

    | Invoke ->
       let vv = L.value_name lli in
       if vv = "" then     
         let called_fun = get_call lli in
         let vars', fname = get_called_fname vars called_fun in
         let ps, params = get_invoke_parameters vars called_fun in
         let t_fname = B._T fname in
         let p0 = B.call t_fname params in
         let on = L.num_operands lli in
         let p1_blk = L.block_of_value @@ L.operand lli (on-3) in
         let p2_blk = L.block_of_value @@ L.operand lli (on-2) in
         let p1,vars''  = L.fold_right_instrs (fun i (acc,_vars) ->
                              let _vars', b = to_while_t_inst _vars i in
                              B.join_at_last acc b, _vars') p1_blk (B.SKIP,vars') in
         let p2,vars''' = L.fold_right_instrs (fun i (acc,_vars) ->
                              let _vars', b = to_while_t_inst _vars i in
                              B.join_at_last acc b, _vars') p2_blk (B.SKIP,vars'') in
         let b = BX.non_det in
         let p012 = B.mk_if b p1 p2 in
         vars''', add_ps ps @@ B.join_at_last p012 p0
       else
         vars, B.SKIP
    |	LandingPad->
       let lli_name = L.value_name lli in
       if lli_name = "" then
         vars, B.SKIP
       else
         let vars',e_dest = get_var vars lli in
         let t_src = B._T @@ B.const 0 in
         let p = B.assign e_dest t_src in
         vars', p
         
         
    |	InsertValue
    |	PHI
      
      -> vars, B.SKIP
    |	Resume
      |	Unreachable
      -> vars, B.SKIP
    |	Sub
      |	And
      -> 
       vars, mk_assign_maybe vars lli O.Sub
    | _ ->
       raise (Err ("\nUnexpected ValueKind " ^ str ^ "\n"))
  in
  
  if List.length !temps > 0 then
    vars', List.fold_left (fun acc i -> B.join_at_last r i) r !temps 
  else
    vars', r
         
and to_while_t_blk f vars blk =
  let vars', ps = Llvm.fold_left_instrs
             (fun (_vars,acc) lli ->
               let _vars', l = to_while_t_inst _vars lli in
               (* B.pprint 2 l ; *)
               match l with
                 B.SKIP ->
                  _vars', acc
               | _ ->
                  _vars', acc @ [WPROG l]
             (* acc @ [WINST lli] *)
             ) (vars,[])
             blk in
  let ps' = List.filter (function
                  WPROG B.SKIP -> false
                | WPROG (B.BLOCK (B.SKIP, _, _)) -> false
                | _ -> true
              ) ps in
  match ps' with
    [] -> vars', WPROG B.SKIP
  | _ when List.for_all (function WPROG B.SKIP -> true | _ -> false) ps -> vars, WPROG B.SKIP
  | _ ->
     vars', WBLK ps
;;
            
let rec to_while_t_blks f vars i j blk_arr acc =
  if i <= j then
    begin
      let llbb = Array.get blk_arr i in

      let vars', p = to_while_t_blk f vars llbb in
      
      match p with
        WPROG B.SKIP ->
         to_while_t_blks f vars' (i+1) j blk_arr acc
      | _ ->
         to_while_t_blks f vars' (i+1) j blk_arr (acc@[p])
    end
  else
    match acc with
      [WBLK acc'] ->
       vars, WBLK acc'
    | _ ->
       vars, WBLK acc  
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

let parse_unreachable blk =
  let last_inst = L.instr_end blk in
  match last_inst with
    L.After inst ->
     begin
       match L.classify_value inst with
         L.ValueKind.Instruction (L.Opcode.Unreachable) ->
          true
       | _ -> false
     end
  | _ -> false
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

let search_phi vars _L0 _L1 blk =
  
  match L.instr_begin blk with
    L.Before instr ->
     begin
       match L.instr_opcode instr with
         O.PHI ->
          if L.num_operands instr = 2 then
            begin
              let op1 = L.operand instr 0 in
              let op2 = L.operand instr 1 in
              let ps1, exp1 = get_bexp_from_llvalue vars op1 in
              let ps2, exp2 = get_bexp_from_llvalue vars op2 in
              details vars op1;
              details vars op2;
              Some (ps1@ps2, instr, exp1, exp2)
            end
          else
            None
       | _ -> None
     end
  | _ -> 
     None


let done_blocks = ref (-1);;
    
let parse_or (dpkg:pkg) ps = function 
    None -> None
  | Some x ->
     let rec aux = function
         [] -> None
       | p::ps' ->
          try
            match p dpkg x with
              Some _ as r -> r
            | None -> aux ps'
          with
            Not_parsed ->
            aux ps'
     in
     aux ps
;;

let parse_br_cond blk_arr (i,n) =
     let rec aux i' =
       if i' < n then
         let p0_blk = Array.get blk_arr i' in
         let _P0_A_L1_L2 = parse_conditional p0_blk in
         match _P0_A_L1_L2 with
           Some (_, inst1, _A, _L1, _L2) ->
            let r = ((i'+1,n), (inst1, _A, _L1, _L2)) in
            r
         | None ->
            aux (i'+1)
       else
         raise Not_parsed
     in
     let r = aux i in
     r
;;

let next_lbl_is blk_arr lblT (i,_) =
     let p1_blk = Array.get blk_arr i in
     let _L1' = P.get_blk_name p1_blk in
     if _L1' = lblT then
       ()
     else
       raise Not_parsed
;;

let next_br_is blk_arr lbl (i,n) =
  match search_label lbl i n blk_arr with
    None -> raise Not_parsed
  | Some (j, inst) -> ((j+1,n), inst) 
;;

let rec next_br blk_arr (i,n) =
  if i > n then
    raise Not_parsed
  else
    let p1_blk = Array.get blk_arr i in
    match parse_unconditional p1_blk with
      Some (inst, _L1) ->
       (inst, i, _L1)
    | None ->
       next_br blk_arr (i+1,n)

      
let rec next_br_lbl blk_arr (i,n) =
  let (inst, i', _L1) = next_br blk_arr (i,n) in
  next_lbl_is blk_arr _L1 (i+1,n);
  (inst, i)

let br_at_end blk_arr i =
  let p1_blk = Array.get blk_arr i in
  match parse_unconditional p1_blk with
    Some (inst, _L) ->
     (inst, _L)
  | None -> raise Not_parsed

let unreachable_at_end blk_arr i =
  let p1_blk = Array.get blk_arr i in
  parse_unreachable p1_blk

let cond_br_at_end blk_arr i =
  let p1_blk = Array.get blk_arr i in
  match parse_conditional p1_blk with
    Some (blk, inst, _A, _L2, _L3) ->
     (inst, _A, _L2, _L3)
  | None -> raise Not_parsed

let rec try_parse f k n =
  try
    if k>n then
      None
    else
      f k 
  with
    Not_parsed -> 
    try_parse f (k+1) n
  | _ ->
     try_parse f (k+1) n
;;

let get_intrp_v = function
    BRK -> break
  | BRK_THEN -> break
  | BRK_ELSE -> break
  | _ -> continue
;;

let parse_break (lblEw, sg) (blk_arr,_,_) ((i,n) as seg) =
  F.dbgf "INTRP" "Parse Break:: B/C:%s in (%d,%d)" lblEw i n;
  try
    let (((i',_) as seg1), (inst1, _A, lblTh, lblEn)) = parse_br_cond blk_arr seg in
    F.dbgf "INTRP" "Parse Break:: BR Th:%s En:%s in %d" lblTh lblEn (fst seg);
    let pre_seg = (i, i'-1) in
    F.dbgf "INTRP" "Parse Break:: checking Label %s in %d" lblTh (fst seg1);
    next_lbl_is blk_arr lblTh seg1;

    F.dbgf "INTRP" "Parse Break:: checking BR %s in %d" lblEw (fst seg1);
    let ((j,n), inst2) = next_br_is blk_arr lblEw seg1 in
    let then_seg = (i',j-1) in
    let rest_seg = (j,n) in
    F.dbgf "INTRP" "Parse Break:: checking Label %s in %d" lblEn (fst rest_seg);
    next_lbl_is blk_arr lblEn rest_seg;
    
    (* F.dbgf "INTRP" "Parse Break:: checking BR %s in %d" lblC (j);
    let (rest_seg, inst3) = next_br_is blk_arr lblC then_seg in *)
    F.dbgf "INTRP" "Parse Break:: Passed";
    
    let dl_instr = [inst1;inst2(* ;inst3 *)] in
    Some (sg, B.var (get_intrp_v sg) [], dl_instr, _A, pre_seg::then_seg::(1,0)::rest_seg::[])
  with Not_parsed -> None  
;;

let parse_break_then (lblEw, sg) (blk_arr,_,_) ((i,n) as seg) =
  F.dbgf "INTRP" "Parse Break Then:: B/C:%s in (%d,%d)" lblEw i n;
  try
    let (((i',_) as seg1), (inst1, _A, lblTh, lblEl)) = parse_br_cond blk_arr seg in
    F.dbgf "INTRP" "Parse Break Then:: BR Th:%s En:%s in %d" lblTh lblEl (fst seg);
    let pre_seg = (i, i'-1) in
    F.dbgf "INTRP" "Parse Break Then:: checking Label %s in %d" lblTh (fst seg1);
    next_lbl_is blk_arr lblTh seg1;

    F.dbgf "INTRP" "Parse Break Then:: checking BR %s in %d" lblEw (fst seg1);
    let ((j,n), inst2) = next_br_is blk_arr lblEw seg1 in
    let then_seg = (i',j-1) in
    
    F.dbgf "INTRP" "Parse Break Then:: checking Label %s in %d" lblEl j;
    next_lbl_is blk_arr lblEl (j,n);

    begin
      let f k =
        if k<n then
          begin
            let (inst1, lblEn) = br_at_end blk_arr k in
            F.dbgf "INTRP" "BR %s at last %d" lblEn k;
            next_lbl_is blk_arr lblEn (k+1,n);
            Some (inst1, k+1)
          end
        else
          None
      in
      match try_parse f j n with
        Some (inst5, j') ->
         let else_seg = (j,j'-1) in
         let rest_seg = (j',n) in
         (* F.dbgf "INTRP" "Parse Break Then:: checking BR %s in %d" lblC (j');
         let (rest_seg, inst3) = next_br_is blk_arr lblC (j',n) in
          *)
         F.dbgf "INTRP" "Parse Break Then:: Passed";
         let dl_instr = [inst1;inst2;(* inst3; *)inst5] in
         Some (sg, B.var (get_intrp_v sg) [], dl_instr, _A, pre_seg::then_seg::else_seg::rest_seg::[])
      | None -> None
    end
  with Not_parsed -> None  
;;

let parse_break_else (lblEw, sg) (blk_arr,_,_) ((i,n) as seg) =
  F.dbgf "INTRP" "Parse Break Else:: B/C:%s in (%d,%d)" lblEw i n;
  try
    let (((i',_) as seg1), (inst0, _A, lblTh, lblEl)) = parse_br_cond blk_arr seg in
    F.dbgf "INTRP" "Parse Break Else:: BR Th:%s En:%s in %d" lblTh lblEl (fst seg);
    let pre_seg = (i, i'-1) in
    F.dbgf "INTRP" "Parse Break Else:: checking Label %s in %d" lblTh (fst seg1);
    next_lbl_is blk_arr lblTh seg1;

    begin
      let f k =
        let (inst1, lblEn) = br_at_end blk_arr k in
        next_lbl_is blk_arr lblEl (k+1,n);
        
        let f2 k2 =
          let ((k2',n'), inst2) = next_br_is blk_arr lblEw (k2,n) in
          next_lbl_is blk_arr lblEn (k2',n');
          Some (k2', inst2)
        in
        let then_seg = (i',k) in
        match try_parse f2 (k+1) n with
          Some (k2', inst2) ->
           let else_seg = (k+1,k2'-1) in
        
           Some (k2', then_seg, else_seg, [inst1;inst2])
        | None ->
           raise Not_parsed
      in
      match try_parse f i' n with
        Some (j', then_seg, else_seg, dl_instr) ->
         F.dbgf "INTRP" "Parse Break Else:: Passed";
         let dl_instr = inst0::dl_instr in
         let rest_seg = (j',n) in
         Some (sg, B.var (get_intrp_v sg) [], dl_instr, _A, pre_seg::then_seg::else_seg::rest_seg::[])
      | None -> None
    end
  with Not_parsed -> None  
;;

let remove_instructions dl_instrs =
  List.iter L.delete_instruction dl_instrs;; 

let to_cond vars _A =
  get_bexp_from_comp vars _A
;;

let interruption_then v = function
    BRK
  | CONT
    | BRK_THEN
    | CONT_THEN -> B.assign v (_TC 1)
  | _ -> B.SKIP
;;

let interruption_else v = function
  | BRK_ELSE
    | CONT_ELSE -> B.assign v (_TC 1)
  | _ -> B.SKIP
;;

let to_prog ((blk_arr, vars, f):pkg) (k,n) =
  let vars', prog = to_while_t_blks f vars k n blk_arr [] in
  vars', to_while_t prog
;;

let rec parse_interruptions ((blk_arr, vars, f) as dpkg) (seg:int * int) lblC lblEw =
  let r = parse_or dpkg [parse_break_then (lblEw, BRK);
                         parse_break_else (lblEw, BRK_ELSE);
                         parse_break (lblEw, BRK);
                         parse_break_then (lblC, CONT);
                         parse_break_else (lblC, CONT_ELSE);
                         parse_break (lblC, CONT);
            ] (Some seg) in
  
  match r with
    Some (k, v, dl_instr, _A, pre_seg::then_seg::else_seg::rest_seg::[]) ->
     begin
       F.dbgf "INTRP" "pre:(%d,%d), then:(%d,%d), else:(%d,%d), rest:(%d,%d)"
         (fst pre_seg) (snd pre_seg) (fst then_seg) (snd pre_seg) (fst else_seg) (snd else_seg) (fst rest_seg) (snd rest_seg);
       remove_instructions dl_instr;
       F.dbgf "INTRP" "interrupted";
       comps := _A::!comps;
       let interrupt_init = B.assign v (_TC 0) in
       let v'0, pre_prog   = parse_program dpkg pre_seg  in
       let ps, cond       = to_cond vars _A in
       let v'1, then_prog' = parse_program (blk_arr, v'0, f) then_seg in
       let v'2, else_prog' = parse_program (blk_arr, v'0, f) else_seg in
       let then_ext   = interruption_then v k in
       let else_ext   = interruption_else v k in
       let then_prog  = B.block @@ B.join_progs [then_prog'; then_ext] in
       let else_prog  = B.block @@ B.join_progs [else_prog'; else_ext] in
       let r' = parse_interruptions (blk_arr, v'0, f) rest_seg lblC lblEw in
       match r' with
         Some (res, v'3, rest_prog) ->
          F.dbgf "INTRP" "More interruption";
          let if_prog = B.mk_if cond then_prog else_prog in
          let natural_cond = BX.UNIT (_T v, V.Op.EQ, _TC 0) in
          let rest_in_if =
            if B.is_empty rest_prog then
              if k = BRK || k = BRK_ELSE then
                (B.block B.SKIP)
              else
                (B.block @@ B.assign v (_TC 0))
            else
              let prg = B.mk_if natural_cond (rest_prog) (B.block B.SKIP) in 
              if k = BRK || k = BRK_ELSE then
                prg
              else
                B.join_at_last (B.assign v (_TC 0)) prg
          in
          begin
            let res_pair = (v,interrupt_init,k) in
            match res with
              Some res_pairs when not (List.mem res_pair res_pairs) ->
              Some (Some (res_pair::res_pairs),
                    v'3,
                    B.block @@ B.join_progs (ps@[pre_prog;if_prog;rest_in_if]))
            | _ ->
               Some (Some [res_pair],
                    v'3,
                    B.block @@ B.join_progs (ps@[pre_prog;if_prog;rest_in_if]))
          end
       | None ->
          F.dbgf "INTRP" "No more interruption";
          let v'4, rest_prog = parse_program (blk_arr, v'2, f) rest_seg in
          Some (None, v'4, add_ps ps @@ rest_prog)
     end
  | _ ->
     
     F.dbgf "INTRP" "Not an interruption";
     let v'4, prog = parse_program (blk_arr, vars, f) seg in
     Some (None, v'4, prog)

and parse_do (blk_arr, vars, f) (pre_seg, (inst0,lblB), (i,n)) =
  
  let f1 k =
    if k >= n then
      raise Not_parsed
    else
      begin
        let (inst1, _A, lblB', lblDe) = cond_br_at_end blk_arr k in
        if lblB = lblB' then
          begin
            next_lbl_is blk_arr lblDe (k+1,n);
            let lblC =
              let blk = Array.get blk_arr k in
              P.get_blk_name blk in
            Some (lblC, lblDe, _A, [inst1], (i,k))
          end
        else
          raise Not_parsed
      end
  in
  match try_parse f1 i n with
    Some (lblC, lblDe, _A, dl_inst, (i',n')) ->
     comps := _A::!comps;
     let rest_seg = (n', n) in
     begin
       let v'1, pre_prog = to_prog (blk_arr, vars, f) pre_seg in
       
       let r' = parse_interruptions (blk_arr, v'1, f) (i',n'-1) lblC lblDe in
       match r' with
         Some (v_o, v'2, prog) ->
          begin
            
            let body_prog = prog in
            let ps, local_cond = to_cond v'2 _A in
            let cond, interrupt_init =
              match v_o with
                Some res_pairs ->
                List.fold_left (fun (local_cond, prg) (v,interrupt_init,k) ->
                    let interruption_cond = BX.UNIT (B._T v, V.Op.EQ, _TC 0) in
                    if k = BRK || k = BRK_ELSE then
                      BX.(local_cond &. interruption_cond), interrupt_init::prg
                    else
                      local_cond, interrupt_init::prg
                   ) (local_cond, [B.SKIP]) res_pairs 
                 
              | None -> local_cond, [B.SKIP] in
            let while_prog = B.mk_while cond body_prog in
            let v'3, rest_prog = parse_program (blk_arr, v'2, f) rest_seg in
            let full_prog = B.join_progs (ps@(pre_prog::interrupt_init@[body_prog; while_prog;rest_prog])) in
            Some (v'3, full_prog)
          end
       | None -> None
     end
  | None ->
     raise Not_parsed
    
and parse_while (blk_arr, vars, f) (pre_seg, (inst0,lblC), (i,n)) = 
  F.dbgf "LOOP" "While beginning @";
  let f1 k =
    if k >= n then
      raise Not_parsed
    else
      begin
        
        let (inst1, _A, lblWb, lblWe) = cond_br_at_end blk_arr k in
        if lblC <> lblWb then
          begin
            next_lbl_is blk_arr lblWb (k+1,n);
            let f2 k2 =
              let ((k3,_), inst2) = next_br_is blk_arr lblC (k2,n) in
              next_lbl_is blk_arr lblWe (k3, n);
              let body_seg = (k+1,k3-1) in
              
              Some (k3, inst2, body_seg)
            in
            match try_parse f2 (k+1) n with
              Some (k4, inst2, body_seg) ->
               let cond_seg = (i,k) in
               let rest_seg = (k4,n) in
               Some (lblC, lblWe, _A, [inst1;inst2], cond_seg, body_seg, rest_seg)
            | None -> raise Not_parsed
          end
        else
          raise Not_parsed
      end
  in
  match try_parse f1 i n with
    Some (lblC, lblDe, _A, dl_inst, cond_seg, body_seg, rest_seg) ->
     comps := _A::!comps;
     F.dbgf "LOOP" "Body Segment: (%d,%d)" (fst body_seg) (snd body_seg); 
     begin
       let v'1, pre_prog = to_prog (blk_arr, vars, f) pre_seg in
       let v'2, cond_prog = parse_program (blk_arr, v'1, f) cond_seg in
       
       let r' = parse_interruptions (blk_arr, v'2, f) body_seg lblC lblDe in
       match r' with
         Some (v_o, v'3, prog) ->
          begin
            let body_prog = prog in
            let ps, local_cond = to_cond v'3 _A in
            let cond, interrupt_init =
              match v_o with
                Some res_pairs ->
                List.fold_left (fun (local_cond, prg) (v,interrupt_init,k) ->
                    let interruption_cond = BX.UNIT (B._T v, V.Op.EQ, _TC 0) in
                    if k = BRK || k = BRK_ELSE then
                      BX.(local_cond &. interruption_cond), interrupt_init::prg
                    else
                      local_cond, interrupt_init::prg
                   ) (local_cond, [B.SKIP]) res_pairs 
                 
              | None -> local_cond, [B.SKIP] in
            let while_prog = B.mk_while cond body_prog in
            let v'3, rest_prog = parse_program (blk_arr, v'2, f) rest_seg in
            let full_prog = B.join_progs (ps@(pre_prog::cond_prog::interrupt_init@[ while_prog;rest_prog])) in
            Some (v'3, full_prog)
          end
       | None -> None
     end
  | None ->
     raise Not_parsed

  
and parse_loop ((blk_arr, vars, f) as dpkg) (init_seg, (i,n)) =
  F.dbgf "LOOP" "Parse Loop:: init_seg:(%d,%d), (i,n):(%d,%d)" (fst init_seg) (snd init_seg) i n;
  let (inst1, lblBC) = br_at_end blk_arr i in  (** br label %do.body *)
  next_lbl_is blk_arr lblBC (i+1, n);          (** do.body: *)
  F.dbgf "LOOP" "B/C:%s" lblBC;
  let r = parse_or dpkg [parse_do; parse_while] (Some (init_seg, (inst1,lblBC), (i+1,n))) in
  F.dbgf "LOOP" "Parse Loop:: Passed";
  r

and parse_ands (blk_arr, vars, f) cond lblEl (i,n) =
  if i <= n then
    begin
      try
        let (inst1, _A, lblTh, lblEl') = cond_br_at_end blk_arr i in
        if lblEl' = lblEl then
          begin
            next_lbl_is blk_arr lblTh (i+1,n);
            comps := _A::!comps;
            let (ps1, then_seg, cond1) = parse_ands (blk_arr, vars, f) cond lblEl (i+1,n) in
            let ps2, cond2 = to_cond vars _A in
            (ps1@ps2, then_seg, BX.(cond1 &. cond2))
          end
        else
          ([], (i,n), cond)
      with
        Not_parsed ->
        ([], (i,n), cond)
    end
  else
    ([], (i,n), cond)

and parse_ifelse (blk_arr, vars, f) (is_negative, lblTh, lblEl, (i,n)) =
  F.dbgf "IF" "Trying If Else (%d,%d)" (i) n;
  let f k =
    let (inst1, lblEn) = br_at_end blk_arr k in
    if lblEn <> lblEl then
      begin
        next_lbl_is blk_arr lblEl (k+1,n);
        let f1 k1 =
          let ((j,n), inst2) = next_br_is blk_arr lblEn (k1,n) in
          next_lbl_is blk_arr lblEn (k1+1,n);
          Some (k1, inst2)
        in
        match try_parse f1 (k+1) n with
          None -> raise Not_parsed
        | Some (k1, inst2) ->
           F.dbgf "IF" "@1";
           Some ((i,k), (k+1,k1), (k1+1,n), [inst1;inst2], lblEn)
      end
    else
      if (is_negative && lblEn <> lblTh) then
        begin
        next_lbl_is blk_arr lblTh (k+1,n);
        let f1 k1 =
          let ((j,n), inst2) = next_br_is blk_arr lblEn (k1,n) in
          next_lbl_is blk_arr lblEn (k1+1,n);
          Some (k1, inst2)
        in
        match try_parse f1 (k+1) n with
          None -> raise Not_parsed
        | Some (k1, inst2) ->
           F.dbgf "IF" "@1";
           Some ((i,k), (k+1,k1), (k1+1,n), [inst1;inst2], lblEn)
        end
      else
        begin
          F.dbgf "IF" "Not Parsed If Else";
          raise Not_parsed
        end
  in
  try_parse f i n

  
and parse_ifonly (blk_arr, vars, f) (is_negative, lblTh, lblEl, (i,n)) =
  F.dbgf "IF" "Trying If Only (%d,%d)" (i) n;
  let f k =
    F.dbgf "IF" "Trying Progressive (%d,%d)" k n;

    try
      let (inst1, lblEn) = br_at_end blk_arr k in
      F.dbgf "IF" "Second %s" lblEn;
      
      if lblEn = lblEl then
        begin
          next_lbl_is blk_arr lblEl (k+1,n);
          
          Some ((i,k), (n,i-1), (k+1,n), [inst1], lblEl)
      end
      else if lblEn = lblTh then
        begin
          next_lbl_is blk_arr lblTh (k+1,n);
          
          Some ((i,k), (n,i-1), (k+1,n), [inst1], lblEl)
        end
      else
        raise Not_parsed
    with
      Not_parsed ->
       F.dbgf "IF" "TRY UnReachable";
      match unreachable_at_end blk_arr k with
      | true when is_negative ->
         F.dbgf "IF" "Found UnReachable";
         next_lbl_is blk_arr lblTh (k+1,n);
         Some ((i,k), (n,i-1), (k+1,n), [], lblTh)
      | _ ->
         F.dbgf "IF" "No UnReachable";
         raise Not_parsed
                        
  in
  try_parse f i n
  
and parse_if ((blk_arr, vars, f) as dpkg) (pre_seg, is_negative, (i,n), inst1, _A, lblTh, lblEnEl) =
  F.dbgf "IF" "Trying If";
  let r = parse_or dpkg [parse_ifelse; parse_ifonly] (Some (is_negative, lblTh, lblEnEl, (i,n))) in
  match r with
    Some (then_seg', else_seg, rest_seg, dl_insts, lblEl) ->
     comps := _A::!comps;
     remove_instructions dl_insts;
     F.dbgf "IF" "then: (%d, %d)" (fst then_seg') (snd then_seg');
     F.dbgf "IF" "else: (%d, %d)" (fst else_seg) (snd else_seg);
     let v'1, pre_prog = to_prog dpkg pre_seg in
     
     let ps1, cond'' = to_cond v'1 _A in
     let cond' = if is_negative then BX.complement cond'' else cond'' in
     let (ps2, then_seg, cond) = parse_ands (blk_arr, v'1, f) cond' lblEnEl then_seg' in 
     let v'2, then_prog = parse_program (blk_arr, v'1, f) then_seg in
     
     let v'3, else_prog = parse_program (blk_arr, v'1, f) else_seg in
     let if_prog = B.mk_if cond (B.block then_prog) (B.block else_prog) in
     let v'2, rest_prog = parse_program (blk_arr, v'1, f) rest_seg in
     let full_prog = B.join_progs (ps1@ps2@[pre_prog;if_prog;rest_prog]) in
     Some (v'2, full_prog)
  | None -> raise Not_parsed

and parse_phi ((blk_arr, vars, f) as dpkg) (pre_seg, is_negative, (i',n), inst1, _A, lblTh, lblEn) =
  F.dbgf "PHI" "Trying Phi";
  let i = i' - 1 in
  let p0_blk = Array.get blk_arr i in
  let _L0 = P.get_blk_name p0_blk in
  let p1_blk = Array.get blk_arr (i+1) in
  let p2_blk = Array.get blk_arr (i+2) in
  let _L1' = P.get_blk_name p1_blk in
  let _L2' = P.get_blk_name p2_blk in
  if lblTh=_L1' && lblEn=_L2' then
    let ((j,n'), inst2) = next_br_is blk_arr lblEn (i+1, i+2) in
    if j=i+2 then
      match search_phi vars _L0 lblTh p2_blk  with
        Some (ps, phi_instr, BX.UNIT (x, V.Op.NE, y), exp2) when x=y ->
         let ps1, exp0 = get_bexp_from_llvalue vars _A in
         let bexp  = BX.OP (exp0, V.Op.AND, exp2) in
         F.dbgf "PHI" "bexp: %a" BX.fstr bexp;
         F.dbgf "PHI" "phi_instr: %s" (stv phi_instr);
         phis := !phis @ [(phi_instr, bexp)];
         let vars',_WP1 = to_prog dpkg (fst pre_seg, i+1) in
         F.dbgf "PHI" "phi(WP1): (%d,%d)" (fst pre_seg) (i+1);
         let l = i+2 in
         let vars'', _WP3 = parse_program (blk_arr, vars', f) (l, n) in
         F.dbgf "PHI" "phi(WP3): (%d,%d)" l n;
         let _WP = B.block @@ B.join_progs (ps@ps1@[_WP1; _WP3]) in
         F.dbgf "PHI" "phi Done.";
         Some (vars'', _WP)
      | _ -> raise Not_parsed
    else
      raise Not_parsed
  else
    raise Not_parsed

and assert_fail ((blk_arr, vars, f) as dpkg) (pre_seg, is_negative, (i,n), inst1, _A, lblTh, lblEl) =
  F.dbgf "AST" "Trying Assert Fail: (%d,%d)" i n;
  let f k =
    F.dbgf "AST" "Assert Fail| Trying Progressive (%d,%d)" k n;

    let (inst2, lblEn) = br_at_end blk_arr k in
    F.dbgf "AST" "Assert Fail| Second %s" lblEn;

    if lblEn <> lblEl then
      begin
        next_lbl_is blk_arr lblEl (k+1,n);
        match unreachable_at_end blk_arr (k+1) with
          true when not is_negative ->
           comps := _A::!comps;
           remove_instructions [inst1;inst2];
           let v'1, pre_prog = to_prog dpkg pre_seg in
           let ps, cond' = to_cond v'1 _A in
           let cond = if is_negative then BX.complement cond' else cond' in
           let prog = B.mk_assert cond in
           let rest_seg = (k+2,n) in
           let v'2, rest_prog = parse_program (blk_arr, v'1, f) rest_seg in
           let full_prog = B.join_progs (ps@[pre_prog;prog;rest_prog]) in
           let _WP = B.block full_prog in
           Some (v'2, _WP)
        | _ ->
           F.dbgf "IF" "Assert Fail|Failed";
           raise Not_parsed
      end
    else
      raise Not_parsed
  in
  try_parse f i n


  
and parse_pos (blk_arr,_,_) (lblTh, _, (i, n)) =
  next_lbl_is blk_arr lblTh (i, n);
  Some false
  
and parse_neg (blk_arr,_,_) (_, lblEn, (i, n)) =
  next_lbl_is blk_arr lblEn (i, n);
  Some true
  
  
and parse_cond ((blk_arr, vars, f) as dpkg) (init_seg, (i,n)) =
  F.dbgf "IF" "Parse Cond (%d,%d)" (i) n;
  let (inst1, _A, lblTh, lblEnEl) = cond_br_at_end blk_arr i in
  F.dbgf "IF" "BR %s %s" lblTh lblEnEl;
  F.dbgf "IF" "A:%s" (stv _A);
  match parse_or dpkg [parse_pos; parse_neg] (Some (lblTh, lblEnEl, (i+1,n))) with
    Some is_negative ->
    F.dbgf "IF" "Second Pass";
    parse_or dpkg [parse_phi; parse_if; assert_fail] (Some (init_seg, is_negative, (i+1,n), inst1, _A, lblTh, lblEnEl))
  | None -> None
        
and parse_program (dpkg:pkg) (i,n) : E.attr list VR.t * B.t =
  let f k =
    if k >= n then
      raise Not_parsed
    else
      let init_seg = (i,k) in
      let seg' = (k,n) in
      let r = parse_or dpkg [parse_loop ; parse_cond ] (Some (init_seg, seg')) in
      r
  in
  match try_parse f i n with
    None -> to_prog dpkg (i,n)
  | Some r -> r
;;

let enblock = function
    B.BLOCK _ as p -> p
  | p -> B.block p
;;

let convert_struct =
  (fun (snm, flds) ->
    let e_flds = List.mapi (fun i fld_ty ->
                     let n = string_of_int i in
                     let v = B.var n (Types.get_types fld_ty) in
                     v, B.T.NULL
                   ) flds in
    (F.corr_structname snm, e_flds, None)
    )

let parse_func vars f =
  B.reset_declaration ();
  let blk_arr = Llvm.basic_blocks f in
  let vars', name = get_var vars f in
  current_func := name;
  F.dbgf "DEB" "Begin: %a" E.fstr name;
  let n = (Array.length blk_arr - 1) in

  if n = -1 then
    vars, Procedure.mk_procedure name [] (enblock B.SKIP)
  else
    begin
      let arr_prms = L.params f in
      let vars'', prms = Array.fold_left (fun (_vars, acc) a ->
                             let _vars', a' = get_var _vars ~is_param:true a in
                             B.add_to_declared a';
                             _vars',  acc@[a']
                           ) (vars',[]) arr_prms  in
      let vars'3 = VR.add continue [] @@ VR.add break [] vars'' in
      
      let pkg : pkg = (blk_arr, vars'3, f) in
      let vars'3, b = parse_program pkg (0, n) in
      let structs = List.fold_left (fun ss ((snm,flds) as s) -> VR.add (F.corr_structname snm) (convert_struct s) ss) VR.empty !structures in
      
      let _, b' = B.restore_prog structs b in
      
      let p = Procedure.mk_procedure name prms (enblock b') in
      F.dbgf "DEB" "End: %a" E.fstr name;
      vars'3, p
    end
;;



let translate_structures gs =
  let g_structures : G.t list =
    List.map convert_struct !structures |> List.map (fun x -> G.STRUCT (x, B.dl)) in
  gs @ g_structures
;;

let translate_global vars g =
  
  let vars', nm = get_var vars ~is_global:true g in
  let opn = L.num_operands g in
  let r =
    if opn > 0 then
      begin
        let op1 = L.operand g 0 in
        let tp = L.type_of op1 in
        match get_array_type tp with
          Some tp ->
          begin
            let size = L.array_length tp in
            G.STATEMENT (B.decl nm size)
          end
        | None ->
           let ps, t = get_exp_from_llvalue vars op1 in
           let init = B.INIT_S (B._T t) in
           G.STATEMENT (add_ps ps @@ B.decl_init nm 1 init)
      end
    else
      G.STATEMENT (B.decl nm 1)
  in
  vars', r
;;


let translate file =
  (* V.Options.show_types := true; *)
  F.p_opt := []; (*  ["FUNC";"PHI";"IF";"LOOP";"INTRP"] ;  *)
  F.pn_s "PRO" ("Translating " ^ file);
  
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file file in
  let llm   = Llvm_bitreader.parse_bitcode llctx llmem in

  let vars', gs1   = L.fold_left_globals (fun (_vars, acc) g ->
                         let _vars', g' = translate_global _vars g in 
                         _vars', acc @ [g']) (VR.empty,[]) llm in
  F.pn_s "DEB" "Globals are translated";

  (* Printf.printf "*** basic blocks/instructions ***\n" ;
  (* Llvm.iter_functions Print.print_fun llm ; *)
  Printf.printf "*** Translation ***\n" ; *)
  let _, fs = L.fold_left_functions (fun (_vars, acc) f ->
                  let _vars',f' = parse_func vars' f in
                  _vars', acc @[f']) (vars',[]) llm  in
  
  let gs2   = List.map (fun (func_name, params, body) ->
                  write_func func_name file params body) fs in
  let gs    = translate_structures (gs1@gs2) in
  (* List.iter (fun g -> Global.pprint g) gs; *)
  gs
