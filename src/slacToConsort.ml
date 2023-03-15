module F = Ftools
module S = Block
module C = ConsortAst
module A = Ast
module V = Base
module E = V.Exp
module T = V.Term
module B = V.BExp
module G = Global

module DM = Map.Make(String)
module S_E = Set.Make(E);;
module S_S = Set.Make(String);;
module M_S = Map.Make(String);;
module M_I = Map.Make(Int);;

exception NotSupported of string
exception Supported of string
exception Unexpected of string

let ldp = Lexing.dummy_pos ;;
let dp = (0,ldp) ;; 
let unit = C.Unit (dp) ;;
let show_not_found_function = ref false;;
let skip_not_found_function = ref false;;

let mk_val v = C.Value (dp, v) ;;
let mk_num n = mk_val (C.(`OInt n)) ;; 

let to_list s = S_E.fold (fun e l -> e::l) s [] ;;

let procedures : (Procedure.t list) ref = ref [] ;;
let structs = ref [] ;;
let arrays = ref [] ;;
              
let string_of_op = function
	| V.Op.ADD -> "+"
	| SUB -> "-"
	| MUL -> "*"
	| DIV -> "-"
	| MOD -> " % "
	| EQ -> "="
	| NE -> "!="
	| LE -> "<"
	| OR -> "||"
	| AND -> "&&"
	| SHL -> "<<"
	| SHR -> ">>"
	| BAND -> "&"
	| BOR -> "|"
  | XOR -> "^"
	| MAPSTO -> "-:"
  | MIN -> " MIN "
  | MAX -> " MAX "
	| DUMMY -> "^^"
;;

let sb = function true -> "True" | _ -> "False";;

let freshv = ref 0 ;;
let fresh_var () =
  let fv = "FRESH_" ^ (string_of_int !freshv) in 
  freshv := !freshv+1;
  fv
;;

let is_not_removable nm =
  match nm with
    "assert"
  | "abort"
    | "malloc"
    | "__VERIFIER_nondet_int"
    | "__VERIFIER_assert"
    | "__assert_fail"
    (* | "reach_error" *)
    | "llvm.stacksave"
    | "llvm.stackrestore"
    -> false
  | _ -> true
;;


let is_a_ptr ?(arr_mode=false) ptrs vs = function
    E.VAR (s,attr) ->
     let b1 = S_E.exists (function E.VAR (sv,_) -> s=sv | _ -> false) ptrs in
     let b2 = List.mem E.PTR attr in
     let b3 = S_E.exists (function E.VAR (sv,attr') as v -> sv=s && E.is_ptr v  | _ -> false) vs in
     F.dbgf "SC_PTR" "%s -> b1:%a b2:%a b3:%a" s F.fstrB b1 F.fstrB b2 F.fstrB b3;
     let b = (b1 || b2 || b3) in
     b
  | _ -> false
;;

let corr_id v =
  let v' =
    if String.contains v '.' then
      String.map (function '.' -> '_' | '$' -> '_' | c -> c) v
    else if v = "length" then
      "length2"
    else
      v
  in
  if String.get v' 0 = '@' then
    "GLOBAL__" ^ String.sub v' 1 (String.length v' - 1)
  else
    v'
;;

let add_ret exp =
  C.(Let (dp, Ast.PVar "__RET__", `Mkref (`OInt 0), exp))
;;

let string_of_evar = function
    E.VAR (v, attr) ->
     begin
       try
         corr_id v
       with
         e -> F.pn v;
              raise e
     end
  | e -> raise (NotSupported ("string_of_evar:" ^ E.var_decode e))
;;

let string_of_tvar = function
    T.EXP e -> string_of_evar e
  | t -> raise (NotSupported ("string_of_tvar:" ^ T.toStr t))
;;

let rec str_lhs = function
  | C.(`OVar s) -> s
  | C.(`OInt i) -> string_of_int i
  | C.(`Tuple lhss) -> "(" ^ String.concat "," (List.map str_lhs lhss) ^ ")"
  | _ -> ""
;;

let rec str_patt = function
  | Ast.PVar s -> s
  | PTuple lhss -> "(" ^ String.concat "," (List.map str_patt lhss) ^ ")"
  | _ -> ""
;;

let rec str_exp = function
  | C.Unit _ -> "()"
  | Value (_,lhs) -> str_lhs lhs
  | Cond (_, _, exp1, exp2) -> "if(){\n" ^ str_exp exp1 ^ "}{\n" ^ str_exp exp2 ^ "}"
  | NCond (_,_, exp1, exp2) -> "if(){\n" ^ str_exp exp1 ^ "}{\n" ^ str_exp exp2 ^ "}"
  | Assign (_,s,lhs) -> s ^ ":=" ^ str_lhs lhs
  | Update (_,lhs1, lhs2, exp) -> ""
  | Let (_, patt, lhs, exp) -> "Let " ^ str_patt patt ^ " = " ^ str_lhs lhs ^ " in\n" ^ str_exp exp
  | Seq (_, exp1, exp2) -> str_exp exp1 ^ ";\n" ^ str_exp exp2
  | _ -> ""
;;

let de_add = function
  | T.EXP (E.BINOP (e1, V.Op.ADD, e2)) -> e1, e2
  | T.EXP (E.VAR _ as e) -> e, E.CONST 0
  | T.NULL -> raise (Unexpected "null cannot be de_add")
  | e -> raise (Unexpected (T.fstr () e ^ " is unexpected"))
;;

let var_to_deref = function
    C.(`OVar e) -> C.(`ODeref e)
  | e -> e

let rec mk_full_empty_tuple st_name =
  print_endline ("STTT " ^ st_name);
  let (_, ls, _) = try List.find (fun (sn,_,_) -> sn=st_name) !structs with _ -> raise (Unexpected ("[3] Not found struct " ^ st_name)) in
  let ls' = List.map (fun (l,_) ->  
                if E.is_struct l && not (E.is_ptr l)then
                  C.(`Mkref (mk_full_empty_tuple (E.get_struct_name l)))
                else if E.is_struct l then
                  C.(`Mkref C.(`Mkref (mk_full_empty_tuple (E.get_struct_name l))))
                else if E.is_ptr l then
                  C.(`Mkref C.(`Mkref C.(`OInt 0)))
                else if E.is_array l then
                  begin
                    let sz = E.get_array_length l |> List.hd |> (function E.CONST i -> i | _ -> failwith "Only constant size array is supported") in
                    let sz' = C.(`OInt sz) in
                    C.(`Mkref C.(`MkArray sz'))
                  end
                else
                  C.(`Mkref C.(`OInt 0))
              ) ls in
  if List.length ls' > 1 then
    `Tuple ls'
  else if List.length ls' = 1 then
    try List.hd ls'
    with _ ->
      raise (Unexpected ("0 (a) fields of " ^ st_name))
  else
    C.(`Mkref C.(`OInt 0))
    (* raise (Unexpected ("0 (b) fields of " ^ st_name)) *)

       
let rec mk_empty_tuple st_name =
  let (_, ls, _) = try List.find (fun (sn,_,_) -> sn=st_name) !structs with _ -> raise (Unexpected ("[3] Not found struct " ^ st_name)) in
  let ls' = List.map (fun (l,_) ->
                
                if E.is_struct l && not (E.is_ptr l)then
                  C.(`Mkref (mk_empty_tuple (E.get_struct_name l)))
                else
                  C.(`Mkref C.(`OInt 0))
              ) ls in
  if List.length ls' > 1 then
    `Tuple ls'
  else if List.length ls' = 1 then
    try List.hd ls'
    with _ ->
      raise (Unexpected ("0 (a) fields of " ^ st_name))
  else
    raise (Unexpected ("0 (b) fields of " ^ st_name))

       
let rec exp_to_lhs ?(arg_mode=false) ?(arr_mode=false) ptrs vs e : C.lhs =
  match e with
    E.NOTHING ->
     raise (NotSupported "Nothing")
  | NEGINF ->
     C.(`OInt (Int.min_int))
  | POSINF ->
     C.(`OInt (Int.max_int))
  | UNDEF ->
  (* C.(`Mkref (mk_empty_tuple st_name)) *)
  (* C.(`Nondet (Some RefinementTypes.Top)) *)
  raise (NotSupported "Undef")
  | VAR (s, _) ->
     if arg_mode then
       C.(`OVar (corr_id s))
     else
       if arr_mode then
         C.(`OVar (corr_id s))
       else
         if is_a_ptr ptrs vs e then
           begin
             C.(`ODeref (corr_id s))
           end
         else
           begin
             C.(`OVar (corr_id s))
           end
  | CONST (n) ->
     C.(`OInt n)
  | FLOAT (f) ->
     C.(`OInt (int_of_float f))
  | BINOP (e1, op, e2) ->
     let e1' = exp_to_lhs ptrs vs e1 in
     let op' = string_of_op op in
     let e2' = exp_to_lhs ptrs vs e2 in
     C.(`BinOp (e1', op', e2'))
  | INDICES (_) ->
     raise (NotSupported "Indices")
  | ADDR (a) ->
     begin
       let n = exp_to_lhs ptrs vs a in
       (* match n with
         C.(`ODeref n') -> C.(`OVar n')
       | _ -> raise (NotSupported "Not Good Addr") *)
       n
     end
  | REF (r) ->
     C.(`Mkref (exp_to_lhs ~arr_mode:false ptrs vs r))     
  | NEG (n) ->
     begin
       match exp_to_lhs ptrs vs n with
         C.(`OInt n') -> C.(`OInt (-n'))
       | _ -> raise (NotSupported "Neg")
     end
  | STRING (s) ->
     C.(`ODeref s) 
  | ARROW (_, _) ->
     raise (NotSupported "Arrow")
  | LBL (_, _) ->
     raise (NotSupported "LBL")
  | FCALL (fn, args) ->
     let args' = List.map (exp_to_lhs ~arg_mode:true ~arr_mode:arr_mode  ptrs vs) args in
     C.(`Call (fn, 0, args'))
  | OFFSET (_, _) ->
     raise (NotSupported "Offset")
  | SIZEOF (s) ->
     if E.is_simple_type s then
       C.(`OInt (E.simple_size s))
     else
       let (_,fields,_) = List.find (fun (sn,_,_) -> sn=s) !structs in
       C.(`OInt (List.length fields))
;;

let term_to_lhs ?(arg_mode=false) ?(arr_mode=false) ptrs vs = function
    T.NULL ->
     C.(`Null)
  | T.EXP e ->
     exp_to_lhs ~arg_mode:arg_mode ~arr_mode:arr_mode ptrs vs e
;;

let rec bexp_to_lhs ptrs vs = function
    B.UNIT (t1, op, t2) ->
     let t1' = term_to_lhs ptrs vs t1 in
     let op' = string_of_op op in
     let t2' = term_to_lhs ptrs vs t2 in
     C.(`BinOp (t1', op', t2'))
  | OP (b1, op, b2) ->
     let b1' = bexp_to_lhs ptrs vs b1 in
     let op' = string_of_op op in
     let b2' = bexp_to_lhs ptrs vs b2 in
     C.(`BinOp (b1', op', b2'))
  | _ -> raise (NotSupported "bexp_to_lhs")
;;

let term_to_imm_op vs = function
    T.NULL -> C.IInt 0
  | T.EXP (E.CONST i) -> C.IInt i
  | T.EXP ((E.VAR (s,_) as e)) ->
     if S_E.mem e vs then
       C.IVar ("*" ^ s)
     else
       C.IVar s
  | _ -> raise (NotSupported "term_to_imm_op")
;;

let read_tuple ptrs vs y' a b f =
  (** let (_, a, _) = b in *)
  let st_name = E.get_struct_name (T.toExp b) in
  F.dbgf "scLOOKUP" "st_name: %s, f:%s" st_name f;
  let (_, ls, _) = try List.find (fun (sn,_,_) -> sn=st_name) !structs with _ -> raise (Unexpected ("[1] Not found struct " ^ st_name)) in
  F.dbgf "scLOOKUP" "fields: %a" (F.fstrL E.fstr ",") (List.map fst ls);
  let sa = E.var_decode a in
  let frv = fresh_var () in
  let tp = List.map (fun (fld,_) -> if E.var_decode fld = f then Ast.PVar frv else Ast.PVar "_") ls in
  let fld,_ = List.find (fun (fld,_) -> E.var_decode fld = f) ls in
  let is_fld_struct = if E.is_struct fld then true else false in
  let b' =
    match term_to_lhs ptrs vs b with
      C.(`ODeref _) as b' -> b'
    | C.(`OVar s) -> C.(`ODeref s)
    | _ -> raise (NotSupported "Read Tuple")
  in
  let decl, prog =
    if is_fld_struct then
      S_E.add a S_E.empty, C.Let (dp, Ast.PVar sa, C.(`ODeref frv),y')
    else
      S_E.empty, C.Seq (ldp, C.Assign (dp, corr_id sa, C.(`ODeref frv)),y')
  in
  
  if List.length tp > 1 then
    decl, C.Let (dp, Ast.PTuple tp, b', prog)
  else
    decl, C.Let (dp, List.hd tp, b', prog)
;;

(* OLD let write_tuple ptrs vs y' a f b =
(** let a = ref (0, 0) in
    let (x, y) = *a in
    a := (x, 100);
    let (b, c) = *a in
 *)
  let st_name = E.get_struct_name (T.toExp a) in
  let (_, ls, _) = try List.find (fun (sn,_,_) -> sn=st_name) !structs with _ ->
                     raise (Unexpected ("[2] Not found struct " ^ st_name)) in
  if List.length ls > 1 then
    let lhs, rhs = List.map (fun (fld,_) ->
                       if E.var_decode fld = f then
                         Ast.(PVar "_"), term_to_lhs ptrs vs b
                       else
                         let v = fresh_var () in
                         Ast.(PVar v), C.(`OVar v)
                     ) ls |> List.split in
    let a' = term_to_lhs ptrs vs a in
    C.Let (dp, Ast.PTuple lhs, a', C.Seq(ldp, C.Assign (dp, E.var_decode @@ T.toExp a, C.(`Tuple rhs)),y'))
  else
    C.Seq(ldp, C.Assign (dp, E.var_decode @@ T.toExp a, term_to_lhs ptrs vs b),y')
;; *)

let write_tuple ptrs vs y' a f b =
(** let a = ref (ref 0, ref 0) in
    let (x, y) = *a in
    y := 100
 *)
  let st_name = E.get_struct_name (T.toExp a) in
  let (_, ls, _) = try List.find (fun (sn,_,_) -> sn=st_name) !structs with _ ->
                     raise (Unexpected ("[2] Not found struct " ^ st_name)) in
  if List.length ls > 1 then
    let v = fresh_var () in
    let lhs = List.map (fun (fld,_) ->
                       if E.var_decode fld <> f then
                         Ast.(PVar "_")
                       else
                         Ast.(PVar v)
                     ) ls in
    let a' = term_to_lhs ptrs vs a in
    C.Let (dp, Ast.PTuple lhs, a', C.Seq(ldp, C.Assign (dp, corr_id v, term_to_lhs ptrs vs b),y'))
  else
    C.Seq (ldp, C.Assign (dp, corr_id @@ E.var_decode @@ T.toExp a, term_to_lhs ptrs vs b),y')
;;


let bexp_to_relation vs = function
    B.UNIT (t1, op, t2) ->
    C.{rop1= term_to_imm_op vs t1;cond=string_of_op op; rop2= term_to_imm_op vs t2}
  | _ -> raise (NotSupported "term_to_imm_op")
;;

let formula_to_bexp = function
  | (_, b::_, _, _)::_ -> b
  | _ -> B._T
;;

let rec to_lhs (b : [ `Var of string | `BinOp of C.lhs * string * C.lhs | `Nondet]) : C.lhs =
  match b with
    C.(`Var s) -> `OVar s
  | `BinOp (b1, op, b2) -> `BinOp (b1, op, b2)
  | `Nondet -> raise (NotSupported "Nondeterministism")
;;



let rec bexp_to_ifcond ptrs vs b : [ `Var of string | `BinOp of C.lhs * string * C.lhs | `Nondet] =
  match b with
  | B.UNIT (T.EXP e1, V.Op.EQ, T.EXP (E.CONST 0)) when E.is_ptr e1 ->
     C.(`BinOp (exp_to_lhs ~arr_mode:true ptrs vs e1, string_of_op V.Op.EQ, C.(`Null)))
  | B.UNIT (T.EXP e1, V.Op.NE, T.EXP (E.CONST 0)) when E.is_ptr e1 ->
     C.(`BinOp (exp_to_lhs ~arr_mode:true ptrs vs e1, string_of_op V.Op.NE, C.(`Null)))
  | B.UNIT (t1, op, t2) ->
     C.(`BinOp (term_to_lhs ptrs vs t1, string_of_op op, term_to_lhs ptrs vs t2))
  | B.OP (b1, V.Op.AND, b2) ->
     let l1  = bexp_to_ifcond ptrs vs b1 in
     let l1' = to_lhs l1 in
     let l2  = bexp_to_ifcond ptrs vs b2 in
     let l2' = to_lhs l2 in
     C.(`BinOp (l1', "&&", l2'))
  | B.OP (B.UNIT (a, V.Op.LE, b), V.Op.OR, B.UNIT (a', V.Op.EQ, b'))
    | B.OP (B.UNIT (a, V.Op.EQ, b), V.Op.OR, B.UNIT (a', V.Op.LE, b')) when a=a' && b=b' ->
     C.(`BinOp (term_to_lhs ptrs vs a, "<=", term_to_lhs ptrs vs b))
  | B.OP (b1, V.Op.OR, b2) ->
     let l1  = bexp_to_ifcond ptrs vs b1 in
     let l1' = to_lhs l1 in
     let l2  = bexp_to_ifcond ptrs vs b2 in
     let l2' = to_lhs l2 in
     C.(`BinOp (l1', "||", l2'))
  | B.OP (b1, V.Op.XOR, b2) -> (* x ^ y = !x&y || x&!y *)
     let b1c = B.complement b1 in
     let b2c = B.complement b2 in
     let l1  = bexp_to_ifcond ptrs vs b1 in
     let l1' = to_lhs l1 in
     let l2  = bexp_to_ifcond ptrs vs b2 in
     let l2' = to_lhs l2 in
     let l1c  = bexp_to_ifcond ptrs vs b1c in
     let l1'c = to_lhs l1c in
     let l2c  = bexp_to_ifcond ptrs vs b2c in
     let l2'c = to_lhs l2c in
     C.(`BinOp (`BinOp (l1'c,"&&",l2'), "||", `BinOp (l1',"&&",l2'c)))
  | _ ->
     B.pprint b; F.pn "";
     raise (NotSupported "bexp_to_ifcond (1)")
;;

let rec mk_cond ptrs vs thenbody elsebody cond =
  match cond with
  | B.UNIT(T.EXP a1, V.Op.EQ, a2) when a2=T.NULL || (E.is_ptr a1 && a2=T.EXP (E.CONST 0)) ->
     let s = E.toStr a1 in
     C.NCond (dp, s, thenbody, elsebody)
  | B.UNIT(T.EXP a1, V.Op.NE, a2) when a2=T.NULL || (E.is_ptr a1 && a2=T.EXP (E.CONST 0)) ->
     let s = E.toStr a1 in
     C.NCond (dp, s, elsebody, thenbody)
  | B.UNIT _ ->
    C.Cond (dp, bexp_to_ifcond ptrs vs cond, thenbody, elsebody)
  | B.OP (b1, V.Op.AND, b2) ->
     C.Cond (dp, bexp_to_ifcond ptrs vs b1, mk_cond ptrs vs thenbody elsebody b2, elsebody)
  | B.OP (B.UNIT (a, V.Op.LE, b), V.Op.OR, B.UNIT (a', V.Op.EQ, b'))
    | B.OP (B.UNIT (a, V.Op.EQ, b), V.Op.OR, B.UNIT (a', V.Op.LE, b')) when a=a' && b=b' ->
     C.Cond (dp, bexp_to_ifcond ptrs vs cond, thenbody, elsebody)
  | B.OP (b1, V.Op.OR, b2) ->
     C.Cond (dp, bexp_to_ifcond ptrs vs b1, thenbody, mk_cond ptrs vs thenbody elsebody b2)
  | B.OP (b1, V.Op.XOR, b2) ->
     let c1 = bexp_to_ifcond ptrs vs b1 in
     let c2 = bexp_to_ifcond ptrs vs b2 in
     (* if c1 then
          (if c2 then F else T)
        else
          (if c2 then T else F)  *)
     let cp1 = C.Cond (dp, c2, elsebody, thenbody) in
     let cp2 = C.Cond (dp, c2, thenbody, elsebody) in
     C.Cond (dp, c1, cp1, cp2)
  | _ ->
     B.pprint cond; F.pn "";
     raise (NotSupported "bexp_to_ifcond (2)")
;;

let is_declared declared a = S_E.mem a declared ;;

let subs_str u t s =
  if u=s then t else s
;;

let rec subs_lhs u t l =
  match l with
  | C.(`OVar s) -> `OVar (subs_str u t s) 
  | `OInt _ -> l
  | `ODeref s -> `ODeref (subs_str u t s)
  | `Nondet _ -> l
  | `BinOp (l, o, r) -> `BinOp (subs_lhs u t l, o, subs_lhs u t r)
  | `Null -> l
  | `OBool _ -> l
  | `Mkref l -> `Mkref (subs_lhs u t l)
  | `MkArray l -> `MkArray (subs_lhs u t l)
  | `Call (s, i, ls) -> `Call (s, i, List.map (subs_lhs u t) ls)
  | `Tuple (ls) -> `Tuple (List.map (subs_lhs u t) ls)
  | `Read (l, r) -> `Read (subs_lhs u t l, subs_lhs u t r)
  | `LengthOf l -> `LengthOf (subs_lhs u t l)
;;

let subs_lhs2 u t l =
  match l with
  | C.(`Var s) -> `Var (subs_str u t s) 
  | `BinOp (l, o, r) -> `BinOp (subs_lhs u t l, o, subs_lhs u t r)
  | _ -> l
;;

let subs_rel u t r =
  let subs_imm_op u t o =
    match o with
      C.IVar s -> C.IVar (subs_str u t s)
    | IInt _ -> o
  in
  {C.rop1 = subs_imm_op u t r.C.rop1;
   cond = r.C.cond;
   rop2 = subs_imm_op u t r.C.rop2
  }
;;

let rec subs_patt u t p =
  match p with
    Ast.PVar s -> Ast.PVar (subs_str u t s)
  | PTuple patts -> PTuple (List.map (subs_patt u t) patts)
  | PNone -> p
;;
    
let rec subs_exp s t e =
  let sl = subs_lhs s t in
  let se = subs_exp s t in
  let sl2 = subs_lhs2 s t in
  let ss = subs_str s t in
  match e with
    C.Unit _ -> e
  | Value (pos, x) ->
     let x' = sl x in
     Value (pos, x')
  | Cond (pos, cond, l, r) ->
     let cond' = sl2 cond in
     let l' = se l in
     let r' = se r in
     Cond (pos, cond', l', r')
  | NCond (pos, str, l, r) ->
     let str' = ss str in
     let l' = se l in
     let r' = se r in
     NCond (pos, str', l', r')
  | Assign (pos, str, l) ->
     let str' = corr_id @@ ss str in
     let l' = sl l in
     Assign (pos, str', l')
  | Update (pos, l1, l2, l3) ->
     let l1' = sl l1 in
     let l2' = sl l2 in
     let l3' = sl l3 in
     Update (pos, l1', l2', l3')
  | Alias (pos, str, a) ->
     let str' = ss str in
     Alias (pos, str', a)
  | EAnnot (pos, annots) -> raise (NotSupported "yet") 
  | Assert (pos, relation) ->
       Assert (pos, subs_rel s t relation)
  | Let (pos, patt, l, e) ->
     Let (pos, subs_patt s t patt, sl l, se e)
  | Seq (pos, l, r) ->
     Seq (pos, se l, se r)
;;

let rec unshadow a t e =
  match e with
    C.Unit _ -> e
  | Value _ -> e
  | Cond (pos, cond, l, r) ->
     Cond (pos, cond, unshadow a t l, unshadow a t r)
  | NCond (pos, str, l, r) ->
     NCond (pos, str, unshadow a t l, unshadow a t r)
  | Assign _
    | Update _
    | Alias _
    | EAnnot _
    | Assert _
    ->
     e
  | Let (pos, patt, l, e) ->
     begin
       match patt with
         Ast.PVar s when s = a ->
          Let (pos, Ast.PVar t, l, subs_exp s t e)
       | _ ->
          Let (pos, patt, l, unshadow a t e)
     end
  | Seq (pos, l, r) ->
     Seq (pos, unshadow a t l, unshadow a t r)
;;

(* let fix_shadowing declared a y =
  (* if S_E.mem a declared then
    let u = (E.toStr a) in
    let t = "_" ^ u in
    let et = E.VAR (t, E.get_attributes a) in 
    unshadow u t y, S_E.add et declared
  else
    y, S_E.add a declared *)
  y, declared
;; *)

let rec declare_and_initialize declared' ptrs vs y a len init_data =
  if is_declared declared' a then
    begin
      y
    end
  else 
    begin
      let rec init_data_to_patt a = function
          S.INIT_E -> Ast.PVar (a)
        | S.INIT_S _ -> Ast.PVar (a)
        | S.INIT_M ins ->
           let ins' = List.mapi (fun i init -> init_data_to_patt (a ^ "." ^ string_of_int i) init) ins in
           Ast.PTuple ins'
      in
      let rec init_data_to_tuple ptrs = function
          S.INIT_E -> C.(`OInt 0)
        | S.INIT_S t -> term_to_lhs ptrs vs t
        | S.INIT_M ins ->
           let ins' = List.map (init_data_to_tuple ptrs) ins in
           C.(`Tuple ins')
      in
      
      (* let y, declared =
        fix_shadowing declared' a y'
      in *)
      
      let is_struct = E.is_struct a in
      let is_array = E.is_array a in
      let y'' =
        match is_struct, is_array with
          false, false ->
           F.dbgf "scDECL" "Both Struct and Array";
           begin
             match len, init_data with
             | [], S.INIT_E ->
                if is_a_ptr ptrs vs a then
                  C.Let (dp,
                         Ast.PVar (string_of_evar a),
                         C.(`Mkref (C.(`OInt 0))),
                         y
                    )
                else
                  C.Let (dp,
                         Ast.PVar (string_of_evar a),
                         (C.(`OInt 0)),
                         y
                    )
             | [], S.INIT_S t ->
                if is_a_ptr ptrs vs a then
                  C.Let (dp,
                         Ast.PVar (string_of_evar a),
                         C.(`Mkref (term_to_lhs ptrs vs t)),
                         y
                    )  
                else
                  C.Let (dp,
                         Ast.PVar (string_of_evar a),
                         term_to_lhs ptrs vs t,
                         y
                    )  
             | E.CONST n::_, S.INIT_E when n=0 || n=1 -> 
                C.Let (dp,
                       Ast.PVar (string_of_evar a),
                       C.(`OInt 0),
                       y
                  )
             | E.CONST n::_, S.INIT_S t when n=0 || n=1 ->
                C.Let (dp,
                       Ast.PVar (string_of_evar a),
                       term_to_lhs ptrs vs t,
                       y
                  )
             | _ -> 
                raise (Supported "declare and initialize (mistake perhaps)")
           end
        | true, false -> (** TODO: Double Check *)
           F.dbgf "scDECL" "Only Struct";
           begin
             match init_data with
               S.INIT_E ->
                F.dbgf "scDECL" "InitE";
                let lhs = init_data_to_patt (string_of_evar a) init_data in
                F.dbgf "scDECL" "InitE lhs";
                let st_name = E.get_struct_name a in
                F.dbgf "scDECL" "InitE Struct:%s" st_name;
                let rhs = try C.(`Mkref (mk_empty_tuple st_name))
                          with Unexpected s as e ->
                                F.dbgf "scDECL" "Exception:%s" s;
                                raise e
                             | F.StError s as e ->
                                F.dbgf "scDECL" "Exception: %s" s;
                                raise e
                             | Not_found as e ->
                                F.dbgf "scDECL" "Not found ";
                                raise e
                             | e ->
                                F.dbgf "scDECL" "Exception: Other kinds";
                                raise e
                in
                F.dbgf "scDECL" "InitE rhs";
                C.Let (dp,
                       lhs,
                       rhs,
                       y
               )
             | S.INIT_S src when E.is_struct @@ T.toExp src ->
                F.dbgf "scDECL" "InitS";
                let src_name = E.get_struct_name @@ T.toExp src in
                let dest_name = E.get_struct_name a in
                if dest_name <> src_name then 
                  let (_, flds, _) = List.find (fun (a, _, _) -> a=src_name) !structs in
                  let lhs_dest = List.map (fun (fld,_) ->
                                    if E.is_struct fld && E.get_struct_name fld = dest_name then
                                      Ast.PVar (string_of_evar a)
                                    else
                                      Ast.PVar "_"
                                  ) flds in
                  C.Let (dp,
                         Ast.PTuple lhs_dest,
                         C.(`ODeref (string_of_evar @@ T.toExp src)),
                         (* term_to_lhs ptrs vs src, *)
                         y
                    )
                else
                  C.Let (dp,
                    init_data_to_patt (string_of_evar a) init_data,
                    init_data_to_tuple ptrs init_data,
                    y
               )
             | _ ->
                F.dbgf "scDECL" "Others";
                C.Let (dp,
                       init_data_to_patt (string_of_evar a) init_data,
                       init_data_to_tuple ptrs init_data,
                       y
                  )
           (* else
             let st_name = E.get_struct_name a in
             let (_, fields,_) = List.find (fun (sn,_,_) -> sn=st_name) !structs in
             
             C.Let (dp,
                  Ast.PVar (string_of_evar a),
                  C.(`MkArray (exp_to_lhs ptrs vs @@ (E.CONST (List.length fields)))),
                  y
             ) *)
           end
        | false, true -> (** TODO: Multidimentional array support *)
           F.dbgf "scDECL" "Only Array";
           C.Let (dp,
                  Ast.PVar (string_of_evar a),
                  C.(`MkArray (exp_to_lhs ptrs vs @@ List.hd len)),
                  y
             )
        | true, true ->
           begin
             let actuallen = List.hd len in
             let st_name = E.get_struct_name a in
             match actuallen with
               E.CONST i_len ->
                F.dbgf "STAR" "len: %d" i_len ;
               begin match init_data with
                 S.INIT_E ->
                  let rec iterate acc i =
                    if i>=i_len then
                      acc
                    else
                      let fv = fresh_var () in
                      let lhs = init_data_to_patt fv init_data in
                      let rhs = C.(`Mkref (mk_empty_tuple st_name)) in
                      iterate ((C.(`OVar fv), (lhs, rhs))::acc) (i+1)
                  in
                  let st_data = iterate [] 0 in
                  let refs, ref_data = List.split st_data in
                  let lhs = init_data_to_patt (string_of_evar a) init_data in
                  let arr = C.Let (dp, lhs, C.(`Mkref (`Tuple refs)), y) in
                  let p = List.fold_left (fun p (a,b) -> C.Let (dp, a, b, p)) arr ref_data in
                  F.dbgf "scDECL" "Struct and Array";
                  p
               | _ -> raise (NotSupported "Others [1]")
               end
           | _ ->
              raise (NotSupported "Others [2]")
           end
      in

      y'' (* , declared *)
    end
;;

let f_counter = ref 0 ;;
let new_name () =
  f_counter := !f_counter + 1;
  "f" ^ string_of_int !f_counter 
;;

let rec join_at_end p p' =
  match p' with
    C.Unit _ -> p
  | Value _ 
    | Cond _
    | NCond _
    | Assign _
    | Update _
    | Alias _
    | Assert _
    | EAnnot _
    -> C.Seq (ldp, p', p)     
  | Let (pos, patt, lhs, exp) ->
     C.Let (pos, patt, lhs, join_at_end p exp)
  | Seq (pos, exp1, exp2) ->
     Seq (pos, exp1, join_at_end p exp2)

let var_of_assignee = function
  | S.ASSIGN (v, _, _, _) -> v
  | S.DECL (v, _, _, _, _) -> v
  | _ -> raise (NotSupported "Assignee")

let to_simple ptrs vs l =
  match l with
    T.EXP ((E.VAR _) as l') ->
     if is_a_ptr ptrs vs l' then
       begin
         let f = fresh_var () in
         T.EXP (E.VAR (f,[])), [(f, C.(`ODeref (string_of_tvar l)))]
       end
     else
       l, []
  | T.EXP ((E.CONST n)) ->
     l, []
  | _ ->
     let f = fresh_var () in
     T.EXP (E.VAR (f,[])), [(f, C.(`OVar (string_of_tvar l)))]
     
let rec mk_assert ptrs vs y b =
  let lhs_b = bexp_to_ifcond ptrs vs b in
  let true_rel = C.{rop1=IInt 0;cond="="; rop2= IInt 0} in
  let false_rel = C.{rop1=IInt 0;cond="="; rop2= IInt 1} in
        
  C.Cond (dp, lhs_b,
          C.Seq (ldp, C.Assert (dp, true_rel), y),
          C.Seq (ldp, C.Assert (dp, false_rel), y))
;;

let print_vars b vs =
  F.pn "-------";
  S.pprint 2 b; 
  S_E.iter (fun v -> E.print v; F.pn "") vs;
  F.pn "";;

let enptr a vs =
  F.dbgf "SC_PTR" "Upgraded to PTR: %a" E.fstr a;
  let a' = E.var_add E.PTR a in
  S_E.add a' (S_E.remove a vs)
;;

let rec body_to_cexp ((dep_map, gvs, (vs:S_E.t)) as gvars) prog =
  
  match prog with
  | S.SKIP -> (S_E.empty, S_E.empty, [], C.Unit (dp))
  | ASSIGN (a, b, y, l) ->
     begin
       F.dbgf "SC_PTR" "ASSIGN a";
       let vs' = enptr a vs in
       let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, vs') y in
       F.pn_s "scCUR" "*** Code ***"; 
       F.pf_s "scCUR" (S.solo_print 0) prog;
       F.dbgf "scCUR" "ASSIGN Begin";
       match b with
         T.EXP (E.BINOP (bh, V.Op.ADD, E.CONST idx)) when E.is_array bh ->
          F.dbgf "scASS" "Assign: %a, %d" E.fstr bh idx; 
          let arr_len = E.get_array_length bh |> List.hd |> E.get_int l in
          let fv = fresh_var () in
          let rec iterate acc i =
            if i>= arr_len then
              acc
            else
              let s = if i=idx then Ast.PVar fv else Ast.PVar "_" in
              iterate (s::acc) (i+1) in
          let lhs = Ast.PTuple (iterate [] 0 |> List.rev) in
          let p   = C.Let (dp, lhs, C.(`ODeref (E.toStr bh)), C.Seq (ldp, C.Assign (dp, corr_id @@ E.var_decode a, C.(`ODeref fv)), y')) in
          F.dbgf "scCUR" "ASSIGN End";
          (ptrs,
           declared,
           ry,
           p
          )
       | _ ->
          let sa = try string_of_evar a with e -> F.dbgf "SC" "Exception in Assing"; raise e in
          let lb = term_to_lhs ptrs vs b in
          F.dbgf "SC_PTR" "ASSIGN b";
          let ptrs' = enptr a ptrs in
          let ptrs'' = match lb with `ODeref x ->
                                      F.dbgf "SC_PTR" "ASSIGN c";
                                      enptr (T.toExp b) ptrs' | _ -> ptrs' in
          let p = C.Seq (ldp, C.Assign (dp, corr_id sa, lb), y') in
          F.dbgf "scCur" "ASSIGN End";
          (ptrs'',
           declared,
           ry,
           p
          )
     end
  | ASSERT (a, y, l) ->
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     
     let formula = ((fun (_, b, _, _) -> B.list_to_bexp b) (List.hd a)) in
     let b = mk_assert ptrs vs y' formula in
     
     (ptrs,
      declared,
      ry,
      b)
  | IF (B.OP(b1, V.Op.AND, b2), b, c, y, l) ->
     let p' = S.IF (b1, IF (b2, b, c, S.SKIP, l), c, y, l) in
     body_to_cexp gvars p'
  | IF (B.OP(b1, V.Op.OR, b2), b, c, y, l) ->
     let p' = S.IF (b1, b, IF (b2, b, c, S.SKIP, l), y, l) in
     body_to_cexp gvars p'
  | IF (a, b, c, y, l) ->
     begin
     F.dbgf "scIF" "IF Begins of %a" B.fstr a;
     let modv_b, _ = S.mod_free_var b in
     let modv_c, _ = S.mod_free_var c in
     let _, freev_y = S.mod_free_var y in
     let comv = S.S.inter (S.S.union modv_b modv_c) freev_y in
     let comv' = S_E.map (E.var_add E.PTR) comv in
     let vs' = S_E.map (fun v -> if S_E.mem v comv then E.var_add E.PTR v else v) vs in
     let gvars' = (dep_map, gvs, vs') in
     let ptrs3, declared1, ry, y' = body_to_cexp gvars' y in
     let gvars'' = (dep_map, gvs, S_E.union (S_E.union vs comv') ptrs3) in
     let ptrs1, declared2, rb, b' = body_to_cexp gvars'' b in
     let ptrs2, declared3, rc, c' = body_to_cexp gvars'' c in
     
     let ptrs = S_E.union (S_E.union (S_E.union ptrs1 ptrs2) ptrs3) comv' in
     let declared = S_E.union (S_E.union declared1 declared2) declared3 in
     let p =
        C.Seq (ldp,
                    mk_cond ptrs vs b' c' a,
                      (* C.Cond (dp, bexp_to_ifcond ptrs vs a, b', c'), *)
                    y'
          ) in
     F.dbgf "scIF" "IF is done of %a" B.fstr a;
     (ptrs,
      declared,
      rb @ rc @ ry,
      p)
     end
  | WHILE (a, bs, b, c, y, l) ->
     F.dbgf "scP" "WHILE Begins of %a" B.fstr a;
     let ptrs, declared, rw, w' = while_to_recf gvars a b y in
     F.dbgf "scP" "WHILE Begins of %a" B.fstr a;
     (ptrs, declared, rw, w')
  | DECL (a', _, S.INIT_E, PROCCALL (Some a, T.EXP (E.VAR ("__VERIFIER_nondet_int",_)), [], _,
                                     y,_), _) when a'=a ->
     let ptrs, declared, ry, y' = try body_to_cexp gvars y with Stack_overflow as e -> F.pn "*** STACK @ DECL ***"; raise e in
     F.pn_s "scCUR" "*** Code ***"; 
     F.pf_s "scCUR" (S.solo_print 0) prog;
     F.dbgf "scCUR" "DECL Begin";
     (* let y', declared = fix_shadowing declared a y' in *)
     let sa = string_of_evar a in
     let lb = C.(`Nondet (Some RefinementTypes.Top)) in
     let p = if is_a_ptr ptrs vs a then
               C.Let (dp, Ast.PVar sa, lb, y')
             else
               C.Let (dp, Ast.PVar sa, lb, y') in
     F.dbgf "scCUR" "DECL End";
     (ptrs,
      declared,
      ry,
      p
     )
  | PROCCALL (_, T.EXP (E.VAR ("llvm_stacksave",_)), [], _,
              y, _) ->
     body_to_cexp gvars y
  | PROCCALL (_, T.EXP (E.VAR ("llvm_stackrestore",_)), _, _,
              y, _) ->
     body_to_cexp gvars y
  | PROCCALL (_, T.EXP (E.VAR ("__VERIFIER_assert",_)), cond::_, _,
              y, _) ->
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     let a = B.UNIT (cond, V.Op.NE, T.zero) in
     (ptrs, declared, ry,
      C.Seq (ldp,
             C.Assert (dp, bexp_to_relation vs a),
             y'))
  | PROCCALL (_, T.EXP (E.VAR ("abort",_)), [], _,
              y, _) ->
     S_E.empty, S_E.empty, [], C.Assert (dp, bexp_to_relation vs (B.UNIT(T.zero, V.Op.NE, T.zero)))
  | PROCCALL (z, a, b, i, y, l) ->
     (* if not (List.exists (fun (pn,_,_) -> E.var_decode pn=T.toStr a) !procedures) then
       let ptrs, declared, ry, y' = body_to_cexp gvars y in
       match z with
         Some z' ->
         let sa = try Ast.PVar (string_of_evar z') with e -> F.dbgf "SC" "Exception in Assing"; raise e in
         let p   = C.Let (dp, sa, C.(`OInt 0), y') in
         ptrs, declared, ry, p
       | _ -> ptrs, declared, ry, y'
     else *)
     begin
       F.dbgf "scP" "%a begins" T.fstr a;
       let args_with_addr' = (List.map T.toExp b)
                    |> S_E.of_list
                    |> S_E.filter (function E.ADDR _ -> true | _ -> false) 
                    |> S_E.map (function E.ADDR x -> x | x -> x) in
       let args_with_addr = S_E.map (E.var_add E.PTR) args_with_addr' in
       let s_a = F.corr_filename @@ (E.var_decode @@ T.toExp a) in
       let simple_args' = S_E.filter (fun v -> not (S_E.mem v args_with_addr')) vs in
       let simple_args'' = match z with
           None -> simple_args'
         | Some x ->
            if List.exists (fun (pn,_,_) -> E.var_decode pn=s_a) !procedures then
              begin
                F.dbgf "SC_PTR" "PROCCALL a";
                enptr x simple_args'
              end
            else
              simple_args'
       in
       let vs'3 = S_E.union simple_args'' args_with_addr in
       let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, vs'3) y in
       let gvs = try DM.find (string_of_tvar a) dep_map with _ -> S_E.empty in
       let lgvs = to_list gvs in
       let lhs_gvs = List.map (exp_to_lhs ~arg_mode:true ptrs vs) lgvs in
       
       F.dbgf "scP" "*** 5 @ %s" s_a;
       try
         let (_, params, _) =
           try
             List.find (fun (pn,_,_) -> E.var_decode pn=s_a) !procedures
           with e ->
                 (* List.iter (fun (pn,_,_) -> F.pn @@ E.var_decode pn) !procedures; *)
                 if !skip_not_found_function then
                   (E.VAR (s_a,[]), [], S.SKIP)
                 else
                   begin
                     if !show_not_found_function then
                       F.pn ("Not found function: " ^ T.toStr a);
                     raise e
                   end
         in
         F.dbgf "scPROC" "*** 6 @ %a" T.fstr a;
         let vparams = List.map var_of_assignee params in
         F.dbgf "scPROC" "*** 6b @ %a" T.fstr a;
         let par_arg = try List.combine b vparams
                       with e ->
                             if E.is_vararg_func (T.toExp a) then
                               List.fold_left (fun (pas, prm) arg ->
                                   match prm with
                                     [] ->
                                      let fv = S.fresh_var [] in
                                      (arg,fv)::pas, []
                                   | x::xs ->
                                      (arg,x)::pas, xs 
                                 ) ([], vparams) b |> (fun (a,_) -> List.rev a)
                             else
                               begin F.iterS T.pprint "," b; F.pn ""; F.iterS E.pprint "," vparams; F.pn ""; raise e end
         in
         F.dbgf "scPROC" "*** 7 @ %a" T.fstr a;
         let args', es =
           List.fold_left (fun (args, es) (a,p) ->
               let e = term_to_lhs ptrs vs a in
               match a with
                 T.EXP (E.ADDR ae) ->
                  begin match E.is_ptr ae, E.is_ptr p with
                    false, true ->
                     if is_a_ptr ptrs vs ae then
                       let e = exp_to_lhs ~arg_mode:true ptrs vs ae in
                       e::args, es
                     else
                       let freshv = fresh_var () in
                       C.(`OVar freshv)::args, fun x -> C.(es (Let (dp, Ast.PVar freshv, `Mkref e, x)))
                  | false, false ->
                     var_to_deref e::args, es
                  | true, false ->
                     let freshv = fresh_var () in
                     C.(`ODeref freshv)::args, fun x -> C.(es (Let (dp, Ast.PVar freshv, var_to_deref e, x)))
                  | true, true ->
                     E.pprint ae; F.pn "";
                     raise (Supported "") end
               | _ ->   
                  let ae = T.toExp a in
                  match E.is_ptr ae, E.is_ptr p with
                  | true, true | false, false ->
                     e::args, es
                  | true, false ->
                     var_to_deref e::args, es
                  | false, true ->
                     let freshv = fresh_var () in
                     C.(`OVar freshv)::args, fun x -> C.(es (Let (dp, Ast.PVar freshv, `Mkref e, x)))
             ) ([], fun x -> x) par_arg in
         
         (* let args' = List.map (fun (a, p) ->
                         let e = term_to_lhs ptrs vs a in
                         let ae = T.toExp a in
                         match E.is_ptr ae, E.is_ptr p with
                         | true, true | false, false -> e
                         | true, false ->
                            var_to_deref e
                         | false, true ->
                            e
                         (* if not (E.is_ptr (T.toExp a)) && E.is_ptr p then
                           term_to_lhs ptrs vs a
                         else
                           term_to_lhs ~arg_mode:(E.is_ptr p) ptrs vs a *)
                       ) par_arg in *)
         F.dbgf "scPROC" "*** 9 @ %a" T.fstr a;
         let all_args = (List.rev args') @ lhs_gvs in
         let ptrs', p, declared =
           match z with
             None ->
              ptrs, C.(Seq (ldp,
                            Value (dp,
                                   `Call (string_of_tvar a, List.length b, all_args)),
                            y')), declared
           | Some z' ->
              S_E.add z' ptrs, C.(Seq (ldp, Assign (dp, corr_id @@ string_of_evar z', `Call (string_of_tvar a, List.length b, all_args)), y')), declared
         in
         F.dbgf "scP" "%a ends" T.fstr a;
         (S_E.union ptrs' args_with_addr,
          declared,
          ry,
          es p)
       with
         Not_found ->
          F.pn ("Not Found @ PROCCALL: " ^ s_a);
          (ptrs,
          declared,
          ry,
          y')
       | e ->
          F.pn "Other Exception";
          raise e
     end
  | MALLOC (a, size, y, l) ->
     if E.is_array a || (E.is_ptr a && not (E.is_struct a)) then
       begin
         let ptrs, declared, ry, y' = body_to_cexp gvars y in
         let s_size = exp_to_lhs ptrs vs size in
         let ptrs' = S_E.filter (fun x->not (E.var_decode x=E.var_decode a)) ptrs in
         (* let y', declared = fix_shadowing declared a y' in *)
         if E.is_ptr a then arrays := a::!arrays ;
         let p = C.Let (dp,
                        Ast.PVar (string_of_evar a),
                        C.(`Mkref (s_size)),
                        y') in
         (ptrs',
          S_E.add a declared,
          ry,
          p
         )
       end
     else if E.is_struct a then
       begin
         try
           let st_name = E.get_struct_name a in
           let tp' = C.(`Mkref (mk_full_empty_tuple st_name)) in
           let tp = if E.is_ptr a then C.(`Mkref tp') else tp' in 
           let ptrs, declared, ry, y' = body_to_cexp gvars y in
           let ptrs' = S_E.filter (fun x->not (E.var_decode x=E.var_decode a)) ptrs in
           (* let y', declared = fix_shadowing declared a y' in *)
           let p = C.Let (dp,
                          Ast.PVar (string_of_evar a),
                          tp,
                          y') in
           (ptrs',
            S_E.add a declared,
            ry,
            p
           )
         with
           _ ->
           F.pn "Unsupported Exception in Malloc";
           body_to_cexp gvars y
       end
     else
       begin
         F.p "Notsupported Malloc "; E.print a; F.pn ""; 
         (* raise (NotSupported "Malloc") *)
         body_to_cexp gvars y
       end
  | MUTATION (a, b, c, y, l) ->
     let ptrs1, declared, ry, y' = body_to_cexp gvars y in
     F.dbgf "scMUTATION" "Mutation begins of %a->%s = %a" T.fstr a b T.fstr c;
     let (pt, ind) = de_add a in
     let fvt = T.head "Mutation" a in
     let ptrs = S_E.add fvt ptrs1 in
     let prog =
       if E.is_array pt then
         if E.is_struct pt then
           begin
             S.pprint 2 (S.MUTATION (a, b, c, S.SKIP, l));
             raise (Supported "Mutation")
           end
         else
           let prog = C.Update (dp,
                                exp_to_lhs ~arr_mode:true ptrs vs pt,
                                exp_to_lhs ptrs vs ind,
                                term_to_lhs ptrs vs c) in
           C.Seq(ldp,
              prog,
              y')
       else
         if E.is_struct pt then (** pt->fld = exp; --> pt := () *)
           try
             write_tuple ptrs vs y' a b c
           with
             _ ->
             F.pn "Unsupported Exception in Mutation";
             y'
           (* C.Update (dp,
                     term_to_lhs ~arr_mode:true ptrs vs a,
                     exp_to_lhs ptrs vs (E.CONST (int_of_string b)),
                     term_to_lhs ptrs vs c) *)
         else
           if ind = E.CONST 0 then
             let prog = C.Assign (dp,
                       corr_id @@ string_of_evar pt,
                       term_to_lhs ptrs vs c
                          ) in
             C.Seq(ldp,
              prog,
              y')
           else
             let prog = C.Update (dp,
                       exp_to_lhs S_E.empty S_E.empty pt,
                       exp_to_lhs ptrs vs ind,
                       term_to_lhs ptrs vs c) in
             C.Seq(ldp,
              prog,
              y')
     in
     F.dbgf "scMUTATION" "Mutation ends of %a->%s = %a" T.fstr a b T.fstr c;
     (* if b = "*" then *)
       (ptrs,
        declared,
        ry,
        prog
       )
     (* else
       raise (Supported "Mutation") *)
  | LOOKUP (a, b, c, y, l) ->
     let (pt, ind) = de_add b in
     let vs' =  (* if E.is_struct pt then vs else *) enptr a vs in
     let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, vs') y in
     F.dbgf "scLOOKUP" "Lookup begins of %a = %a->%s" E.fstr a T.fstr b c;
     let decl, prog =
       if E.is_array pt then
         if E.is_struct pt then
           begin
             F.dbgf "scLOOKUP" "Array and Struct Case";
             S.pprint 2 (S.LOOKUP (a, b, c, S.SKIP, l));
             (* raise (Supported "Lookup: Array and Struct Case") TEMP *)
             S_E.empty, y'
           end
         else
           begin
             F.dbgf "scLOOKUP" "Array and no Struct Case: %a" E.fstr a;
             let rhs = C.(`Read (exp_to_lhs ~arr_mode:true ptrs vs pt, exp_to_lhs ptrs vs ind)) in
             S_E.empty, C.Seq (ldp, C.Assign (dp, corr_id @@ string_of_evar a, rhs), y')
           end
       else
         if E.is_struct pt then
           begin
             F.dbgf "scLOOKUP" "not Array and Struct Case";
             try
               read_tuple ptrs vs y' a b c
             with
               _ -> S_E.empty, y' 
           end
         else
           if ind = E.CONST 0 then
             let rhs = C.(`ODeref (string_of_evar pt)) in
             S_E.empty, C.Seq (ldp, C.Assign (dp, corr_id @@ string_of_evar a, rhs), y')
           else
             let rhs = C.(`Read (exp_to_lhs S_E.empty S_E.empty pt, exp_to_lhs ptrs vs ind)) in
             S_E.empty, C.Seq (ldp, C.Assign (dp, corr_id @@ string_of_evar a, rhs), y')
     in
     F.dbgf "scLOOKUP" "Lookup ends of %a = %a->%s" E.fstr a T.fstr b c;
     (S_E.add a ptrs,
      S_E.union decl declared,
      ry,
      prog
     )
  | BLOCK (a, y, l) ->
     begin
       let a' =
       match y with
         S.SKIP -> a
       | _ -> S.join_at_last y a
       in
       
       body_to_cexp gvars a'
     end
     
     (*begin
       match y with
         S.SKIP ->
          ptrs1, declared, ra, a'
       | _ ->
          let ptrs2, declared, ry, y' = body_to_cexp gvars y in
          let ptrs = S_E.union ptrs1 ptrs2 in
          (ptrs,
           declared,
           ry,
           C.Seq (ldp,
                  a',
                  y'))
     end *)
  | DECL (a, len, init_data, y, l) ->
     let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, S_E.add a vs) y in
     F.pn_s "scCUR" "*** Code ***"; 
     F.pf_s "scCUR" (S.solo_print 0) prog;
     F.dbgf "scCUR" "DECL Begin";
     let y'' (* , declared *) =
       try
         declare_and_initialize declared ptrs vs y' a len init_data
       with
         _ -> y'
     in
     F.dbgf "scCUR" "Decl ends of %a" E.fstr a;
     (ptrs,
      declared,
      ry,
      y''
     )
  | RETURN (i, _, l) ->
     let exp = 
       C.Value (dp, term_to_lhs S_E.empty vs i)
     in
     (S_E.empty, S_E.empty, [], exp)
  | DISPOSE (a1, y, l) ->
     let ptrs2, declared, ry, y' = body_to_cexp gvars y in
     ptrs2, declared, ry, y'
  (*| s ->
     S.pprint 0 s; 
     raise (NotSupported "Others2") *)

and while_to_recf (dep_map, gvs, vs) cond body rest = 
  let modv, freev' = S.mod_free_var body in
  let fvcond = B.fv cond in
  let freev = S.S.union freev' @@ S.S.of_list fvcond in
  
  let modv'' = S_E.map (function E.VAR (v, attr) -> E.VAR (v, E.PTR::attr) | e -> e) modv in
  let onlyfreev = S.S.diff freev modv in
  let freev' = S.S.elements freev in
  
  let args = List.map (fun v ->
                 let s' = try string_of_evar v with e -> F.dbgf "SC" "Exception in While(1)"; raise e in
                 C.(`OVar s')
               ) freev' in
  let params = try List.map string_of_evar freev' with e -> F.dbgf "SC" "Exception in While(2)"; raise e in
  let new_fn = "WHILE__" ^ (new_name ()) in
  let new_call = C.(`Call (new_fn, List.length args, args)) in
  let gvars' = (dep_map, gvs, S_E.union (S_E.union vs modv'') onlyfreev) in
  let ptrs0, _, fns, then_body' = try body_to_cexp gvars' body with Stack_overflow as e -> F.pn "*** STACK @ WHILE BODY ***"; raise e in
  let exp' = (C.Value (dp, new_call)) in

  let then_body = join_at_end exp' then_body' in
  let ptrs = S_E.union ptrs0 modv'' in
  let f_body = mk_cond ptrs vs then_body (C.Unit (dp)) cond in
  (* C.Cond (dp, bexp_to_ifcond ptrs vs cond, then_body, C.Unit (dp)) in *)
  let new_f = (new_fn, params, f_body) in
  
  let ptrs1, declared, fns1, body' = body_to_cexp gvars' rest in
    
  let ret = C.(Seq (ldp, exp', body')) in
  let ptrs' = S_E.union (S_E.union modv'' ptrs0) ptrs1 in
  let fn' = new_f::fns@fns1 in
  ptrs', declared, fn', ret 
;;


let string_of_globals = function
  | G.STATEMENT a -> C.(`OVar (var_of_assignee a |> string_of_evar))
  | _ -> raise (NotSupported "string of globals")

let func_to_fn dep_map (nm, params, body) =
  F.dbgf "scFUN" "*** *** *** \nBegin: %a" E.fstr nm;
  let snm = string_of_evar nm in
  let gvars' = try DM.find snm dep_map with Not_found -> F.dbgf "ERR" "Not found %s" snm; raise Not_found in
  let gvars = S_E.map (E.var_add E.PTR) gvars' in
  let vparams = List.map var_of_assignee params in
  F.pf_s "scFUN" (S.pprint 0) body; 
  let ptrs, _, fns, exp = try
      body_to_cexp (dep_map, gvars, S_E.union gvars (S_E.of_list vparams)) body
    with
      Stack_overflow as e -> F.pn "***!!!***"; raise e
    | e -> F.dbgf "ERR" "Exception in body translation"; raise e (* (NotSupported "body") *) in
  let sparams = List.map string_of_evar vparams in
  let l_glo_dep = to_list gvars |> List.map string_of_evar in
  F.dbgf "scFUN" "-- -- -- -- -- \nDone: %a" E.fstr nm;
  fns @ [(snm, sparams@l_glo_dep, exp)]
;;

let stmt_to_fn ptrs b = function
  | S.ASSIGN (v, t, _, _) ->
     let sv = string_of_evar v in
     let lhs = term_to_lhs S_E.empty S_E.empty  t in
     C.Let (dp, Ast.PVar sv, lhs, b)
  | S.DECL (v, len, init_data, _, _) ->
     let exp (*,_*) = declare_and_initialize S_E.empty ptrs S_E.empty b v len init_data in
     exp
  | _ -> raise (NotSupported "Stmt")

               (*
let rec slacC_to_prog dep_map acc = function
    [] -> acc
  | G.PROC ((_,_,S.SKIP),_,_,_,_)::xs
  | G.PROC ((_,_,S.BLOCK(S.SKIP,S.SKIP,_)),_,_,_,_)::xs
    ->
     slacC_to_prog dep_map acc xs
  | G.PROC (proc,_,_,_,_)::xs ->
     let prog = func_to_fn dep_map proc in
     slacC_to_prog dep_map (acc @ prog) xs
  | G.FFILE (st,_)::xs ->
     let fin = open_in st in
     let pd : G.t = Marshal.from_channel fin in
     close_in fin;
     slacC_to_prog dep_map acc (pd::xs)
  | _::xs -> slacC_to_prog dep_map acc xs
;; *)



       
let slacC_to_prog dep_map acc funs =
  let len = List.length funs in
  let progs : ('a*'b*'c) list ref = ref [] in
  for i=0 to len-1 do
    let proc = List.nth funs i in
    let prog = func_to_fn dep_map proc in
    progs := prog @ !progs
    (* let x = List.nth funs i in
    match x with
    | G.PROC ((_,_,S.SKIP),_,_,_,_)
      | G.PROC ((_,_,S.BLOCK(S.SKIP,S.SKIP,_)),_,_,_,_)
      ->
       ()
    | G.PROC (proc,_,_,_,_) ->
       let prog = func_to_fn dep_map proc in
       progs := prog @ !progs
    | G.FFILE (st,_) ->
       raise (Unexpected "FFILE")
    | _ -> () *)
  done;
  !progs
;;


let rec get_func_global_dep_fn progs dep_map fn body =
  let sfn = string_of_evar fn in
  F.pn ("DepMap | " ^ sfn);
  let sfn = string_of_evar fn in
  if DM.mem sfn dep_map then
    dep_map, DM.find sfn dep_map
  else
    begin
      let procs : S_S.t = S.get_func_call body |> S_S.filter (fun f -> is_not_removable f) in
      let _, fv = S.mod_free_var body in
      let gfv : S_E.t = S_E.filter E.is_global fv in
    
      let all_gfv, dep_map' =
        S_S.fold (fun proc ((fvs:S_E.t), _dep_map) ->
            try
              let (_fn, _, _body) = List.find (fun (fn,_,_) -> string_of_evar fn = proc) progs in 
              let _dep_map', fv = get_func_global_dep_fn progs dep_map _fn _body in
              S_E.union fvs fv, _dep_map'
            with
              Not_found ->
              S_E.empty, _dep_map
          ) procs (gfv, dep_map) in
      
      let dep_map'' = DM.add sfn all_gfv dep_map' in
      dep_map'', all_gfv
    end
;;

let rec get_func_global_dep progs dep_map = function
  | [] -> dep_map
  | (fn, params, S.SKIP)::xs
    | (fn, params, S.BLOCK (S.SKIP, S.SKIP, _))::xs ->
     get_func_global_dep progs dep_map xs
  | (fn, params, body)::xs ->
     let sfn = string_of_evar fn in
     
     if DM.mem sfn dep_map then
       get_func_global_dep progs dep_map xs
     else
       let dep_map', _ = get_func_global_dep_fn progs dep_map fn body in
       get_func_global_dep progs dep_map' xs
;;

module Pair = struct
  type t = int * string
  let compare (i,a) (j,b) = if i=j then String.compare a b else i-j
end
            
let rec get_func_global_dep3 progs dep_map glos =
  let funcs',all =
    List.fold_left
      (fun (acc,all) -> (function (_, _, S.SKIP) -> (acc,all)
                          | (_, _, S.BLOCK (S.SKIP, S.SKIP, _)) -> (acc,all)
                          | (fn, params, body) ->
                             let procs : S_S.t = S.get_func_call body |> S_S.filter (fun f -> is_not_removable f) in
                             let sfn = string_of_evar fn in
                             let _, fv = S.mod_free_var body in
                             let gfv : S_E.t = S_E.filter E.is_global fv in
                             
                             (M_S.add sfn (procs,gfv) acc, S_S.add sfn (S_S.union procs all))
      )) (M_S.empty,S_S.empty) glos in
  F.dbgf "SC-ARR" "|all|: %d, |funcs|: %d" (S_S.cardinal all) (M_S.cardinal funcs');
  let _,idx = S_S.fold (fun f (i,acc) -> (i+1,M_S.add f i acc)) all (0, M_S.empty) in 
  let module S_P = Set.Make(Pair) in
  let funcs : (S_P.t * S_E.t) M_S.t  =
    M_S.map
      (fun (procs,gfv) ->
        let procs' =
          S_S.fold
            (fun p acc ->
              try
                S_P.add (M_S.find p idx, p) acc
              with
                Not_found ->              
                acc
            ) procs S_P.empty in
        procs', gfv
      ) funcs' in
  
  let funcs' : (S_P.t * S_E.t) M_S.t =
    M_S.fold
      (fun _ _ (funcs': ((S_P.t * S_E.t)) M_S.t) ->
        let funcs'' :(S_P.t * S_E.t) M_S.t =
          M_S.map (fun ((procs : S_P.t), gfv) ->
              let procs', gfv' =
                S_P.fold (fun (_,p) (acc, gfv) ->
                    if M_S.mem p funcs then
                      let (procs, gfv') = M_S.find p funcs in 
                      S_P.union acc procs, S_E.union gfv gfv'
                    else
                      acc, gfv
                  ) procs (procs, gfv) in
              
              procs', gfv'
            ) funcs' in
        funcs''
      ) funcs funcs
  in
  F.pf_s "SC-ARR" (
  M_S.iter (fun f (procs,_) ->
      S_P.iter (fun (_,j) -> F.dbgf "SC-ARR" "%s --> %s" f j) procs
    )) funcs' ;
  let dep_map = M_S.map snd funcs' in
  dep_map ;;
  
let rec get_func_global_dep2 progs dep_map glos =
  let _, funcs, idx =
    List.fold_left
      (fun (i,acc,idx) -> (function (_, _, S.SKIP) -> (i,acc,idx)
                          | (_, _, S.BLOCK (S.SKIP, S.SKIP, _)) -> (i,acc,idx)
                          | (fn, params, body) ->
                             let procs : S_S.t = S.get_func_call body |> S_S.filter (fun f -> is_not_removable f) in
                             let sfn = string_of_evar fn in
                             (i+1, M_S.add sfn (i,procs) acc, M_I.add i (sfn,procs) idx)
      )) (0,M_S.empty,M_I.empty) glos in
  let n = M_S.cardinal funcs in
  let mat = (Array.make_matrix n n 0) in
  let get i j = Array.get (Array.get mat i) j in
  let set i j x =
    let mat2 = Array.get mat i in
    Array.set mat2 j x;
    Array.set mat  i mat2 in
  for i=0 to n-1 do
    for j=0 to n-1 do
      let fi, fns = M_I.find i idx in
      let fj, _ = M_I.find i idx in
      if S_S.mem fj fns then
        begin
          set i j 1
        end
    done
  done;
  for k=0 to n-1 do
    for i=0 to n-1 do
      for j=0 to n-1 do
        let xik = get i k in
        let xkj = get k j in
        if xik > 0 && xkj > 0 then
          set i j (xik+xkj)
      done
    done
  done;  
  for i=0 to n-1 do
    for j=0 to n-1 do
      let x = (Array.get (Array.get mat i) j) in
      if x>0 then
        F.dbgf "SC-ARR" "%d,%d --> %d" i j x; 
   done
  done;
  ()

let to_procs =
  List.fold_left
      (fun acc -> function G.PROC (proc,_,_,_,_) -> acc@[proc]
                         (* | G.FFILE (st,_) ->
                            begin
                              let fin = open_in st in
                              let pd : G.t = Marshal.from_channel fin in
                              close_in fin;
                              match pd with
                              | G.PROC ((_,_,S.SKIP),_,_,_,_)
                                | G.PROC ((_,_, S.BLOCK(S.SKIP,S.SKIP,_)),_,_,_,_) ->
                                 acc
                              | G.PROC (proc,_,_,_,_) -> acc@[proc]
                              | _ -> acc
                            end *)
                         | _ -> acc
      ) []
;;

let is_sub x y = false
  (* let xs = String.to_seq x in
  let ys = String.to_seq y in
  let rec is_sub xs ys ys0 =
    match xs, ys with
      Seq.Nil, _ -> false
    | _, Seq.Nil -> true
    | Seq.Cons (x, xs'), Seq.Cons (y, ys') ->
       if x=y then
         is_sub (xs' ()) (ys' ()) ys0
       else
         is_sub (xs' ())  ys0 ys0
  in
  is_sub (xs ()) (ys ()) (ys ())
   *)
  
let manage_main procs =
  let does_main_exists = List.exists (fun (nm, _, _) ->
                             let snm = E.toStr nm in
                             snm = "main" || is_sub snm "MainWindow") procs in
  let procs' = 
    if does_main_exists then
      begin
        List.map (function (nm, a, b) when is_sub (E.toStr nm) "MainWindow" -> (E.VAR ("main",[]), a, b) | e -> e) procs
      end
    else
      begin
        print_endline "Main is necesary";
        let asserteds = List.filter (fun (_,_,body) -> S.has_assert body) procs in
        let main = Procedure.mk_main asserteds in
        Procedure.pprint main ;
        main::procs
      end
  in
  print_endline "Manage Main ends";
  procs'
;;

    
let slac_to_consort progs =
  structs := List.fold_left (fun acc -> (function G.STRUCT (a,_) -> a::acc | _ -> acc)) [] progs;
  let funs = List.filter
               (function
                | G.PROC ((_,_,S.SKIP),_,_,_,_)
                  | G.PROC ((_,_,S.BLOCK (S.SKIP, S.SKIP, _)),_,_,_,_) ->
                   false
                | G.PROC ((nm,_,_),_,_,_,_) when is_not_removable (E.var_decode nm)  ->
                   true
                | G.FFILE (_,nm) when is_not_removable nm  ->
                (* true *) false
                | _ -> false) progs in
  let globals = List.filter (function G.STATEMENT _ -> true | _ -> false) progs in
  let procs =
    funs
    |> to_procs
    |> Procedure.fp_cut
    |> manage_main
         (* |> Pruner.prune_procs *)
    
  in
  procedures := procs;

  let dep_map = get_func_global_dep3 procs  DM.empty procs in
  F.pn "dep map is finished";
 
  let fns' = slacC_to_prog dep_map [] procs in
  let fns = Slicer.slice_fns fns' in
  F.dbgf "XXX" "@@G";
  let is_main_exists = List.exists (function ("main",params,_) -> true
                                              (* List.iter (fun s -> print_endline s) params; *)
                                           | (mn, _,_) -> is_sub mn "MainWindow"
                         ) fns in
  let main_params =
    if is_main_exists then
        let (_, main_params, _) = List.find (function ("main",params,_) -> true
                                                                             (* List.iter (fun s -> print_endline s) params; *)
                                                    | (mn, _,_) -> is_sub mn "MainWindow") fns in
        List.map (fun s -> E.encode (s,[])) main_params
    else
      [] in
  F.dbgf "XXX" "@@H";
  
  let main, gvs =
    if is_main_exists then
      begin
        let gvs = try DM.find "main" dep_map
                  with _ ->
                    try
                      let main_fn, _ = List.find (fun (m,_) -> is_sub m "MainWindow") @@ DM.bindings dep_map in
                      print_endline ("Alternate Main Function Found" ^ main_fn);
                      DM.find main_fn dep_map
                    with
                      _ ->
                      S_E.empty in
        let s_gvs = List.map (exp_to_lhs ~arg_mode:true S_E.empty S_E.empty) (main_params @ (to_list gvs)) in
        
        C.Value (dp, C.(`Call ("main",0,s_gvs))), gvs
      end
    else
      C.Unit dp, S_E.empty
  in
  let main_param_decls = List.map (fun e -> G.STATEMENT (S.DECL (e, [], S.INIT_E, S.SKIP, V.Locs.dummy))) main_params in
  F.dbgf "XXX" "@@I";
  let body = List.fold_left (fun b g ->
                 match g with
                   G.STATEMENT s -> stmt_to_fn gvs b s
                 | _ -> b
               ) main (main_param_decls @ globals) in
  (fns, body);;

let translate_to_consort consort fname progs =
  Ftools.pn "Translation to ConSORT is begins";
  let (fns, body) = slac_to_consort progs in
  Ftools.pn "Translation to ConSORT is finished";
  
  if consort = "" then
    begin
      ConsortAstPrinter.pretty_print_program stdout (fns, body)
    end
  else
    begin
      let fname' = fname ^ ".consort" in
      let ic = open_out fname' in
      ConsortAstPrinter.pretty_print_program ic (fns, body);
      (* let (fns', body') = Slicer.slice_prog (fns, body) in
      ConsortAstPrinter.pretty_print_program ic (fns', body');
       *)
      
      close_out ic;
      (* F.pn ("ConSORT output is written to " ^ fname');
      F.pn ("... will be verified by " ^ consort);
      let s1 = Sys.command (consort ^ " " ^ fname') in
      let s2 = Sys.command (consort ^ " -exec interp " ^ fname') in
      if s1 = 0 && s2 = 0 then
        F.pn ("Verification finished.")
      else
        F.pn ("Verification failed.") *)
    end;
  
  ()
