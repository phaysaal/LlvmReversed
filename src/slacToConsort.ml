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

exception NotSupported of string
exception Supported of string
exception Unexpected of string
                     
let dlpos = Lexing.dummy_pos ;;
let dummy_pos = (0,dlpos) ;; (** Q.What is the first element? *)
let unit = C.Unit (dummy_pos) ;;
let mk_val v = C.Value (dummy_pos, v) ;;
let mk_num n = mk_val (C.(`OInt n)) ;; 

let to_list s = S_E.fold (fun e l -> e::l) s [] ;;

let procedures : (Procedure.t list) ref = ref [] ;;
              
let string_of_op = function
	| V.Op.ADD -> "+"
	| SUB -> "-"
	| MUL -> "*"
	| DIV -> "/"
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

let is_a_ptr ptrs vs = function
    E.VAR (s,attr) ->
     let b1 = S_E.exists (function E.VAR (sv,_) -> s=sv | _ -> false) ptrs in
     let b2 = List.mem E.PTR attr in
     let b3 = S_E.exists (function E.VAR (sv,attr') -> sv=s (* && List.mem E.PTR attr' *) | _ -> false) vs in
     (* let cnd = s = "z" || s = "z_addr" in
     F.dbgf "SC" "is_a_ptr|v:%a" E.fstr v;
     F.dbgc cnd "is_a_ptr|Probably a PTR: %s" (sb b1);
     F.dbgc cnd "is_a_ptr|Originally PTR: %s" (sb b2);
     F.dbgc cnd "is_a_ptr|Defined as PTR: %s" (sb b3); *)
     b1 || b2 || b3
  | _ -> false
;;

let corr_id v =
  let v' =
    if String.contains v '.' then
      String.map (function '.' -> '_' | '$' -> '_' | c -> c) v
    else
      v
  in
  if String.get v' 0 = '@' then
    "GLOBAL__" ^ String.sub v' 1 (String.length v' - 1)
  else
    v'
;;

let add_ret exp =
  C.(Let (dummy_pos, Ast.PVar "__RET__", `Mkref (`OInt 0), exp))
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
  | e -> raise (NotSupported ("string_of_evar:" ^ E.toStr e))
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

let rec exp_to_lhs ?(arg_mode=false) ptrs vs e : C.lhs =
  match e with
    E.NOTHING ->    raise (NotSupported "Nothing")
  | NEGINF ->       C.(`OInt (Int.min_int))
  | POSINF ->       C.(`OInt (Int.max_int))
  | UNDEF ->        raise (NotSupported "Undef")
  | VAR (s, _) ->   if arg_mode then
                      C.(`OVar (corr_id s))
                    else
                      begin
                        E.pprint e; F.pn "";
                        S_E.iter (fun v -> E.pprint v; F.pw "") vs ; F.pn "";
     
                        if is_a_ptr ptrs vs e (*E.is_ptr e || List.exists (function E.VAR (sv,_) -> s=sv | _ -> false) ptrs *) then
                          C.(`ODeref (corr_id s))
                        else
                          C.(`OVar (corr_id s))
                      end
  | CONST (n) ->    C.(`OInt n)
  | FLOAT (f) ->    raise (NotSupported "Float")
  | BINOP (e1, op, e2) ->
                    C.(`BinOp (exp_to_lhs ptrs vs e1, string_of_op op, exp_to_lhs ptrs vs e2))
  | INDICES (_) ->  raise (NotSupported "Indices")
  | ADDR (a) ->     (* C.(`Mkref (exp_to_lhs ptrs vs a)) (* Need to make sure if it is correct *) *) exp_to_lhs ptrs vs a
  | REF (r) ->      C.(`Mkref (exp_to_lhs ptrs vs r))     
  | NEG (n) ->      begin
      match exp_to_lhs ptrs vs n with
        C.(`OInt n') -> C.(`OInt (-n'))
      | _ -> raise (NotSupported "Neg")
    end
  | STRING (s) ->   C.(`ODeref s) 
  | ARROW (_, _) -> raise (NotSupported "Arrow")
  | LBL (_, _) ->   raise (NotSupported "LBL")
  | FCALL (fn, args) ->
     C.(`Call (fn, 0, List.map (exp_to_lhs ~arg_mode:true ptrs vs) args))
  (** Q.What the second element mean? *) 
  | OFFSET (_, _) -> raise (NotSupported "Offset")
  | SIZEOF (s) ->   if E.is_simple_type s then
                      C.(`OInt (E.simple_size s))
                    else
                      raise (NotSupported "Sizeof")
;;

let term_to_lhs ?(arg_mode=false) ptrs vs = function
    T.NULL -> C.(`Null)
  | T.EXP e -> exp_to_lhs ~arg_mode:arg_mode ptrs vs e
;;

let rec bexp_to_lhs ptrs vs = function
    B.UNIT (t1, op, t2) -> C.(`BinOp (term_to_lhs ptrs vs t1, string_of_op op, term_to_lhs ptrs vs t2))
  | OP (b1, op, b2) -> C.(`BinOp (bexp_to_lhs ptrs vs b1, string_of_op op, bexp_to_lhs ptrs vs b2))
  | _ -> raise (NotSupported "bexp_to_lhs")
;;

let term_to_imm_op = function
    T.NULL -> C.IInt 0
  | T.EXP (E.CONST i) -> C.IInt i
  | T.EXP (E.VAR (s,_)) -> C.IVar s
  | _ -> raise (NotSupported "term_to_imm_op")
;;
                           
let bexp_to_relation = function
    B.UNIT (t1, op, t2) ->
    C.{rop1= term_to_imm_op t1;cond=string_of_op op; rop2= term_to_imm_op t2}
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
    B.UNIT (t1, op, t2) ->
     C.(`BinOp (term_to_lhs ptrs vs t1, string_of_op op, term_to_lhs ptrs vs t2))
  | B.OP (b1, V.Op.AND, b2) ->
     let l1  = bexp_to_ifcond ptrs vs b1 in
     let l1' = to_lhs l1 in
     let l2  = bexp_to_ifcond ptrs vs b2 in
     let l2' = to_lhs l2 in
     C.(`BinOp (l1', "/\\", l2'))
  | B.OP (B.UNIT (a, V.Op.LE, b), V.Op.OR, B.UNIT (a', V.Op.EQ, b'))
    | B.OP (B.UNIT (a, V.Op.EQ, b), V.Op.OR, B.UNIT (a', V.Op.LE, b')) when a=a' && b=b' ->
     C.(`BinOp (term_to_lhs ptrs vs a, "<=", term_to_lhs ptrs vs b))
  | _ ->
     B.pprint b; F.pn "";
     raise (NotSupported "bexp_to_ifcond")
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
  match e with
    C.Unit _ -> e
  | Value (pos, x) -> Value (pos, subs_lhs s t x)
  | Cond (pos, cond, l, r) ->
     Cond (pos, subs_lhs2 s t cond, subs_exp s t l, subs_exp s t r)
  | NCond (pos, str, l, r) ->
     NCond (pos, subs_str s t str, subs_exp s t l, subs_exp s t r)
  | Assign (pos, str, l) ->
     Assign (pos, subs_str s t str, subs_lhs s t l)
  | Update (pos, l1, l2, l3) ->
     Update (pos, subs_lhs s t l1, subs_lhs s t l2, subs_lhs s t l3)
  | Alias (pos, str, a) ->
     Alias (pos, subs_str s t str, a)
  | EAnnot (pos, annots) -> raise (NotSupported "yet") 
  | Assert (pos, relation) ->
       Assert (pos, subs_rel s t relation)
  | Let (pos, patt, l, e) ->
     Let (pos, subs_patt s t patt, subs_lhs s t l, subs_exp s t e)
  | Seq (pos, l, r) ->
     Seq (pos, subs_exp s t l, subs_exp s t r)
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

let fix_shadowing declared a y =
  F.dbgc (E.toStr a="z_addr") "z_addr is found in %a" (F.fstrL E.fstr ",") (S_E.elements declared);

  if S_E.mem a declared then
    let u = (E.toStr a) in
    let t = "_" ^ u in
    let et = E.VAR (t, E.get_attributes a) in 
    unshadow u t y, S_E.add et declared
  else
    y, S_E.add a declared
;;


let rec declare_and_initialize declared' ptrs vs y a len init_data =
  (* if is_declared declared' a then
    y', declared'
  else *)
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
           begin
             match len, init_data with
             | [], S.INIT_E ->
                if (* not (is_declared declared a) && *) is_a_ptr ptrs vs a then
                  C.Let (dummy_pos,
                         Ast.PVar (string_of_evar a),
                         C.(`Mkref (C.(`OInt 0))),
                         (* C.(`OInt 0), *)
                         y
                    )
                else
                  y
             | [], S.INIT_S t ->
                C.Let (dummy_pos,
                       Ast.PVar (string_of_evar a),
                       (* C.(`Mkref (term_to_lhs t)), *)
                       term_to_lhs ptrs vs t,
                       y
                  )  
             | E.CONST n::_, S.INIT_E when n=0 || n=1 -> 
                C.Let (dummy_pos,
                       Ast.PVar (string_of_evar a),
                       (* C.(`Mkref (C.(`OInt 0))), *)
                       C.(`OInt 0),
                       y
                  )
             | E.CONST n::_, S.INIT_S t when n=0 || n=1 ->
                C.Let (dummy_pos,
                       Ast.PVar (string_of_evar a),
                       (* C.(`Mkref (term_to_lhs t)), *)
                       term_to_lhs ptrs vs t,
                       y
                  )
             | _ -> 
                raise (Supported "declare and initialize (mistake perhaps)")
           end
        | true, false -> (** TODO: Check and correct *)
           C.Let (dummy_pos,
                  init_data_to_patt (string_of_evar a) init_data,
                  init_data_to_tuple ptrs init_data,
                  y
             )
        | false, true -> (** TODO: Multidimentional array support *)
           C.Let (dummy_pos,
                  Ast.PVar (string_of_evar a),
                  C.(`MkArray (exp_to_lhs ptrs vs @@ List.hd len)),
                  y
             )
        | _, _ -> raise (NotSupported "Others")
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
    -> C.Seq (Lexing.dummy_pos, p', p)     
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
         E.pprint l'; F.pn "";
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
    
       
let mk_assert ptrs vs y b =
  match b with
    B.UNIT (l, op, r) ->
     let l', pr1 = to_simple ptrs vs l in
     let r', pr2 = to_simple ptrs vs r in
     let pr : (string * C.lhs) list = pr1@pr2 in
     let cnd = C.{rop1= term_to_imm_op l';cond=string_of_op op; rop2= term_to_imm_op r'} in
     let prog =  C.Seq (dlpos,
             C.Assert (dummy_pos, cnd),
             y) in
     let all_prog = List.fold_left (fun prog (l,r) -> C.Let (dummy_pos, Ast.PVar l, r, prog)) prog pr in
     all_prog
  | B.OP (l, op, r) ->
     raise (NotSupported " yet")
  | _ ->
     raise (NotSupported " and never will")
;;

let rec body_to_cexp ((dep_map, gvs, (vs:S_E.t)) as gvars) prog =
  
  match prog with
  | S.SKIP -> (S_E.empty, S_E.empty, [], C.Unit (dummy_pos)) (** Q.Except for debugging, is there any other use of pos? *)
  | ASSIGN (a, b, y, _) ->             (** Q. Is Assign or Let or Update for a C assignment? *)
     let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, S_E.add a vs) y in
     let sa = try string_of_evar a with e -> F.dbgf "SC" "Exception in Assing"; raise e in
     let lb = term_to_lhs ptrs vs b in
     (* let y', declared = fix_shadowing declared a y' in *)

     let nptr, p =
       (* match is_a_ptr ptrs vs a, is_a_ptr ptrs vs (T.toExp b) with
         true, true ->
          S_E.empty, C.Let (dummy_pos, Ast.PVar sa, lb, y')
       | _ -> *)
          S_E.add a S_E.empty, (* C.Let (dummy_pos, Ast.PVar sa, lb, y') *)
          C.Seq (Lexing.dummy_pos, C.Assign (dummy_pos, sa, lb), y')
         
       (* | false, false
       | true, false ->
          S_E.add a S_E.empty, (* C.Let (dummy_pos, Ast.PVar sa, C.(`Mkref lb), y') *)
          C.Seq (Lexing.dummy_pos, C.Assign (dummy_pos, sa, lb), y')
       | false, true ->
          S_E.add a S_E.empty, (* C.Let (dummy_pos, Ast.PVar sa, lb, y') *)
          (** a fresh variable is required *)
          C.Seq (Lexing.dummy_pos, C.Assign (dummy_pos, sa, lb), y')
        *)
     in

     (* let nptr, p =
       if is_a_ptr ptrs vs a then
         (* C.(Let (dummy_pos, Ast.PVar sa, `Mkref lb, y')) *)
         S_E.add a S_E.empty, C.Seq (Lexing.dummy_pos, C.Assign (dummy_pos, sa, lb), y')
       else
         S_E.empty, C.Let (dummy_pos, Ast.PVar sa, lb, y') in
      *)
     F.dbg "SC" "Assign|a:" E.pprint a;
     
     F.dbgf "SC" "Assign(E)";
     (S_E.union nptr ptrs,
      declared,
      ry,
      p
      (* C.Seq (dlpos,
             C.Assign (dummy_pos, sa, lb),
             y') *)
     
     ) (** Q. Does Seq correspond to composition? *) 
     
  | ASSERT (a, y, l) ->
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     F.pn_s "SC" "Assert|"; (* F.iterS (E.print) "," vs; F.pn ""; *)
     let b = try mk_assert ptrs vs y' ((fun (_, b, _, _) -> List.hd b) (List.hd a)) with e -> F.dbgf "SC" "Exception in assert"; raise e in
     
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
     let modv_b, _ = S.mod_free_var b in
     let modv_c, _ = S.mod_free_var c in
     let _, freev_y = S.mod_free_var y in
     let comv = S.S.inter (S.S.union modv_b modv_c) freev_y in
     let comv' = S_E.map (function E.VAR (v, attr) -> E.VAR (v, E.PTR::attr) | e -> e) comv in
     let gvars' = (dep_map, gvs, S_E.union vs comv') in
     let ptrs3, declared1, ry, y' = body_to_cexp gvars' y in
     let gvars'' = (dep_map, gvs, S_E.union (S_E.union vs comv') ptrs3) in
     let ptrs1, declared2, rb, b' = body_to_cexp gvars'' b in
     let ptrs2, declared3, rc, c' = body_to_cexp gvars'' c in
     
     let ptrs = S_E.union (S_E.union (S_E.union ptrs1 ptrs2) ptrs3) comv' in
     
     F.dbg "SC" "IF a:" B.pprint a;
     (* F.dbg "SC" "ptrs:" (F.iterS E.pprint ",") ptrs; *)
     (ptrs,
      S_E.union (S_E.union declared1 declared2) declared3,
      rb @ rc @ ry,
      C.Seq (dlpos,
             C.Cond (dummy_pos, bexp_to_ifcond ptrs vs a, b', c'),
             y'
     ))
  | WHILE (a, bs, b, c, y, l) ->
     F.dbgf "SC" "While(B)|";
     let ptrs, declared, rw, w' = while_to_recf gvars a b y in
     F.dbgf "SC" "While(E)|";
     (ptrs, declared, rw, w')
  | PROCCALL (_, T.EXP (E.VAR ("__VERIFIER_nondet_int",_)), [], _,
              DECL(a, _, INIT_E,
                   DECL(_,_,_,
                        ASSIGN(a',r',y,l),_), _), _)
    | PROCCALL (_, T.EXP (E.VAR ("__VERIFIER_nondet_int",_)), [], _,
              DECL(a, _, INIT_E,
                        ASSIGN(a',r',y,l),_), _)
     when a=a' ->
     
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     let y', declared = fix_shadowing declared a y' in
     let sa = string_of_evar a in
     let lb = C.(`Nondet (Some RefinementTypes.Top)) in
     let p = if is_a_ptr ptrs vs a then
               C.Let (dummy_pos, Ast.PVar sa, lb, y')
             else
               C.Let (dummy_pos, Ast.PVar sa, lb, y') in
     (ptrs,
      declared,
      ry,
      p
     )
  | PROCCALL (_, T.EXP (E.VAR ("llvm.stacksave",_)), [], _,
              y, _) ->
     body_to_cexp gvars y
  | PROCCALL (_, T.EXP (E.VAR ("llvm.stackrestore",_)), _, _,
              y, _) ->
     body_to_cexp gvars y
  | PROCCALL (_, T.EXP (E.VAR ("__VERIFIER_assert",_)), cond::_, _,
              y, _) ->
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     let a = B.UNIT (cond, V.Op.NE, T.zero) in
     (ptrs, declared, ry,
      C.Seq (dlpos,
             C.Assert (dummy_pos, bexp_to_relation a),
             y'))
  | PROCCALL (_, T.EXP (E.VAR ("abort",_)), [], _,
              y, _) ->
     S_E.empty, S_E.empty, [], C.Assert (dummy_pos, bexp_to_relation (B.UNIT(T.zero, V.Op.NE, T.zero)))
  | PROCCALL (z, a, b, i, y, l) ->
     let addrs' = (List.map T.toExp b)
                  |> S_E.of_list
                  |> S_E.filter (function E.ADDR _ -> true | _ -> false) 
                  |> S_E.map (function E.ADDR x -> x | x -> x) in
     let addrs = S_E.map (function E.VAR (s,attr) -> E.VAR (s, E.PTR::attr) | v -> v) addrs' in
     let vs = S_E.filter (fun v -> not (S_E.mem v addrs')) vs in
     let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, S_E.union vs addrs) y in
     let gvs = try DM.find (string_of_tvar a) dep_map with _ -> S_E.empty in
     let lgvs = to_list gvs in
     (* let lval = 
       (* if S_E.is_empty gvs then *)
         Ast.PVar "__RET__"
       (* else
         let ast_gvs = List.map (fun v -> Ast.PVar (string_of_evar v)) lgvs in
         Ast.PTuple (Ast.PVar "$ret"::ast_gvs) *)
     in *)
     let lhs_gvs = List.map (exp_to_lhs ~arg_mode:true ptrs vs) lgvs in
     let (_, params, _) = List.find (fun (pn,_,_) -> pn=T.toExp a) !procedures in
     let vparams = List.map var_of_assignee params in
     
     let par_arg = List.combine b vparams in
     let args' = List.map (fun (a, p) ->
                     if not (E.is_ptr p) then
                       term_to_lhs ptrs vs a
                     else
                       term_to_lhs ~arg_mode:true ptrs vs a
                   ) par_arg in 
     let all_args = args' @ lhs_gvs in
     let ptrs', p, declared = (* C.Let (dummy_pos,
             lval,
             C.(`Call (string_of_tvar a, List.length b, all_args)),
             y') in *)
       match z with
         None ->
         ptrs, C.(Seq (Lexing.dummy_pos,
                 Value (dummy_pos,
                         `Call (string_of_tvar a, List.length b, all_args)),
                 y')), declared
       | Some z' ->
          let y', declared = fix_shadowing declared z' y' in
          S_E.add z' ptrs, C.(Seq (Lexing.dummy_pos, Assign (dummy_pos, string_of_evar z', `Call (string_of_tvar a, List.length b, all_args)), y')), declared
          (* C.(Seq (Lexing.dummy_pos,
                 Assign (dummy_pos,
                         string_of_evar z',
                         `Call (string_of_tvar a, List.length b, all_args)),
                 y')) *)
     in
     (S_E.union ptrs' addrs,
      declared,
      ry,
      p)
  (* | CONS (a, size, y, l) *)
  | MALLOC (a, size, y, l)  (** TODO: Need to correctly handle for struct and array *)
    ->
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     let s_size = exp_to_lhs ptrs vs size in
     let ptrs' = S_E.filter (fun x->not (E.toStr x=E.toStr a)) ptrs in
     let y', declared = fix_shadowing declared a y' in
     let p = C.Let (dummy_pos,
             Ast.PVar (string_of_evar a),
             C.(`MkArray (s_size)),
             y') in
     (ptrs',
      declared,
      ry,
      p
     )   
  | MUTATION (a, b, c, y, l) ->
     let ptrs1, declared, ry, y' = body_to_cexp gvars y in
     let (pt, ind) = de_add a in
     let fvt = T.head "Mutation" a in

     let ptrs = S_E.add fvt ptrs1 in
     F.dbg "SC" "MUTATION:" F.p "";
     (* F.dbg "SC" "ptrs:" (F.iterS E.pprint ",") ptrs; *)

     let prog =
       if E.is_array pt then
         C.Update (dummy_pos,
                  exp_to_lhs S_E.empty S_E.empty pt,
                  exp_to_lhs ptrs vs ind,
                  term_to_lhs ptrs vs c)
       else
         if ind = E.CONST 0 then
           C.Assign (dummy_pos,
                     string_of_evar pt,
                     term_to_lhs ptrs vs c
             )
         else
           C.Update (dummy_pos,
                     exp_to_lhs S_E.empty S_E.empty pt,
                     exp_to_lhs ptrs vs ind,
                     term_to_lhs ptrs vs c)
     in
     if b = "*" then
       (ptrs,
        declared,
        ry,
        C.Seq(Lexing.dummy_pos,
              prog,
              y')
       )
     else
       raise (Supported "Mutation")
  | LOOKUP (a, b, c, y, l) ->
     
     let ptrs, declared, ry, y' = body_to_cexp gvars y in
     let (pt, ind) = de_add b in
     F.dbg "SC" "LOOKUP a:" E.pprint a;
     (* F.dbg "SC" "ptrs:" (F.iterS E.pprint ",") ptrs; *)
     let y', declared = fix_shadowing declared a y' in

     let rhs =
       if E.is_array pt then
         C.(`Read (exp_to_lhs S_E.empty S_E.empty pt, exp_to_lhs ptrs vs ind))
       else
         if ind = E.CONST 0 then
           C.(`ODeref (string_of_evar pt))
         else
           C.(`Read (exp_to_lhs S_E.empty S_E.empty pt, exp_to_lhs ptrs vs ind))
     in
     if c = "*" then
       (S_E.add a ptrs,
        declared,
        ry,
        (* C.Let (dummy_pos,
               Ast.PVar (string_of_evar a),
               rhs,
               y') *)
        C.Seq (Lexing.dummy_pos, C.Assign (dummy_pos, string_of_evar a,rhs), y')
       )
     else
       raise (Supported "Lookup")
  (* | DISPOSE (a, y, l) -> DISPOSE (a, join_at_last last y, l)
   -> MALLOC (a, tl, join_at_last last y, l)
  | SARRAY (a, b, tl, y, l) -> SARRAY (a, b, tl, join_at_last last y, l)
  | MAPS (a, b, y, l) -> MAPS (a, b, join_at_last last y, l)
  | PARALLEL (b, c, y, l) -> PARALLEL (b, c, join_at_last last y, l)
   *)
  | BLOCK (a, y, l) ->
     let ptrs1, declared, ra, a' = body_to_cexp gvars a in
     begin
       match y with
         S.SKIP ->
          F.dbg "SC" "BLOCK:" F.p "";
          (* F.dbg "SC" "ptrs:" (F.iterS E.pprint ",") ptrs1; *)
          ptrs1, declared, ra, a'
       | _ ->
          let ptrs2, declared, ry, y' = body_to_cexp gvars y in
          let ptrs = S_E.union ptrs1 ptrs2 in
          F.dbg "SC" "BLOCK:" F.p "";
          (* F.dbg "SC" "ptrs:" (F.iterS E.pprint ",") ptrs; *)
          (ptrs,
           declared,
           ry,
           C.Seq (dlpos,
                  a',
                  y'))
     end
  | DECL (a, len, init_data, y, l) ->

     let ptrs, declared, ry, y' = body_to_cexp (dep_map, gvs, S_E.add a vs) y in
     let y'' (* , declared *) = try declare_and_initialize declared ptrs vs y' a len init_data with e -> F.dbgf "SC" "Exception in DECL"; raise e in
     (ptrs,
      declared,
      ry,
      y''
     )
  | RETURN (i, _, l) ->
     (* let _, gvs = gvars in *)
     let exp = 
       (* if S_E.is_empty gvs then *)
       C.Value (dummy_pos, term_to_lhs S_E.empty vs i)
       (* else
         let l_gvs = to_list gvs in
         let lhs_gvs = List.map exp_to_lhs l_gvs in
         C.Value (dummy_pos, C.(`Tuple (term_to_lhs i :: lhs_gvs ))) *)
     in
     (S_E.empty, S_E.empty, [], exp)
  (* | BREAK (y, l) -> BREAK (join_at_last last y, l)
  | CONTINUE (y, l) -> CONTINUE (join_at_last last y, l)
  | LABEL (lbl, el, y, l) -> LABEL (lbl, el, join_at_last last y, l) *)
  | s ->
     S.pprint 0 s; 
     raise (NotSupported "Others2")

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
  let params = try List.map string_of_evar freev'  with e -> F.dbgf "SC" "Exception in While(2)"; raise e in
  let new_fn = "WHILE__" ^ (new_name ())  in
  let new_call = C.(`Call (new_fn, List.length args, args)) in
  let gvars' = (dep_map, gvs, S_E.union (S_E.union vs modv'') onlyfreev) in
  let ptrs0, _, fns, then_body' = body_to_cexp gvars' body in
  let exp' = (* C.(Let (dummy_pos, Ast.PTuple (List.map (fun s -> Ast.PVar s) params), tuple, C.Value (dummy_pos, new_call))) in *)
    (C.Value (dummy_pos, new_call)) in

  let then_body = join_at_end exp' then_body' in
  let ptrs = S_E.union ptrs0 modv'' in
  let f_body = C.Cond (dummy_pos, bexp_to_ifcond ptrs vs cond, then_body, C.Unit (dummy_pos)) in
  (* let f_body' = add_ret f_body in *)
  let new_f = (new_fn, params, f_body) in
  
  let ptrs1, declared, fns1, body' = body_to_cexp gvars' rest in
    
  let ret = C.(Seq (Lexing.dummy_pos, exp', body')) in
  F.dbg "SC" "NEW WHILE:" F.p new_fn;
  S_E.union (S_E.union modv'' ptrs0) ptrs1, declared, new_f::fns@fns1, ret 
;;


let string_of_globals = function
  | G.STATEMENT a -> C.(`OVar (var_of_assignee a |> string_of_evar))
  | _ -> raise (NotSupported "string of globals")

       
let func_to_fn dep_map (nm, params, body) =
  F.dbgf "SC" "fn: %a" E.fstr nm;
  let snm = string_of_evar nm in
  let gvars = DM.find snm dep_map in
  let vparams = List.map var_of_assignee params in
  (* let ret = E.VAR ("__RET__", [PTR]) in *)
  let ptrs, _, fns, exp = body_to_cexp (dep_map, gvars, (* ret:: *)(S_E.of_list vparams)) body in
  (* let exp' = add_ret exp in *)
  let sparams = List.map string_of_evar vparams in
  let l_glo_dep = to_list gvars |> List.map string_of_evar in
  fns @ [(snm, sparams@l_glo_dep, exp)]
;;

let stmt_to_fn b = function
  | S.ASSIGN (v, t, _, _) ->
     let sv = string_of_evar v in
     let lhs = term_to_lhs S_E.empty S_E.empty  t in
     C.Let (dummy_pos, Ast.PVar sv, lhs, b)
  | S.DECL (v, len, init_data, _, _) ->
     (* let sv = string_of_evar v in *)
     let exp (*,_*) = declare_and_initialize S_E.empty S_E.empty S_E.empty b v len init_data in
     exp
  | _ -> raise (NotSupported "Stmt")
    
let rec simpleC_to_prog dep_map acc = function
    [] -> acc
  | G.PROC (proc,_,_,_,_)::xs ->
     
     let prog = func_to_fn dep_map proc in
     
     simpleC_to_prog dep_map (acc @ prog) xs
  | G.FFILE (st,_)::xs ->
     let fin = open_in st in
     let pd : G.t = Marshal.from_channel fin in
     close_in fin;
     simpleC_to_prog dep_map acc (pd::xs)
  | _::xs -> simpleC_to_prog dep_map acc xs
  (* | G.STATEMENT a::xs -> global_to_prog (acc @ stmt_to_fn a) xs
  | G.STRUCT _::xs -> global_to_prog acc xs
  
   *)
;;

let rec get_func_global_dep_fn progs dep_map fn body =
  let sfn = string_of_evar fn in
  if DM.mem sfn dep_map then
    dep_map, DM.find sfn dep_map
  else
    let procs : S_S.t = S.get_func_call body in
    let _, fv = S.mod_free_var body in
    let gfv : S_E.t = S_E.filter E.is_global fv in
    let all_gfv, dep_map' = S_S.fold (fun proc ((fvs:S_E.t), _dep_map) ->
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
;;

let rec get_func_global_dep progs dep_map = function
  | [] -> dep_map
  | (fn, params, body)::xs ->
     if DM.mem (string_of_evar fn) dep_map then
       get_func_global_dep progs dep_map xs
     else
       let dep_map', _ = get_func_global_dep_fn progs dep_map fn body in
       get_func_global_dep progs dep_map' xs
  
;;

let is_not_removable nm =
  match nm with
    "assert"
  | "abort"
    | "__VERIFIER_nondet_int"
    | "__VERIFIER_assert"
    | "__assert_fail"
    | "reach_error"
    | "llvm.stacksave"
    | "llvm.stackrestore"
    -> false
  | _ -> true
;;

(*
let update_func_params_call.
    *)
let slac_to_consort progs =
  F.dbgf "SC" "|progs|: %d" (List.length progs);
  let funs = List.filter
               (function G.PROC ((nm,_,_),_,_,_,_) when is_not_removable (E.toStr nm)  ->
                          true
                       | G.FFILE (_,nm) when is_not_removable nm  ->
                          true
                       | _ -> false) progs in
  F.dbgf "SC" "|funs|: %d" (List.length funs);
  let globals = List.filter (function G.STATEMENT _ -> true | _ -> false) progs in
  F.dbgf "SC" "|globals|: %d" (List.length globals);
  let procs = List.fold_left (fun acc -> function G.PROC (proc,_,_,_,_) -> acc@[proc]
                                                | G.FFILE (st,_) ->
                                                   begin
                                                     let fin = open_in st in
                                                     let pd : G.t = Marshal.from_channel fin in
                                                     close_in fin;
                                                     match pd with
                                                       G.PROC (proc,_,_,_,_) -> acc@[proc]
                                                     | _ -> acc
                                                   end
                                                | _ -> acc
                ) [] funs in
  procedures := procs;
  F.dbgf "SC" "|procs|: %d" (List.length procs);
  let dep_map = get_func_global_dep procs DM.empty procs in
  F.dbgf "SC" "dep_map: %d" (DM.cardinal dep_map);
  DM.iter (fun fn _ -> F.dbgf "SC" "%s" fn) dep_map;
  let fns = simpleC_to_prog dep_map [] funs in
  let is_main_exists = List.exists (function ("main",_,_) -> true | _ -> false) fns in
  let main =
    if is_main_exists then
      begin
        let gvs = try DM.find "main" dep_map with _ -> S_E.empty in
        let s_gvs = List.map (exp_to_lhs ~arg_mode:true S_E.empty S_E.empty) (to_list gvs) in
        (* let global_vars = List.map string_of_globals globals in *)
        C.Value (dummy_pos, C.(`Call ("main",0,s_gvs)))
      end
    else
      C.Unit dummy_pos
  in
  let body = List.fold_left (fun b g ->
                 match g with
                   G.STATEMENT s -> stmt_to_fn b s
                 | _ -> b
               ) main globals in
  (fns, body)

let print_consort progs =
  
  let (fns, body) = slac_to_consort progs in
  (* let (fns', body') = C.simplify (fns, body) in *)
  Ftools.pn "**************************************";
  
  ConsortAstPrinter.pretty_print_program stdout (fns, body);
  Ftools.pn "**************************************";
  (* AstPrinter.pretty_print_program stdout (fns', body'); *)
  ()
