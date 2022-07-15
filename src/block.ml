open Base
open Ftools
module T = Term
module E = Exp
module B = BExp
module F = Formula

module L = Locs

exception Err of string

type init = INIT_E | INIT_S of Term.t | INIT_M of init list
type t =
  | SKIP
  | ASSERT of Formula.t * t * Locs.t
  | ASSIGN of E.t * T.t * t * L.t
  | IF of B.t * t * t * t * L.t
  | WHILE of B.t * B.t list * t * F.t * t * L.t
  | PROCCALL of Exp.t option * Term.t * Term.t list * int * t * Locs.t
  | CONS of Exp.t * (Exp.t * Term.t) list * t * Locs.t  (** Name, fields, rest *)
  | MALLOC of Exp.t * Exp.t * t * Locs.t (** Name, Length, rest *)
  | SARRAY of Exp.t * Term.t * (Exp.t * Term.t) list * t * Locs.t (** Name, Length, rest *)
  | MUTATION of Term.t * Field.t * Term.t * t * Locs.t
  | LOOKUP of Exp.t * Term.t * Field.t * t * Locs.t
  | DISPOSE of Term.t * t * Locs.t
  | MAPS of Exp.t * Exp.t * t * Locs.t
  | PARALLEL of t * t * t * Locs.t
  | BLOCK of t * t * Locs.t
  | DECL of Exp.t * Exp.t list * init * t * Locs.t
  | RETURN of Term.t * t * Locs.t
  | LABEL of string * string list * t * Locs.t
  | BREAK of t * Locs.t
  | CONTINUE of t * Locs.t
  | FAIL
;;

let noop = ref false

let extra sl =
  match (String.length sl) with
  | 0 -> "       "
  | 1 -> "    "
  | 2 -> "   "
  | 3 -> "  "
  | 4 -> " "
  | _ -> ""
;;

(** Print line numbers. *)
let printl (_, l) =
  (* let sl = string_of_int l in
     p "["; p sl; p "]"; p (extra sl) *) ()
;;

let rec print_init = function
    INIT_E -> ()
  | INIT_S t -> Term.pprint t
  | INIT_M il -> p "["; iterS print_init ";" il; p "]"
;;

let string_of_decl a len =
  match a with
    E.VAR (sname, _) ->
     let sname' = corr_fieldname sname in
     
    ((if Exp.is_struct a then
        let st = Exp.get_struct_name a in
        (st ^ " ")
      else
        "int ") ^ 
       if Exp.is_ptrptr a then
         "**"
       else if Exp.is_ptr a then
         "*"
       else
         "") ^ 
    (if Exp.is_funcptr a && not (Exp.is_func a) then
       ( "( *" ^ sname' ^ ")") else sname') ^ 
      (if List.length len > 0 then
         ("[" ^ fstrL (Exp.fstr) "][" () len ^ "]") else "")
  | _ ->
     raise (Err ("Invalid variable to declare: " ^ (E.fstr () a)) )
;;

let rec pprint t = function
  | SKIP -> p ""
  | ASSERT (b, y, l) -> begin
       printl l;
       pt "assert(" t;
       (iterS BExp.pprint " && ") ((fun (_,b,_,_) -> b) (List.hd b));
       pn ");"; pprint t y
     end
  | RETURN (i, y, l) ->
     begin
       printl l;
       pt "return " t;
       Term.pprint i;
       pn ";";
       pprint t y
     end
  | CONTINUE (y, l) ->
     begin
       printl l;
       pt "continue" t;
       pn ";";
       pprint t y
     end
  | BLOCK (a, y, l) ->
     printl l;
     pt "{" t;
     pn "";
     pprint (t+1) a;
     (* p (extra ""); *)
     pt "}" t;
     pn "";
     pprint t y
  | DECL (a, len, init_data, y, l) ->
     begin
       printl l;
       
       pt (string_of_decl a len) t;
       if init_data <> INIT_E then p " = ";
       print_init init_data;
       (* if init_data != [] then
          begin
          p " = ";
          if Exp.is_array a then p "[";
          if not (Exp.is_array a) || List.hd init_data <> Term.NULL then
          iterS Term.pprint "," init_data;
          if Exp.is_array a then p "]"
          end; *)
       
       pn ";";
       pprint t y end;
  | ASSIGN (a, b, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = ";
       Term.pprint b;
       pn ";"; pprint t y
     end
  | IF (a, b, c, y, l) -> begin
      let tb = match b with BLOCK _ -> t | _ -> t+1 in
      let tc = match c with BLOCK _ -> t | _ -> t+1 in
      printl l; pt "if(" t; BExp.pprint a; pn ")"; pprint tb b; printl l; 
      begin
        match c with
          BLOCK (SKIP, _, _) -> pt "\n" t
        | _ ->
           pt "else\n" t; pprint tc c
      end;
      pprint t y end
  | WHILE (a, bs, b, c, y, l) ->
     begin
       printl l;
       pt "while(" t;
       BExp.pprint a;
       p ")" ;
       (*if List.length bs > 0 then
         (iterS BExp.pprint "&" bs)
       else
         pw "{True}"; *)
       pn "";
       pprint t b; pprint t y
     end
  | PROCCALL (z, a, b, i, y, l) ->
     begin
       printl l;
       pt "" t;
       (match z with
         None -> ()
       | Some e -> Exp.pprint e; p " = ");
       Term.pprint a;
       pw "(";
       iterS Term.pprint ", " b;
       p " )";
       p "; //"; pi i;
       pprint t y
     end
  | CONS (a, b, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       pw " = cons(";
       iterS (fun (a,b)-> Exp.pprint a;
                          p ":"; Term.pprint b) ", " b;
       p " )";
       pn ";";
       pprint t y
     end
  | MUTATION (a, b, c, y, l) ->
     begin
       printl l; pt "" t;
       if b <> "*" then
         begin
           Term.pprint a;
           p "->"; p (corr_fieldname b)
         end
       else
         begin
           p "*("; Term.pprint a;  p ")"
         end;
       pw " =";
       Term.pprint c; pn ";";
       pprint t y
     end
  | LOOKUP (a, b, c, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = ";
       if c <> "*" then
         begin
           Term.pprint b;
           p "->";
           pw (corr_fieldname c)
         end
       else
         begin
           p "*("; Term.pprint b; p ")"
         end;
       pn ";";
       pprint t y
     end
  | DISPOSE (a, y, l) -> begin printl l; pt "dispose(" t; Term.pprint a; p ")"; pn ";"; pprint t y end
  | MAPS (a, b, y, l) -> begin printl l; pt "" t; Exp.pprint a; p " : "; Exp.pprint b; pn ";"; pprint t y end
  | PARALLEL (b, c, y, l) -> begin printl l; pt "parallel" t; pn "{"; pprint (t+1) b; p (extra ""); pt "}{" t; pn ""; pprint (t+1) c; p (extra ""); pt "}" t; pn ""; pprint t y end
  | MALLOC (a, tl, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = MALLOC( "; Exp.pprint tl;
       pn " );";
       pprint t y
     end
  | SARRAY (a, b, tl, y, l) ->
     begin
       printl l; pt "" t; Exp.pprint a; p " = SARRAY(";  Term.pprint b; p ")";
       if tl != [] then 
         begin
           p "=["; iterS (fun (_, t)->Term.pprint t) "," tl;
           p "]"
         end;
       pn ";";
       pprint t y end
  | LABEL (l, el, y, ll) -> printl ll; pt ("LABEL " ^ l  ^ "(") t; iterS pw "," el; pn ");"; pprint t y
  | BREAK (y, ll) -> printl ll; pt "BREAK" t; pn ";"; pprint t y
  | FAIL -> p "   "; pt "fail" (t+1); pn ";"
;;


let dl = ("", 0) ;;
let null = T.NULL ;;
let _T e = T.EXP e ;;

let corr_id v =
  let mx = 150 in
  let v' =
    if String.contains v '.' then
      String.map (function '.' -> '_' | '$' -> '_' | c -> c) v
    else
      v
  in
  let v'' =
    if String.length v' > 1 && String.get v' 0 = '@' then
      "GLOBAL__" ^ String.sub v' 1 (String.length v' - 1)
    else
      v' in
  let v'3 =
    if String.length v'' > mx then
      String.sub v'' (String.length v''-mx) mx
    else
      v''
  in
  v'3
;;

let var x attr = E.VAR (corr_id x, attr) ;;
let ret attr = var "__RET__" attr |> _T;;

let fresh = ref 0;;

let fresh_var attr =
  let v = string_of_int !fresh in
  let r = var ("FR_" ^ v) attr in
  fresh := !fresh + 1;
  r
;;


let op s = match s with
  | _ -> raise (Err s)
;;
let const i = E.CONST i ;;
let constf i = E.FLOAT i ;;
let bin e1 op e2 = E.BINOP (e1, op, e2) ;;
let add e1 e2 = bin e1 Op.ADD e2 ;;
let return e =
  let p = RETURN (e, SKIP, dl) in
  (* pprint 0 p; *)
  p
;;

let rec join_at_last last = function
    | SKIP -> last
    | FAIL -> FAIL
    | ASSIGN (a, b, y, l) -> ASSIGN (a, b, join_at_last last y, l)
    | ASSERT (a, y, l) -> ASSERT (a, join_at_last last y, l)
    | IF (a, b, c, y, l) -> IF (a, b, c, join_at_last last y, l)
    | WHILE (a, bs, b, c, y, l) -> WHILE (a, bs, b, c, join_at_last last y, l)
    | PROCCALL (z, a, b, i, y, l) -> PROCCALL (z, a, b, i, join_at_last last y, l)
    | CONS (a, b, y, l) -> CONS (a, b, join_at_last last y, l)
    | MUTATION (a, b, c, y, l) -> MUTATION (a, b, c, join_at_last last y, l)
    | LOOKUP (a, b, c, y, l) -> LOOKUP (a, b, c, join_at_last last y, l)
    | DISPOSE (a, y, l) -> DISPOSE (a, join_at_last last y, l)
    | MALLOC (a, tl, y, l) -> MALLOC (a, tl, join_at_last last y, l)
    | SARRAY (a, b, tl, y, l) -> SARRAY (a, b, tl, join_at_last last y, l)
    | MAPS (a, b, y, l) -> MAPS (a, b, join_at_last last y, l)
    | PARALLEL (b, c, y, l) -> PARALLEL (b, c, join_at_last last y, l)
    | BLOCK (a, y, l) -> BLOCK (a, join_at_last last y, l)
    | DECL (a, len, init_data, y, l) -> DECL (a, len, init_data, join_at_last last y, l)
    | RETURN (i, y, l) -> RETURN (i, join_at_last last y, l)
    | BREAK (y, l) -> BREAK (join_at_last last y, l)
    | CONTINUE (y, l) -> CONTINUE (join_at_last last y, l)
    | LABEL (lbl, el, y, l) -> LABEL (lbl, el, join_at_last last y, l)
;;

module S = Set.Make(Exp)

let from_list ls =
  List.fold_left (fun s e -> S.add e s) S.empty ls
;;

let rec fv_of_init = function
    INIT_E -> S.empty
  | INIT_S t -> from_list @@ T.fv t
  | INIT_M ls ->
     let fvs = List.map fv_of_init ls in
     let r = List.fold_left (fun u s -> S.union s u) S.empty fvs in
     r
;;

let rec mod_free_var = function
    | SKIP -> S.empty, S.empty
    | FAIL -> S.empty, S.empty
    | ASSIGN (a, b, y, l) ->
       let ms, fs = mod_free_var y in
       S.add a ms, S.union (S.add a (from_list @@ T.fv b)) fs
    | ASSERT (a, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.union (from_list @@ Formula.fv (List.hd a)) fs
    | IF (a, b, c, y, l) ->
       let ms1, fs1 = mod_free_var b in
       let ms2, fs2 = mod_free_var c in
       let ms, fs = mod_free_var y in
       S.union(S.union ms1 ms2) ms, S.union (from_list @@ B.fv a) (S.union(S.union fs1 fs2) fs)
    | WHILE (a, bs, b, c, y, l) ->
       let ms1, fs1 = mod_free_var b in
       let ms, fs = mod_free_var y in
       S.union ms1 ms, S.union (from_list @@ B.fv a) (S.union fs1 fs)
    | PROCCALL (z, a, b, i, y, l) ->
       let ms, fs = mod_free_var y in
       begin
         match z with
           None -> ms, List.map T.fv b |> List.concat |> from_list |> S.union fs
         | Some z' -> S.add z' ms,
                      List.map T.fv b |> List.concat |> from_list |> S.union fs |> S.add z'
       end
    | CONS (a, b, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.add a fs
    | MUTATION (a, b, c, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.union (T.fv a @ T.fv c |> from_list) fs
    | LOOKUP (a, b, c, y, l) ->
       let ms, fs = mod_free_var y in
       S.add a ms, S.union (E.fv a @ T.fv b |> from_list) fs
    | DISPOSE (a, y, l) ->
       mod_free_var y
    | MALLOC (a, tl, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.add a fs
    | SARRAY (a, b, tl, y, l) ->
       mod_free_var y
    | MAPS (a, b, y, l) ->
       mod_free_var y
    | PARALLEL (b, c, y, l) ->
       mod_free_var y
    | BLOCK (a, y, l) ->
       let ms, fs = mod_free_var y in
       let ms1, fs1 = mod_free_var a in
       S.union ms ms1, S.union fs fs1
    | DECL (a, len, init_data, y, l) ->
       let ms, fs = mod_free_var y in
       let zs = S.union (S.union (List.map E.fv len |> List.concat |> from_list) (fv_of_init init_data)) fs in
       S.remove a ms, S.remove a zs
    | RETURN (i, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.union (T.fv i |> from_list) fs
    | BREAK (y, l) ->
       mod_free_var y
    | CONTINUE (y, l) ->
       mod_free_var y
    | LABEL (lbl, el, y, l) ->
       mod_free_var y
;;

module Proc = Set.Make(String)
         
let rec get_func_call stmt =
  match stmt with
  | SKIP -> Proc.empty
  | FAIL -> Proc.empty
  | ASSIGN (_, _, y, _)
    | ASSERT (_, y, _)
    | CONS (_, _, y, _)
    | MUTATION (_, _, _, y, _)
    | LOOKUP (_, _, _, y, _)
    | DISPOSE (_, y, _)
    | MALLOC (_, _, y, _)
    | SARRAY (_, _, _, y, _)
    | MAPS (_, _, y, _)
    | PARALLEL (_, _, y, _)
    | DECL (_, _, _, y, _)
    | RETURN (_, y, _)
    | BREAK (y, _)
    | CONTINUE (y, _)
    | LABEL (_, _, y, _) ->
       get_func_call y
  | IF (_, b, c, y, _) ->
     Proc.union (Proc.union (get_func_call b) (get_func_call c)) (get_func_call y)
  | WHILE (_, _, b, _, y, _) ->
     Proc.union (get_func_call b) (get_func_call y)
  | PROCCALL (_, a, _, _, y, _) ->
     Proc.add (T.toStr a) (get_func_call y)
  | BLOCK (a, y, _) ->
     Proc.union (get_func_call a) (get_func_call y)
;;


let declared = ref [] ;;

let decl x ty =
  match x with
    E.VAR _ ->
    let p = if ty = 1 then
              DECL (x, [], INIT_E, SKIP, dl)
            else
              DECL (x, [const ty], INIT_E, SKIP, dl)
    in
    dbgf "VAR" "|(%a)" E.fstr x;
    declared := x::!declared;
    (* pprint 0 p; *)
    p
  | _ ->
     raise (Err ("Invalid variable to declare " ^ (E.fstr () x)))
;;

let decl_init x ty init =
  let p = if ty = 1 then
            DECL (x, [], init, SKIP, dl)
          else
            DECL (x, [const ty], init, SKIP, dl)
  in
  declared := x::!declared;
  (* pprint 0 p; *)
  p;;

let reset_declaration () =
  declared := List.filter E.is_global !declared
;;

let rec decl_all = function
    [] -> SKIP
  | x::xs ->
     if List.mem x !declared then
       decl_all xs
     else
       let p = decl x 1 in
       let ps = decl_all xs in
       join_at_last ps p
;;

let add_to_declared x =
  declared := !declared @ [x];;

let assign x e =
  let p = ASSIGN (x, e, SKIP, dl) in
  let ps = decl_all (x::T.fv e) in
  join_at_last p ps
;;

let cond_assign x e1 op e2 =
  let p1 = assign x (_T @@ E.CONST 1) in
  let p2 = assign x (_T @@ E.CONST 0) in
  let b  = B.UNIT (_T e1, op, _T e2) in
  let p = IF (b, p1, p2, SKIP, dl) in
  DECL (x, [], INIT_E, p, dl)
;;
let size x =
    let e_size = if x = "i32 1" then
                 E.CONST 1
               else
                 E.CONST 0 in
    e_size ;;

let malloc x size =
  let p = MALLOC (x, size, SKIP, dl) in
  let ps = decl_all ([x]) in
  join_at_last p ps
  (* pprint 0 p; *)
;;

let mutation x f e =
  let p = MUTATION (x, f, e, SKIP, dl) in
  (* pprint 0 p; *)
  p
;;
let lookup x e f =
  let p = LOOKUP (x, e, f, SKIP, dl) in
  (* pprint 0 p; *)
  let ps = decl_all (x::T.fv e) in
  join_at_last p ps
  
;;

let call ?(ret=None) x fs =
  let p = PROCCALL (ret, x, fs, 0, SKIP, dl) in
  p
;;

let block b =
  if b = BLOCK (SKIP, SKIP, dl) then
    b
  else
    BLOCK (b, SKIP, dl)
;;

let mk_if c p1 p2 =
  IF (c, p1, p2, SKIP, dl)
;;

let mk_assert b =
  ASSERT ([F.(uempty &~ b)], SKIP, dl)
;;

let mk_while b p =
  WHILE (b, [], p, F.empty, SKIP, dl)
;;

let rec join_progs = function
    [] -> SKIP
  | BLOCK(p1,SKIP,_)::xs ->
     join_at_last (join_progs xs) p1
  | x::xs ->
     join_at_last (join_progs xs) x


let __E x = Exp.VAR x
          
let __V = function Exp.VAR v -> v | _ -> raise (StError "Not a variable")
                                       
let __A = function Exp.VAR (_,a) -> a | _ -> raise (StError "Not a variable")
                                           
let __N = function Exp.VAR (n,_) -> n | _ -> raise (StError "Not a variable")

let rec is_empty = function
    SKIP -> true
  | BLOCK (p1, p2, _) -> is_empty p1 && is_empty p2
  | _ -> false


let rec substitute u t p =
  let subs_op = function
      None -> None
    | Some x -> Some (E.substitute u t x)
  in
  let subs_e = E.substitute u t in
  let subs_t = T.substitute (_T u) (_T t) in
  let subs_b = B.substitute (_T u) (_T t) in
  let subs = substitute u t in
  let rec subs_i = function
      INIT_E -> INIT_E
    | INIT_S t -> INIT_S (subs_t t)
    | INIT_M sl -> INIT_M (List.map subs_i sl) 
  in
  match p with
  | SKIP -> p
  | FAIL -> p
  | ASSIGN (a, b, y, l) ->
     ASSIGN (subs_e a, subs_t b, subs y, l)
  | ASSERT (a, y, l) ->
     ASSERT (a, subs y, l)
  | IF (a, b, c, y, l) ->
     IF (subs_b a, subs b, subs c, subs y, l)
  | WHILE (a, bs, b, c, y, l) ->
     WHILE (subs_b a, bs, subs b, c, subs y, l)
  | PROCCALL (z, a, b, i, y, l) ->
     PROCCALL (subs_op z, a, List.map subs_t b, i, y, l)
  | CONS (a, b, y, l) ->
     CONS (subs_e a, b, subs y, l)
  | MUTATION (a, b, c, y, l) ->
     MUTATION (subs_t a, b, subs_t c, subs y, l)
  | LOOKUP (a, b, c, y, l) ->
     LOOKUP (subs_e a, subs_t b, c, subs y, l)
  | DISPOSE (a, y, l) ->
     DISPOSE (subs_t a, subs y, l)
  | MALLOC (a, tl, y, l) ->
     MALLOC (subs_e a, subs_e tl, subs y, l)
  | SARRAY (a, b, tl, y, l) ->
     SARRAY (a, b, tl, subs y, l)
  | MAPS (a, b, y, l) ->
     MAPS (a, b, y, l)
  | PARALLEL (b, c, y, l) ->
     PARALLEL (b, c, y, l)
  | BLOCK (a, y, l) ->
     BLOCK (subs a, subs y, l)
  | DECL (a, len, init_data, y, l) ->
     if a = u || a = t then
       p
     else
       DECL (a, List.map subs_e len, subs_i init_data, subs y, l)
  | RETURN (i, y, l) ->
     RETURN (subs_t i, subs y, l)
  | BREAK (y, l) ->
     BREAK (subs y, l)
  | CONTINUE (y, l) ->
     CONTINUE (subs y, l)
  | LABEL (lbl, el, y, l) ->
     LABEL (lbl, el, subs y, l)

let addfvs s fvs =
  S.union s (S.of_list fvs)
;;

let addfv (r,s) fvs =
  (r, addfvs s fvs)
;;

let restore_prog structures p =
  let rec restore_prog locals p =
    match p with
    | SKIP -> (S.empty, S.empty), p
    | FAIL -> (S.empty, S.empty), p
    | ASSIGN (a, T.EXP ((E.VAR _) as b), y, l) when E.is_ptr a && E.is_ptr b && E.is_param b ->
       (* pprint 2 (ASSIGN (a, T.EXP b, SKIP, l)); *)
       let y' = substitute a b y in
       let (r,s), y'' = restore_prog locals y' in
       if !noop || (* E.is_global a || E.is_param a ||*) not (List.mem a locals) || S.mem a s then
         (S.add a r, addfvs s (E.fv b)), y''
       else
         (r,s), y''
    | ASSIGN (a, b, y, l) ->
       let (r, s), y' = restore_prog locals y in
       if !noop || (* E.is_global a || E.is_param a ||*) not (List.mem a locals) || S.mem a s then
         (r, addfvs s (T.fv b)), ASSIGN (a, b, y', l)
       else
         (r, s), y'
    | ASSERT (a, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (F.fv (List.hd a)),  ASSERT (a, y', l)
    | DECL (x, d1, d2,
            IF (a,
                (ASSIGN(x1, T.EXP (E.CONST 1),SKIP,_) as p1),
                (ASSIGN(x2, T.EXP (E.CONST 0),SKIP,_) as p2),
                y, l),d3) when x1=x2 && x=x1 ->
       let (r, s), y' = restore_prog locals y in
       
       if E.is_global x1 || E.is_param x1 || S.mem x1 s then
         let s' = S.add x s in
         (r, addfvs s' (B.fv a)), DECL (x, d1, d2, IF (a, p1, p2, y', l), d3)
       else
         (r,s), y'
    | IF (a, b, c, y, l) ->
       (* let nn = !noop in
       noop := true; *)
       let (r1, s1), b' = restore_prog [] b in
       let (r2, s2), c' = restore_prog [] c in
       let (r3, s3), y' = restore_prog locals y in
       (* noop := nn; *)
       let r = S.union (S.union r1 r2) r3 in
       let s = S.union (S.union s1 s2) s3 in
       (r, addfvs s (B.fv a)), IF (a, b', c', y', l)
    | WHILE (a, bs, b, c, y, l) ->
       let nn = !noop in
       noop := true;
       let (r1, s1), b' = restore_prog [] b in
       let (r3, s3), y' = restore_prog locals y in
       noop := nn;
       let r = S.union r1 r3 in
       let s = S.union s1 s3 in
       (r, addfvs s (B.fv a)), WHILE (a, bs, b', c, y', l)
    | PROCCALL (z, a, b, i, y, l) ->
       let (r,s), y' = restore_prog locals y in
       let fv_b = List.concat (List.map T.fv b) in
       begin
         match z with
           None ->
            (r, addfvs s fv_b), PROCCALL (z, a, b, i, y', l)
         | Some z' ->
            if !noop || (* E.is_global z' || E.is_param z' || *) not (List.mem z' locals) || S.mem z' s then
              (r, addfvs s (z'::fv_b)), PROCCALL (z, a, b, i, y', l)
            else
              (r, addfvs s fv_b), PROCCALL (None, a, b, i, y', l)
       end
    | CONS (a, b, y, l) ->
       let r, y' = restore_prog locals y in
       r, CONS (a, b, y', l)
    | MUTATION (a, b, c, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv a @ T.fv c), MUTATION (a, b, c, y', l)
    | LOOKUP (a, b, c, y, l) ->
       let (r, s), y' = restore_prog locals y in
       if !noop || E.is_global a || E.is_param a || S.mem a s then
         (r, addfvs s (T.fv b)), LOOKUP (a, b, c, y', l)
       else
         (r, s), y'
    (*  r, LOOKUP (a, b, c, y', l) *)
    | DISPOSE (a, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv a), DISPOSE (a, y', l)
    | MALLOC (a, tl, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (a::E.fv tl), MALLOC (a, tl, y', l)
    | SARRAY (a, b, tl, y, l) ->
       let r, y' = restore_prog locals y in
       r, SARRAY (a, b, tl, y', l)
    | MAPS (a, b, y, l) ->
       let r, y' = restore_prog locals y in
       r, MAPS (a, b, y', l)
    | PARALLEL (b, c, y, l) ->
       let r, y' = restore_prog locals y in
       r, PARALLEL (b, c, y', l)
    (* | BLOCK (BLOCK (a, SKIP, l), y, _) *)
    | BLOCK (a, y, l) ->
       let (r1,s1), a' = restore_prog [] a in
       let (r2,s2), y' = restore_prog locals y in
       (S.union r1 r2, S.union s1 s2), BLOCK (a', y', l)
    | DECL (a, len, init_data, y, l) ->
       begin
         let locals = a::locals in
         let deal_decl () =
           let (r,s), y' = restore_prog locals y in
           let fvlen = List.map E.fv len |> List.concat in
           let fvinit = fv_of_init init_data in
           let s' = addfvs (S.union fvinit s) fvlen in
           if S.mem a r then
             (S.remove a r, s), y'
           else
             if !noop || E.is_global a || E.is_param a || S.mem a s then
               (r, s'), DECL (a, len, init_data, y', l)
             else
               (r, s), y'
         in
         
         if not !noop && init_data = INIT_E then
           let is_not_in y = let _, fvs = mod_free_var y in
                             not (S.mem a fvs) in
           match y with
             MALLOC (c1, tl,
                     ASSIGN (p, c2, y, _), _) when a=c1 && c1=T.toExp c2 && is_not_in y ->
              let tl =
                if E.is_struct p then
                  match tl with
                    E.CONST n ->
                     let m = E._struct_size structures p in
                     let z = E.CONST (n/m) in
                     let snm = E.get_struct_name p in
                     E.BINOP (E.SIZEOF snm, Op.MUL, z)
                  | _ ->
                     tl
                else
                  tl
              in
              restore_prog locals (MALLOC (p, tl, y, l))
           | IF (b,
                 ASSIGN (c1, T.EXP(E.CONST 1), SKIP, _),
                 ASSIGN (c2, T.EXP(E.CONST 0), SKIP, _),
                 DECL (cv'', _, INIT_E,
                       ASSIGN (cv', cp,
                               ASSERT ([(_,[B.UNIT (cv, Op.NE, T.EXP (E.CONST 0))],[],[])],
                                       y,_),_), _),
                 _) when T.toExp cv=cv' && T.toExp cp=a && a=c1 && a=c2 && is_not_in y ->
              restore_prog locals (ASSERT ([([],[b],[],[])],y,l))
           | LOOKUP (a1, pt, fld,
                     ASSIGN (c, a2, y, _), _) when a=a1 && a=T.toExp a2 && is_not_in y ->
              restore_prog locals (LOOKUP (c, pt, fld, y, l))
           | _ ->
              deal_decl ()
         else
           deal_decl ()
       end
    | RETURN (i, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv i), RETURN (i, y', l)
    | BREAK (y, l) ->
       let r, y' = restore_prog locals y in
       r, BREAK (y', l)
    | CONTINUE (y, l) ->
       let r, y' = restore_prog locals y in
       r, CONTINUE (y', l)
    | LABEL (lbl, el, y, l) ->
       let r, y' = restore_prog locals y in
       r, LABEL (lbl, el, y', l)
  in
  restore_prog [] p


let adjust_ptr vars p =
  let module VR = Map.Make(String) in

  let term_to_ref b =
    let fvb = T.fv b in
    let pre, v_map =
         List.fold_left (fun (pre, v_map) v ->
             let vn, attr = E.decode v in
             if VR.mem vn vars then
               let attr' = VR.find vn vars in
               if not (E.is_ptr v) && List.mem E.PTR attr' then
                 let nv = fresh_var attr in
                 let p = lookup nv (_T v) "*" in
                 let pre' = join_at_last pre p in
                 let v_map' = (v, nv)::v_map in
                 (pre', v_map')
               else
                 (pre, v_map)
             else
               (pre, v_map)
           ) (SKIP, []) fvb in
       let b' = List.fold_left (fun b (x,y) -> T.substitute (_T x) (_T y) b) b v_map in 
       b', pre
  in
  
  let rec adjust_ptr locals p =
    match p with
    | SKIP -> p
    | FAIL -> p
    | ASSIGN (a, b, y, l) ->
       let y' = adjust_ptr locals y in
       let b', pre = term_to_ref b in
       let p =
         let an, attr = E.decode a in
         if VR.mem an vars then
           let attr' = VR.find an vars in
           if not (E.is_ptr a) && List.mem E.PTR attr' then
             mutation (_T a) "*" b' 
           else
             ASSIGN (a, b',y',l)
         else
           ASSIGN (a, b',y',l)
       in
       join_at_last p pre
    | ASSERT (a, y, l) ->
       let y' = adjust_ptr locals y in
       ASSERT (a, y', l) 
    | IF (a, b, c, y, l) ->
       let b' = adjust_ptr locals b in
       let c' = adjust_ptr locals c in
       let y' = adjust_ptr locals y in
       IF (a, b', c', y', l)
    | WHILE (a, bs, b, c, y, l) ->
       let b' = adjust_ptr locals b in
       let y' = adjust_ptr locals y in
       WHILE (a, bs, b', c, y', l)
    | PROCCALL (z, a, b, i, y, l) ->
       let y' = adjust_ptr locals y in
       PROCCALL (z, a, b, i, y', l)
    | CONS (a, b, y, l) ->
       let y' = adjust_ptr locals y in
       CONS (a, b, y', l)
    | MUTATION (a, b, c, y, l) ->
       let y' = adjust_ptr locals y in
       MUTATION (a, b, c, y', l)
    | LOOKUP (a, b, c, y, l) ->
       let y' = adjust_ptr locals y in
       LOOKUP (a, b, c, y', l)
    | DISPOSE (a, y, l) ->
       let y' = adjust_ptr locals y in
       DISPOSE (a, y', l)
    | MALLOC (a, tl, y, l) ->
       let y' = adjust_ptr locals y in
       MALLOC (a, tl, y', l)
    | SARRAY (a, b, tl, y, l) ->
       let y' = adjust_ptr locals y in
       SARRAY (a, b, tl, y', l)
    | MAPS (a, b, y, l) ->
       let y' = adjust_ptr locals y in
       MAPS (a, b, y', l)
    | PARALLEL (b, c, y, l) ->
       let y' = adjust_ptr locals y in
       PARALLEL (b, c, y', l)
    | BLOCK (a, y, l) ->
       let a' = adjust_ptr [] a in
       let y' = adjust_ptr locals y in
       BLOCK (a', y', l)
    | DECL (a, len, init_data, y, l) ->
       let y' = adjust_ptr locals y in
       DECL (a, len, init_data, y', l)
    | RETURN (i, y, l) ->
       let y' = adjust_ptr locals y in
       RETURN (i, y', l)
    | BREAK (y, l) ->
       let y' = adjust_ptr locals y in
       BREAK (y', l)
    | CONTINUE (y, l) ->
       let y' = adjust_ptr locals y in
       CONTINUE (y', l)
    | LABEL (lbl, el, y, l) ->
       let y' = adjust_ptr locals y in
       LABEL (lbl, el, y', l)
  in
  adjust_ptr [] p


let adjust_calls aux_funcs p =
  let module VR = Map.Make(String) in

  let rec adjust_calls p =
    match p with
    | SKIP -> p
    | FAIL -> p
    | ASSIGN (a, b, y, l) ->
       let y' = adjust_calls y in
       ASSIGN (a, b,y',l)
    | ASSERT (a, y, l) ->
       let y' = adjust_calls  y in
       ASSERT (a, y', l) 
    | IF (a, b, c, y, l) ->
       let b' = adjust_calls  b in
       let c' = adjust_calls  c in
       let y' = adjust_calls  y in
       IF (a, b', c', y', l)
    | WHILE (a, bs, b, c, y, l) ->
       let b' = adjust_calls  b in
       let y' = adjust_calls  y in
       WHILE (a, bs, b', c, y', l)
    | PROCCALL (z, a, b, i, y, l) ->
       begin
         let y' = adjust_calls  y in
         try
           let sa, attr = E.decode (T.toExp a) in
           let (fs_name, args, _) = List.find (fun (a',_,_) -> a'=sa) aux_funcs in
           let a' = _T @@ E.encode (fs_name, attr) in
           PROCCALL (z, a', args, i, y', l)
         with
           Not_found ->
        PROCCALL (z, a, b, i, y', l)
       end
    | CONS (a, b, y, l) ->
       let y' = adjust_calls  y in
       CONS (a, b, y', l)
    | MUTATION (a, b, c, y, l) ->
       let y' = adjust_calls  y in
       MUTATION (a, b, c, y', l)
    | LOOKUP (a, b, c, y, l) ->
       let y' = adjust_calls  y in
       LOOKUP (a, b, c, y', l)
    | DISPOSE (a, y, l) ->
       let y' = adjust_calls  y in
       DISPOSE (a, y', l)
    | MALLOC (a, tl, y, l) ->
       let y' = adjust_calls  y in
       MALLOC (a, tl, y', l)
    | SARRAY (a, b, tl, y, l) ->
       let y' = adjust_calls  y in
       SARRAY (a, b, tl, y', l)
    | MAPS (a, b, y, l) ->
       let y' = adjust_calls  y in
       MAPS (a, b, y', l)
    | PARALLEL (b, c, y, l) ->
       let y' = adjust_calls  y in
       PARALLEL (b, c, y', l)
    | BLOCK (a, y, l) ->
       let a' = adjust_calls  a in
       let y' = adjust_calls  y in
       BLOCK (a', y', l)
    | DECL (a, len, init_data, y, l) ->
       let y' = adjust_calls  y in
       DECL (a, len, init_data, y', l)
    | RETURN (i, y, l) ->
       let y' = adjust_calls  y in
       RETURN (i, y', l)
    | BREAK (y, l) ->
       let y' = adjust_calls  y in
       BREAK (y', l)
    | CONTINUE (y, l) ->
       let y' = adjust_calls  y in
       CONTINUE (y', l)
    | LABEL (lbl, el, y, l) ->
       let y' = adjust_calls  y in
       LABEL (lbl, el, y', l)
  in
  adjust_calls p
