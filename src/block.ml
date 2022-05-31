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
       pt "int " t;
       ((if Exp.is_struct a then
          let st = Exp.get_struct_name a in
          pw st);
       if Exp.is_ptrptr a then
         pw "**"
       else if Exp.is_ptr a then
         pw "*");
       if Exp.is_funcptr a && not (Exp.is_func a) then( p "(*"; Exp.pprint a; p ")") else Exp.pprint a;
       if List.length len > 0 then
         (p "["; iterS Exp.pprint "-" len; p "]");
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
           p "->"; p b
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
           pw c
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
  let v' =
    if String.contains v '.' then
      String.map (function '.' -> '_' | '$' -> '_' | c -> c) v
    else
      v
  in
  if String.length v' > 1 && String.get v' 0 = '@' then
    "GLOBAL__" ^ String.sub v' 1 (String.length v' - 1)
  else
    v'
;;

let var x attr = E.VAR (corr_id x, attr) ;;
let ret attr = var "__RET__" attr |> _T;;

let op s = match s with
  | _ -> raise (Err s)
;;
let const i = E.CONST i ;;
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
  let p = if ty = 1 then
            DECL (x, [], INIT_E, SKIP, dl)
          else
            DECL (x, [const ty], INIT_E, SKIP, dl)
  in
  declared := x::!declared;
  (* pprint 0 p; *)
  p;;

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
  (* pprint 0 p; *)
  p
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