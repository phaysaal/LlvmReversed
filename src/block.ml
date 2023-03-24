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
  | MALLOC of Exp.t * Exp.t * t * Locs.t (** Name, Length, rest *)
  | MUTATION of Term.t * Field.t * Term.t * t * Locs.t
  | LOOKUP of Exp.t * Term.t * Field.t * t * Locs.t
  | DISPOSE of Term.t * t * Locs.t
  | BLOCK of t * t * Locs.t
  | DECL of Exp.t * Exp.t list * init * t * Locs.t
  | RETURN of Term.t * t * Locs.t
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

let printl (_, l) = ()
;;

let rec print_init = function
    INIT_E -> ()
  | INIT_S t -> Term.pprint t
  | INIT_M il -> p "["; iterS print_init ";" il; p "]"
;;

let string_of_decl a len =
  match a with
    E.VAR (sname, attrs) ->
     let sname' = corr_fieldname sname in
     let str = E.var_get_printable_string (sname', attrs) in
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
       ( "( * " ^ str ^ ")") else str) ^ 
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
  | BLOCK (a, y, l) ->
     printl l;
     pt "{" t;
     pn "";
     pprint (t+1) a;
     pt "}" t;
     pn "";
     pprint t y;
     pt "" t;
  | DECL (a, len, init_data, y, l) ->
     begin
       printl l;
       pt (string_of_decl a len) t;
       if init_data <> INIT_E then p " = ";
       print_init init_data;
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
  | MALLOC (a, tl, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = MALLOC( "; Exp.pprint tl;
       pn " );";
       pprint t y
     end
;;


let rec solo_print t = function
  | SKIP -> p "SKIP\n"
  | ASSERT (b, y, l) -> begin
       printl l;
       pt "assert(" t;
       (iterS BExp.pprint " && ") ((fun (_,b,_,_) -> b) (List.hd b));
       pn ");"
     end
  | RETURN (i, y, l) ->
     begin
       printl l;
       pt "return " t;
       Term.pprint i;
       pn ";";
     end
  | BLOCK (a, _, l) ->
     printl l;
     pt "{" t;
     pn "";
     pprint (t+1) a;
     pt "}" t;
     pn "";
  | DECL (a, len, init_data, _, l) ->
     printl l;
     pt (string_of_decl a len) t;
     if init_data <> INIT_E then p " = ";
     print_init init_data;
     pn ";";
  | ASSIGN (a, b, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = ";
       Term.pprint b;
       pn ";";
     end
  | IF (a, b, c, _, l) -> begin
      let tb = match b with BLOCK _ -> t | _ -> t+1 in
      let tc = match c with BLOCK _ -> t | _ -> t+1 in
      printl l; pt "if(" t; BExp.pprint a; pn ")"; pprint tb b; printl l; 
      begin
        match c with
          BLOCK (SKIP, _, _) -> pt "\n" t
        | _ ->
           pt "else\n" t; pprint tc c
      end;
    end
  | WHILE (a, bs, b, c, _, l) ->
     begin
       printl l;
       pt "while(" t;
       BExp.pprint a;
       p ")" ;
       pn "";
       pprint t b;
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
     end
  | DISPOSE (a, y, l) -> begin printl l; pt "dispose(" t; Term.pprint a; p ")"; pn ";"; end
  | MALLOC (a, tl, y, l) ->
     begin
       printl l;
       pt "" t;
       Exp.pprint a;
       p " = MALLOC( "; Exp.pprint tl;
       pn " );";
       
     end
;;

let dl = ("", 0) ;;
let null = T.NULL ;;
let _T e = T.EXP e ;;

let corr_id v =
  let v' =
    if String.contains v '.' then
      String.map (function '.' | ':' | '$' -> '_' | c -> c) v
    else
      v
  in
  let v'' =
    if String.length v' > 1 && String.get v' 0 = '@' then
      "GLOBAL__" ^ String.sub v' 1 (String.length v' - 1)
    else
      v' in
  
  v''
;;

module CString = struct
  type t = Str of string
  let compare (Str x) (Str y) = String.compare x y
  let t s = Str (corr_id s)
  let f (Str s) = s
end

let var x attr = E.VAR (corr_id x, attr) ;;
let ret attr = var "__RET__" attr |> _T;;

let global_exception = var "__GLOBAL_EXCEPTION__" [] ;;

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
let return ?(exc=T.zero) is_exc e =
  let p = if is_exc then
            ASSIGN (global_exception, T.zero, RETURN (e, SKIP, dl), dl)
          else
            RETURN (e, SKIP, dl)
  in
  p
;;

let rec join_at_last last = function
    | SKIP -> last
    | ASSIGN (a, b, y, l) -> ASSIGN (a, b, join_at_last last y, l)
    | ASSERT (a, y, l) -> ASSERT (a, join_at_last last y, l)
    | IF (a, b, c, y, l) -> IF (a, b, c, join_at_last last y, l)
    | WHILE (a, bs, b, c, y, l) -> WHILE (a, bs, b, c, join_at_last last y, l)
    | PROCCALL (z, a, b, i, y, l) -> PROCCALL (z, a, b, i, join_at_last last y, l)
    | MUTATION (a, b, c, y, l) -> MUTATION (a, b, c, join_at_last last y, l)
    | LOOKUP (a, b, c, y, l) -> LOOKUP (a, b, c, join_at_last last y, l)
    | DISPOSE (a, y, l) -> DISPOSE (a, join_at_last last y, l)
    | MALLOC (a, tl, y, l) -> MALLOC (a, tl, join_at_last last y, l)
    | BLOCK (a, y, l) -> BLOCK (a, join_at_last last y, l)
    | DECL (a, len, init_data, y, l) -> DECL (a, len, init_data, join_at_last last y, l)
    | RETURN (i, y, l) -> RETURN (i, SKIP, l)
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
    | BLOCK (a, y, l) ->
       let ms, fs = mod_free_var y in
       let ms1, fs1 = mod_free_var a in
       S.union ms ms1, S.union fs fs1
    | DECL (a, len, init_data, y, l) ->
       let ms, fs = mod_free_var y in
       let zs = S.union (S.union (List.map E.fv len |> List.concat |> from_list) (fv_of_init init_data)) fs in
       let ms' = S.remove a ms in
       let zs' = S.remove a zs in
       ms',zs'
    | RETURN (i, y, l) ->
       let ms, fs = mod_free_var y in
       ms, S.union (T.fv i |> from_list) fs
;;

module Proc = Set.Make(String)
         
let rec get_func_call stmt =
  match stmt with
  | SKIP -> Proc.empty
  | ASSIGN (_, _, y, _)
    | ASSERT (_, y, _)
    | MUTATION (_, _, _, y, _)
    | LOOKUP (_, _, _, y, _)
    | DISPOSE (_, y, _)
    | MALLOC (_, _, y, _)
    | DECL (_, _, _, y, _)
    | RETURN (_, y, _) ->
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
    let p = if ty = 1 && not (E.is_array x) then
              DECL (x, [], INIT_E, SKIP, dl)
            else if ty = 1 then
              let l = E.get_array_length x in
              DECL (x, l, INIT_E, SKIP, dl)
            else
              DECL (x, [const ty], INIT_E, SKIP, dl)
    in
    dbgf "VAR" "|(%a)" E.fstr x;
    declared := x::!declared;
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
  p;;

let reset_declaration () =
  declared := List.filter E.is_global !declared
;;

let rec decl_all = function
    [] -> SKIP
  | x::xs ->
     if List.mem x !declared || E.is_param x then
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
;;

let mutation x f e =
  let p = MUTATION (x, f, e, SKIP, dl) in
  p
;;
let lookup x e f =
  let p = LOOKUP (x, e, f, SKIP, dl) in
  let ps = decl_all (x::T.fv e) in
  join_at_last p ps
;;

let call ?(ret=None) x fs =
  match ret with
    None ->
    let p = PROCCALL (ret, x, fs, 0, SKIP, dl) in
    p
  | Some r ->
     let ps = decl_all [r] in
     let p = PROCCALL (ret, x, fs, 0, SKIP, dl) in
     join_at_last p ps
;;

let block b =
  if b = BLOCK (SKIP, SKIP, dl) then
    b
  else
    BLOCK (b, SKIP, dl)
;;

let enblock = function
    BLOCK _ as p -> p
  | p -> block p
;;


let mk_if c p1 p2 =
  IF (c, enblock p1, enblock p2, SKIP, dl)
;;

let mk_assert b =
  ASSERT ([F.(uempty &~ b)], SKIP, dl)
;;

let mk_while b p =
  WHILE (b, [], enblock p, F.empty, SKIP, dl)
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
  let subs_t p =    
    let r = T.substitute (_T u) (_T t) p in
    r
  in
  let subs_b = B.substitute (_T u) (_T t) in
  let subs = substitute u t in
  let rec subs_i = function
      INIT_E -> INIT_E
    | INIT_S t -> INIT_S (subs_t t)
    | INIT_M sl -> INIT_M (List.map subs_i sl) 
  in
  match p with
  | SKIP -> p
  | ASSIGN (a, b, y, l) ->
     ASSIGN (subs_e a, subs_t b, subs y, l)
  | ASSERT (a, y, l) ->
     ASSERT (a, subs y, l)
  | IF (a, b, c, y, l) ->
     IF (subs_b a, subs b, subs c, subs y, l)
  | WHILE (a, bs, b, c, y, l) ->
     WHILE (subs_b a, bs, subs b, c, subs y, l)
  | PROCCALL (z, a, b, i, y, l) ->
     PROCCALL (subs_op z, a, List.map subs_t b, i, subs y, l)
  | MUTATION (a, b, c, y, l) ->
     MUTATION (subs_t a, b, subs_t c, subs y, l)
  | LOOKUP (a, b, c, y, l) ->
     LOOKUP (subs_e a, subs_t b, c, subs y, l)
  | DISPOSE (a, y, l) ->
     DISPOSE (subs_t a, subs y, l)
  | MALLOC (a, tl, y, l) ->
     MALLOC (subs_e a, subs_e tl, subs y, l)
  | BLOCK (a, y, l) ->
     BLOCK (subs a, subs y, l)
  | DECL (a, len, init_data, y, l) ->
     if a = u || a = t then
       p
     else
       DECL (a, List.map subs_e len, subs_i init_data, subs y, l)
  | RETURN (i, y, l) ->
     RETURN (subs_t i, subs y, l)

let addfvs s fvs =
  S.union s (S.of_list fvs)
;;

let addfv (r,s) fvs =
  (r, addfvs s fvs)
;;

let get_declared_var = function
    DECL (v, _,_,_,_) -> [v]
  | _ -> []

let restore_prog structures p =
  let rec restore_prog locals p =
    match p with
    | SKIP -> (S.empty, S.empty), p
    | ASSIGN (a, T.EXP ((E.VAR _) as b), y, l) when E.is_ptr a && E.is_ptr b && E.is_param b ->
       let y' = substitute a b y in
       let (r,s), y'' = restore_prog locals y' in
       (S.add a r, addfvs s (E.fv b)), y''
    | ASSIGN (a, b, y, l) ->
       let (r, s), y' = restore_prog locals y in
       (r, addfvs s (T.fv b)), ASSIGN (a, b, y', l)
    | ASSERT (a, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (F.fv (List.hd a)),  ASSERT (a, y', l)
    | DECL (x, d1, d2,
            IF (a,
                BLOCK ((ASSIGN(x1, T.EXP (E.CONST 1),SKIP,_) as p1), SKIP, _),
                BLOCK ((ASSIGN(x2, T.EXP (E.CONST 0),SKIP,_) as p2), SKIP, _),
                ASSIGN (from_blk, blk, y, l1), l),d3) when x1=x2 && x=x1 ->
       begin match y with
       | WHILE (b, _, _, _, _, _) |
           IF (b, _, _, _, _) |
             ASSERT ((_,b::_,_,_)::_, _, _)  when a=b ->
          let y1 = ASSIGN (from_blk, blk, y, l1) in
          let (r, s), y' = restore_prog locals y1 in
          if E.is_global x1 || E.is_param x1 || S.mem x1 s then
            let s' = S.add x s in
            (r, addfvs s' (B.fv a)), DECL (x, d1, d2, IF (a, p1, p2, y', l), d3)
          else
            (r,s), y'
       | SKIP ->
          (S.empty, S.empty), ASSIGN (from_blk, blk, SKIP, l1)
       | _ ->
          let y1 = ASSIGN (from_blk, blk, y, l1) in
          let (r, s), y' = restore_prog locals y1 in
          let s' = S.add x s in
          (r, addfvs s' (B.fv a)), DECL (x, d1, d2, IF (a, p1, p2, y', l), d3)
       end
    | IF (a, b, c, y, l) ->
       let (r1, s1), b' = restore_prog [] b in
       let (r2, s2), c' = restore_prog [] c in
       let (r3, s3), y' = restore_prog locals y in
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
              (r, addfvs s (z'::fv_b)), PROCCALL (z, a, b, i, y', l)
       end
    | MUTATION (a, b, c, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv a @ T.fv c), MUTATION (a, b, c, y', l)
    | LOOKUP (a, b, c, y, l) ->
       let (r, s), y' = restore_prog locals y in
       if !noop || E.is_global a || E.is_param a || S.mem a s then
         (r, addfvs s (T.fv b)), LOOKUP (a, b, c, y', l)
       else
         (r, s), y'
    | DISPOSE (a, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv a), DISPOSE (a, y', l)
    | MALLOC (a, tl, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (a::E.fv tl), MALLOC (a, tl, y', l)
    | BLOCK (BLOCK (a, SKIP, l), y, _)
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
             (r, s'), DECL (a, len, init_data, y', l)
         in
         
         if init_data = INIT_E then
           begin
             let is_not_in y = let _, fvs = mod_free_var y in
                                   not (S.mem a fvs) in
             match y with
             | MALLOC (c1, tl,
                       ASSIGN (p, c2, y, _), _) when a=c1 && c1=T.toExp c2 && is_not_in y ->
              if E.toStr a = "call13" then
                begin
                  if !noop then pn "noop:true" else pn "noop:false";
                  if init_data = INIT_E then pn "init:<>" else pn "init:...";
                end;
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
             | _ ->
                begin
                  if not !noop then
                    match y with
                    | IF (b,
                          BLOCK (ASSIGN (c1, T.EXP(E.CONST 1), SKIP, _), SKIP, _),
                          BLOCK (ASSIGN (c2, T.EXP(E.CONST 0), SKIP, _), SKIP, _),
                          DECL (cv'', _, INIT_E,
                                ASSIGN (cv', cp,
                                        ASSERT ([(_,[B.UNIT (cv, Op.NE, T.EXP (E.CONST 0))],[],[])],
                                                y,_),_), _),
                          _) when T.toExp cv=cv' && T.toExp cp=a && a=c1 && a=c2 && is_not_in y ->
                       restore_prog locals (ASSERT ([([],[b],[],[])],y,l))
                    | LOOKUP (a1, pt, fld,
                              ASSIGN (c, a2, y, _), _) when a=a1 && a=T.toExp a2 && is_not_in y ->
                       let p = LOOKUP (c, pt, fld, y, l) in
                       
                       restore_prog locals (p)
                    | _ ->
                       deal_decl ()
                  else
                    deal_decl ()
                end
           end
         else
           deal_decl ()
       end
    | RETURN (i, y, l) ->
       let r, y' = restore_prog locals y in
       addfv r (T.fv i), RETURN (i, y', l)
  in
  restore_prog [] p


let adjust_ptr vars p =
  let module VR = Map.Make(CString) in

  let term_to_ref b =
    let fvb = T.fv b in
    let pre, v_map =
         List.fold_left (fun (pre, v_map) v ->
             let vn, attr = E.decode v in
             let vn' = CString.t vn in
             if VR.mem vn' vars then
               let attr' = VR.find vn' vars in
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
    | ASSIGN (a, b, y, l) ->
       let y' = adjust_ptr locals y in
       let b', pre = term_to_ref b in
       let p =
         let an, attr = E.decode a in
         let an' = CString.t an in
         if VR.mem an' vars then
           let attr' = VR.find an' vars in
           if not (E.is_ptr a) && List.mem E.PTR attr' then
             join_at_last y' (mutation (_T a) "*" b') 
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
  in
  adjust_ptr [] p


let adjust_calls aux_calls fname p =
  let module VR = Map.Make(String) in
  let dc = function
      DECL (v, _, _, _, _) ->
      v
    | _ -> raise Not_found
  in
  
  let rec adjust_calls d p =
    match p with
    | SKIP -> p
    | ASSIGN (a, b, y, l) ->
       let y' = adjust_calls d y in
       ASSIGN (a, b,y',l)
    | ASSERT (a, y, l) ->
       let y' = adjust_calls d y in
       ASSERT (a, y', l) 
    | IF (a, b, c, y, l) ->
       let b' = adjust_calls d b in
       let c' = adjust_calls d c in
       let y' = adjust_calls d y in
       IF (a, b', c', y', l)
    | WHILE (a, bs, b, c, y, l) ->
       let b' = adjust_calls d b in
       let y' = adjust_calls d y in
       WHILE (a, bs, b', c, y', l)
    | PROCCALL (z, a, b, i, y, l) ->
       begin
         let y' = adjust_calls d y in
         try
           let sa, attr = E.decode (T.toExp a) in
           let (_, _, args) = List.find (fun (fname',blk_name,_) -> fname=fname' && blk_name=sa) aux_calls in
           let a' = _T @@ E.encode (sa, attr) in
           let params = List.map dc d in
           let args',bd = List.fold_left (fun (args, bd) arg ->
                           try
                             let vn = T.toStr arg in
                             let param = List.find (fun x -> E.var_decode x = vn) params in
                             if E.is_ptr param then
                               let fresh_v = fresh_var [] in
                               let p = DECL (fresh_v, [], INIT_E, LOOKUP (fresh_v, T.enptr arg, "*", bd, dl), dl) in
                               args@[_T fresh_v], p
                             else
                               args@[arg], bd
                           with
                             _ -> args@[arg], bd
                         ) ([],SKIP) args in
           join_at_last (PROCCALL (z, a', b@args', i, y', l)) bd
         with
           Not_found ->
        PROCCALL (z, a, b, i, y', l)
       end
    | MUTATION (a, b, c, y, l) ->
       let y' = adjust_calls d y in
       MUTATION (a, b, c, y', l)
    | LOOKUP (a, b, c, y, l) ->
       let y' = adjust_calls d y in
       LOOKUP (a, b, c, y', l)
    | DISPOSE (a, y, l) ->
       let y' = adjust_calls d y in
       DISPOSE (a, y', l)
    | MALLOC (a, tl, y, l) ->
       let y' = adjust_calls d y in
       MALLOC (a, tl, y', l)
    | BLOCK (a, y, l) ->
       let a' = adjust_calls d a in
       let y' = adjust_calls d y in
       BLOCK (a', y', l)
    | DECL (a, len, init_data, y, l) ->
       let y' = adjust_calls d y in
       DECL (a, len, init_data, y', l)
    | RETURN (i, y, l) ->
       let y' = adjust_calls d y in
       RETURN (i, y', l)
  in
  p

let rec ends_with_ret = function
    | SKIP -> false
    | ASSIGN (_, _, y, _)
    | ASSERT (_, y, _)
    | IF (_, _, _, y, _)
    | WHILE (_, _, _, _, y, _)
    | PROCCALL (_, _, _, _, y, _)
    | MUTATION (_, _, _, y, _)
    | LOOKUP (_, _, _, y, _)
    | DISPOSE (_, y, _)
    | MALLOC (_, _, y, _)
    | DECL (_, _, _, y, _)
      ->
       ends_with_ret y
    | BLOCK (y1, y2, _) ->
       ends_with_ret y1 || ends_with_ret y2
    | RETURN (_, _, _) ->
       true
    
let call_to_ret attr is_exc = function
  | PROCCALL (None, fn, args, i, y, l) ->
     let fv = fresh_var attr in
     let d  = decl fv 1 in
     let p  = PROCCALL (Some fv, fn, args, i, y, l) in
     let r  = return is_exc @@ _T fv in
     block @@ join_progs [d;p;r]
  | PROCCALL (Some v, fn, args, i, y, l) as p ->
     let r  = return is_exc @@ _T v in
     block @@ join_progs [p;r]
  | p -> p

let ret_merge = function
    IF (b,
        BLOCK(
            DECL(v1,a1,b1,
                 PROCCALL (Some v1',fn1,arg1,i1,
                           RETURN (T.EXP v1'',SKIP,l2),
                           l4),
                 l1),
            SKIP,_),
        BLOCK(
            DECL(v2,_,_,
                 PROCCALL (Some v2',fn2,arg2,i2,
                           RETURN (T.EXP v2'',SKIP,_),
                           l5) ,
                 _),
            SKIP,_),
        SKIP, l) when v1=v1' && v1'=v1'' && v2=v2' && v2'=v2''->
    DECL(v1,a1,b1,
         IF (b,
             PROCCALL (Some v1',fn1,arg1,i1,SKIP,l4),
             PROCCALL (Some v1',fn2,arg2,i2,SKIP,l5),
             RETURN (T.EXP v1'',SKIP,l2),l
           )
         ,l)
  
  | p -> p

let rec traverse f f1 acc p =
  match p with 
    SKIP -> acc
   | ASSERT (_, p', _) 
    | ASSIGN (_, _, p', _)
    | PROCCALL (_, _, _, _, p', _)
    | MALLOC (_, _, p', _)
    | MUTATION (_, _, _, p', _)
    | LOOKUP (_, _, _, p', _)
    | DISPOSE (_, p', _)
    | DECL (_, _, _, p', _)
    | RETURN (_, p', _)
    -> traverse f f1 (f acc p) p'
  | IF (_, p1, p2, p', _)
    -> let acc' = f acc p in
       let acc1 = traverse f f1 (f1 acc acc') p1 in
       let acc2 = traverse f f1 (f1 acc acc1) p2 in
       let acc3 =  traverse f f1 (f1 acc acc2) p' in
       acc3
  | WHILE (_, _, p1, _, p', _)
    | BLOCK (p1, p', _)
    ->
     let acc' = f acc p in
     let acc1 = traverse f f1 (f1 acc acc') p1 in
     let acc2 = traverse f f1 (f1 acc acc1) p' in
     acc2

let has_assert p =
  traverse
    (fun acc -> function | ASSERT _ -> true
                                | _ -> acc)
    (fun a b -> a || b)
    false
  p
           
let get_attribute p =
  let f _ = function
      RETURN (t, _, _) ->
      let attr = try
          T.head "" t
        |> E.get_attributes
        with
          _ -> []
      in
      attr
    | _ -> []
  in
  let f1 a b = b in
  traverse f f1 [] p

let rec next p =
  match p with 
    SKIP -> raise Not_found
  | ASSERT (_, p', _)
  | ASSIGN (_, _, p', _)
  | PROCCALL (_, _, _, _, p', _)
  | MALLOC (_, _, p', _)
  | MUTATION (_, _, _, p', _)
  | LOOKUP (_, _, _, p', _)
  | DISPOSE (_, p', _)
  | DECL (_, _, _, p', _)
  | RETURN (_, p', _)
  | IF (_, _, _, p', _) 
  | WHILE (_, _, _, _, p', _)
  | BLOCK (_, p', _) -> p'

let rec filter f p =
  if p = SKIP then SKIP else
  let n, s = f p in
  let p'' = filter f n in
  if not s then
    p''
  else
    match p with 
      SKIP -> SKIP
    | ASSERT (a, _, l) -> ASSERT (a, p'', l)
    | ASSIGN (a, b, _, l) -> ASSIGN (a, b, p'', l)
    | PROCCALL (a, b, c, d, _, l) -> PROCCALL (a, b, c, d, p'', l)
    | MALLOC (a, b, _, l) -> MALLOC (a, b, p'', l) 
    | MUTATION (a, b, c, _, l) -> MUTATION (a, b, c, p'', l) 
    | LOOKUP (a, b, c, _, l) -> LOOKUP (a, b, c, p'', l)
    | DISPOSE (a, _, l) -> DISPOSE (a, p'', l) 
    | DECL (a, b, c, _, l) -> DECL (a, b, c, p'', l)
    | RETURN (a, _, l) -> RETURN (a, p'', l)
    | IF (a, p1, p2, _, l) -> IF (a, filter f p1, filter f p2, p'', l)
    | WHILE (a, b, p1, c, _, l) ->
       let p1' = filter f p1 in
       WHILE (a, b, p1', c, p'', l)
    | BLOCK (p1, _, l) ->
       let p1' = filter f p1 in
       BLOCK (p1', p'', l)

let rem_decl p =
  filter (function DECL _ as p -> next p, false | p -> next p, true) p

let rem_lib_fun p =
  let p' =
  filter (function
        (DECL (x1, len, _,
              LOOKUP (x2, _, "*",
                      PROCCALL (_, proc, params, _, n, _), _), _)) as p
           when x1=x2 ->
        
         let p_name = E.var_decode @@ T.toExp proc in
         if p_name = "fprintf" && x1 = T.toExp @@ List.nth params 1 then
           begin  (n, false) end
         else if p_name = "perror" && x1 = T.toExp @@ List.nth params 0 then
           begin  (n, false) end
         else if p_name = "_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC1EPKcRKS3_" && x1 = T.toExp @@ List.nth params 1 then
           begin  (n, false) end
         else
           begin (next p, true) end
      | p -> next p, true
    ) p in
  p'


let rec get_fp_args proc_names body =
  let f = (fun acc ->
      function
        PROCCALL (_, a, args, _, _, _) ->
         let (_, args') = List.fold_left
                            (fun (i,acc) arg ->
                              let earg = Term.toExp arg in
                              let sa = Exp.toStr earg in
                              if List.exists (fun pn -> pn=sa) proc_names then
                                (i+1, (a,i,sa)::acc)
                              else if E.is_func earg then
                                (i+1, (a,i,sa)::acc)
                              else
                                (i+1, acc)
                            ) (0,[]) args in
         acc @ args'
      | _ -> []
    ) in
  let f1 a b = a @ b in
  traverse f f1 [] body
  
let rec replace_fp p_map p =
  let module S = Map.Make(String) in
  match p with 
    SKIP -> SKIP
  | ASSERT (a, p', l) ->
     ASSERT (a, replace_fp p_map p', l)
  | ASSIGN (a, b, p', l) ->
     ASSIGN (a, b, replace_fp p_map p', l)
  | PROCCALL (z, a, args, b, p', l) ->
     let sa = (Term.toStr a) in
     if S.mem sa p_map then
       let fp_args = S.find sa p_map in
       let fp_arg = List.hd fp_args in
       PROCCALL (z, Term.EXP (Exp.VAR (fp_arg,[])), args, b, replace_fp p_map p', l)
     else
       PROCCALL (z, a, args, b, replace_fp p_map p', l)
  | MALLOC (a, b, p', l) ->
     MALLOC (a, b, replace_fp p_map p', l)
  | MUTATION (a, b, c, p', l) ->
     MUTATION (a, b, c, replace_fp p_map p', l)
  | LOOKUP (a, b, c, p', l) ->
     LOOKUP (a, b, c, replace_fp p_map p', l)
  | DISPOSE (a, p', l) ->
     DISPOSE (a, replace_fp p_map p', l)
  | DECL (a, b, c, p', l) ->
     DECL (a, b, c, replace_fp p_map p', l)
  | RETURN (a, p', l) ->
     RETURN (a, replace_fp p_map p', l)
  | IF (b, p1, p2, p', l) ->
     IF (b, p1, p2, replace_fp p_map p', l)
  | WHILE (b, c, p1, d, p', l) ->
     WHILE (b, c, p1, d, replace_fp p_map p', l)
  | BLOCK (p1, p', l) ->
    BLOCK (p1, replace_fp p_map p', l)      

                  
