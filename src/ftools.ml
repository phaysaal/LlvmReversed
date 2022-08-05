exception Error
exception SError
exception StError of string
exception NoRealizationForQuantifier of string
exception MemoryLeak of string
exception IndexOutOfBound of string
exception NoProc of string
exception NullPointerException of string
exception ExpToBExp
exception NotAVariable of string
                        
let warning : ((string * string * int list) list ref) = ref []  ;;

let warn message (filename, lineno) o =
	if List.exists (
         fun (x, f, ls) -> message = x && filename = f && List.exists (fun l' -> l' = lineno) ls
       ) (!warning) then
		o
	else
	 begin
    (*			print_string ("WARNING" ^ s ^ (Locs.toStr l) ^ "\n"); *)
    if List.exists (fun (x, f, _) -> x = message && filename = f) !warning then
      begin
			  warning := List.map (fun (x, f, ls) -> if x = message then (x, f, ls @ [lineno]) else (x, f, ls)) !warning
      end
    else
      begin
        warning := (!warning) @ [(message, filename, [lineno])]
		  end;
    o
  end

let error : (string * (string * int) list) list ref = ref []  ;;

let e_warn message (filename, lineno) =
	if List.exists (fun (x, ls) -> message = x && List.exists (fun (f, _) ->filename = f) ls) (!error) then
		()
	else
	 begin
    (*			print_string ("WARNING" ^ s ^ (Locs.toStr l) ^ "\n"); *)
    if List.exists (fun (x, _) -> x = message) !error then
      begin
			  error := List.map (fun (x, ls) -> if x = message then (x, ls @ [(filename, lineno)]) else (x, ls)) !error
      end
    else
      begin
        error := (!error) @ [(message, [(filename, lineno)])]
		  end;
    ()
  end

let dott : float ref = ref 0.0;;
let e_print _ =
  List.iter (fun (m, ls) ->
      prerr_endline m;
      prerr_string "    ["; List.iter (fun (f, l) -> prerr_string "("; prerr_string f; prerr_string ","; prerr_int l; prerr_string ")") ls; prerr_string "]"; prerr_endline ".") !error;;

let funcdir = ref "";;
let cfunc = ref "";;

let max_filename = 150;;
(*
let kmod = ref "";;
 *)

let l_or lst = List.fold_left (||) false lst

let l_and lst = List.fold_left (&&) true lst

(* let (.) f g x = g x >> f *)

let rec (|.|) f = function
	| [] -> raise SError
	| [x] -> x
	| h :: t -> f h (f |.| t)

let (><) f g o x = (f x) o (g x)

let mapf o fs x = List.map ((|>) x) fs |> o

let (|>>|) f vs = List.map f vs

let (|>-) f vs = List.filter f vs

let (|->>) f (a, vs) = List.fold_left f a vs

let (|<-) k xs = List.exists (fun x -> x = k) xs;;

let num = ref 0

let addr = ref 1

let newaddr _ = addr := (!addr + 8); (string_of_int !addr)

(* let (:=) z y x = if x = z then y else x *)

let rec valid = function
	| [] -> None
	| Some x :: _ -> Some x
	| None :: t -> valid t

let rec valids_only = function
	| [] -> []
	| None :: t -> valids_only t
	| Some x :: t -> x :: valids_only t

(** Drop common elements from two lists *)
let rec drop_common xs = function
	| [] 		-> (xs, [])
	| h::t 	->
		let (a, b) = List.partition ((=) h) xs in
		begin
			match a with
			| [] -> let (c, d) = drop_common b t in (c, h::d)
			| _ ->  let (c, d) = drop_common b t in (c, d)
		end

let rec common xs = function
	| [] 		-> []
	| h::t 	->
		let (a, b) = List.partition ((=) h) xs in
		begin
			match a with
			| [] -> let c = common b t in c
			| _ ->  let c = common b t in h::c
		end

let rec has_duplicate = function
	| [] -> false
	| x::xs -> (x |<- xs) || (has_duplicate xs)

let p str = (* prerr_string *) print_string str
let pn str = (* prerr_endline *) print_endline str
let pw str = p str; p " "
let pi str = pn (string_of_int str)
let pl str = p (string_of_int str)
let pb b = if b then p "True" else p "False"


let p_opt : string list ref = ref []

let pf_s opt f s = if (opt |<- !p_opt) || ("ALL" |<- !p_opt) then f s

let p_s opt s = pf_s opt p s

let pn_s opt s = pf_s opt pn s

let pf_s' opt s = pf_s opt (fun x -> x) s (* Q: isn't it same as p_s? No problem if its usage different. *)

let dbg t c f d =
  p_s t c;
  p_s t " ";
  pf_s t f d;
  pn_s t "";;

let dbgf tag =
  Printf.ksprintf (fun s -> if tag |<- !p_opt then print_endline s else ())
;;

let dbgc fn =
  Printf.ksprintf (fun s -> if fn then print_endline s else ())
;;

let redot () =
  dott.contents <- Sys.time ();;

let dot () =
  let tt = Sys.time () in
  if tt -. !dott > 5.0 then
    begin
      prerr_string "."; flush stderr;
      redot ();
    end
  (* dott.contents <- (!dott) + 1;
  if !dott > 3 then
    dott.contents <- 0;
  (* let pos = pos_out stderr in
  seek_out stderr (pos-1); *) 
  let ch =
    match !dott with
      0 -> "."
    | 1 -> "-"
    | 2 -> "."
    | 3 -> "-"
    | _ -> "."
  in *);;

let pp_sep_fmt fmt = fun ppf () -> Format.fprintf ppf fmt

let pp_list_with pp_a fmt ppf l =
  Format.pp_print_list ~pp_sep:(pp_sep_fmt fmt) pp_a ppf l

let pp_print_list pp_a ppf l =
  Format.fprintf ppf "[@[<1>[%a]@]" (pp_list_with pp_a ";@ ") l

let rec pt str = function
	| 0 -> p str
	| n -> p "    "; pt str (n-1)


let rec concatS s f = function
	| [] -> ""
	| [o] -> f o
	| h::t -> (f h) ^ s ^ (concatS s f t)

let fstrL f s () =
  concatS s (f ())

let fstrId = fun () s -> Format.sprintf "%s" s
  
let rec iterS f delim = function
	| [] -> p ""
	| [x] -> f x
	| h::t -> f h; p delim; iterS f delim t

let ps x = p "\""; p x; p "\""

(*
	(x, [a,b,c])  [[(y,p)], [(y,q)]] = [[(x,a),(y,p)],	[(x,b),(y,p)],	[(x,c),(y,p)]] @	[[],	[],	[]]
 *)
let multiply accum (x, xs) = if xs = [] then accum else
	match accum with
	| [] -> (fun a -> [(x, a)]) |>>| xs
	| _ -> (fun h -> ((fun a -> (x, a)::h) |>>| xs)) |>>| accum |> List.concat

let cross ls = (multiply) |->> ([], ls)

(** Set equality *)
let rec (|==|) xs = function
	| [] -> xs = []
	| y::ys ->
		let (z, zs) = List.partition ((=) y) xs in
		z = [y] && zs |==| ys


(** Find all permutations *)
(*
let rec permute2 xs =
  let rec f all pre = function
  	| [] -> (pre, [])::all
  	| x::xs as l -> f ((pre, l)::all) (pre @ [x]) xs
  in
  let insert x xs = (fun (ws,vs) -> ws @ (x::vs)) |>>| (f [] [] xs) in
  match xs with
  | [] -> []
	| z::[] -> [[z]]
  | z::zs -> List.concat ((insert z) |>>| (permute2 zs))
*)

let rec permute xs =
	let rec del x = function
		| [] -> []
		| y::ys -> if x = y then ys else y::(del x ys)
	in
	match xs with
	| [] -> []
	| [x] -> [[x]]
	| _ -> (fun ac x -> ac @ ((fun zs -> x::zs) |>>| (permute (del x xs)))) |->> ([], xs)


let (>><<) xs ys =
	let f zs x = (fun z -> (z,x)) |>>| zs in
	List.flatten ((f xs) |>>| ys)

let uniq xs =
  let rec uniq' ys = function
      [] -> ys
    | x::xs -> if x |<- ys then uniq' ys xs else uniq' (x::ys) xs
  in
  List.rev (uniq' [] xs)


let print_warning fname =
  if (List.length !warning) <> 0 then
    begin
    p fname; p ": \n";
    List.iter (fun (x, _, ls) ->
          p x;
          p " at line(s) ";
          iterS pl ", " ls;
          pn ".";
        ) !warning
    end

let erase_warning _ = warning.contents <- [];;

let (@@@) x y =
  let x' = (fun x1 -> not (x1 |<- y)) |>- x in
  x' @ y;;

let cassert b c =
  if b then
    ()
  else
    raise (StError ("Assert failed: " ^ c))

let is_main mn =
  let res1 = String.length mn > 5 in
  if res1 then    
    begin
      let res2 = String.sub mn 0 5 = "main@" in
      res2 
    end
  else
    begin
    false
    end

let _List_hd ls =
  try
    List.hd ls
  with
    _ -> raise (StError "LIST HD PROBLEM")

let concat = List.concat


let rec parallel_subs f e = function
    [] -> e
  | (to_be, by)::xs ->
     let matched, unmatched = List.partition (fun (to_be', _) -> to_be' = by) xs in
     if List.length matched = 0 then 
       let e' = f to_be by e in
       parallel_subs f e' xs
     else
       parallel_subs f e (matched@((to_be, by)::unmatched))

let opcat s1 s2 =
  match s1, s2 with
    None, _ -> s2
  | _, None -> s1
  | Some x1, Some x2 -> Some (x1@@@x2)

let op_p f = function
    Some a -> f a
  | None -> p "<NONE>"

let print_pairs p1 sp p2 spn data =
  iterS (fun (a,b) -> p1 a; p sp; p2 b) spn data
  
let rec take n = function
    [] when n = 0 -> []
  | [] -> raise (StError ((string_of_int n) ^ " is more than the length of list"))
  | _::_ when n = 0 -> []
  | x::xs -> x::take (n-1) xs;;

let string_of_list f xs =
  let rec aux = function
      [] -> ""
    | [x] -> f x
    | x::xs ->
       f x ^ ";" ^ aux xs
  in
  "[" ^ aux xs ^ "]"


let print_list f xs =
  let rec aux = function
      [] -> p ""
    | [x] -> f x
    | x::xs ->
       f x; p ";"; aux xs
  in
  p "["; aux xs; p "]"
;;

let print_gc stat =
  pw "major words:";
  print_float stat.Gc.major_words;
  pw "minor words:";
  print_float stat.Gc.minor_words;
  pw ", major collections:";
  pl stat.Gc.major_collections;
  pw ", heap words:";
  pl stat.Gc.heap_words;
  pw ", free words:";
  pl stat.Gc.free_words;
  pw ", live words:";
  pl stat.Gc.live_words;
  pw ", live blocks:";
  pl stat.Gc.live_blocks;
  pw ", free blocks:";
  pl stat.Gc.free_blocks;
  pw ", compaction:";
  pl stat.Gc.compactions;
  pn "\n";
  
;;

let corr_filename s =
  if String.length s > max_filename then
    String.sub s (String.length s-max_filename) max_filename
  else
    s

let flatten_path str =
  let s = String.map (fun c -> if c = '/' || c = '.' then '_' else c) str  in
  corr_filename s
  
let make_path dir tag name =
  dir ^ "/" ^ (flatten_path tag) ^ "___" ^ name

let new_prog_var _ = num.contents <- (!num + 1); !cfunc ^ "#_" ^ (string_of_int !num);;

let write_file filename data =
  let fout = open_out (filename) in
  Marshal.to_channel fout data [];
  close_out fout;;

let read_file filename =
  let fin = open_in (flatten_path filename) in
  try
    let data = Marshal.from_channel fin in
    close_in fin;
    data
  with
    End_of_file ->
    close_in_noerr fin;
    raise End_of_file
  | e ->
     close_in_noerr fin;
     raise e
;;

(* Reading all files in dirpath + left-folding *)
(* Give a function f : <D:folded data> -> <A:unMarchaled data> -> <A*D: folded data> *)
(* Type information is necessary *)
let read_and_fold (f: 'a-> 'b -> 'a)  (v: 'a) dirpath =
  (* Note: an unMarshaled file has type 'b and a folded data has type 'a *)
  let counter = ref 0 in
  let files_arr = Sys.readdir dirpath in
  Array.sort String.compare files_arr;
  let files = Array.to_list files_arr in
  print_int (List.length files); print_string " files: "; flush_all ();
  let f1 dat file =
    let filepath = dirpath ^ "/" ^ file in
    if !counter < 10 then counter := !counter + 1 else (counter := 0;print_string ".";flush_all ());
    if Sys.is_directory filepath
    then dat
    else 
      begin
        dbg "DONES" "Reading:" p filepath;
        let b = read_file filepath in
        f dat b
      end
  in
  let res = List.fold_left f1 v files in
  print_newline ();
  res
;;

(* Reading all files in dirpath. *)
(* It returns a list of unMarchaled files (type information is necessary) *)
let read_allfiles dirpath = read_and_fold (fun bL b -> b :: bL) [] dirpath
;;


let is_debug = ref false;;

let pause msg =
  if !is_debug then
    begin
      if msg = "" then pn "Paused. Press ENTER to continue." else pn msg;
      let _ = read_line () in
      ()
    end
  else
    ()
;;


let foldri n f : 'a list =
  let rec foldri acc = function
      0 -> acc
    | i -> foldri (f (i-1) :: acc) (i-1)
  in
  foldri [] n
;;

let split delim (str : string) : string list =
  if str = "" then
    []
  else
    let ls : char list = foldri (String.length str) (fun i -> String.get str i) in
    let to_string l_chars =
      (fun s c -> s ^ (String.make 1 c)) |->> ("", l_chars)
    in
    let f (prevs, cur) c =
      if c = delim then
        (to_string (List.rev cur)::prevs, [])
      else
        (prevs, c::cur)
    in
    let (prevs, cur) = f |->> (([],[]), ls) in
    List.rev (to_string (List.rev cur)::prevs)
;;

let corr_structname s =
  String.map (function '.' -> '_' | c -> c) s

let corr_fieldname f =
  let i =  Char.code (String.get f 0) in
  if Char.code '0' <= i && i <= Char.code '9' then
    "field_" ^ f
  else
    f
