open Int64
open Ftools

(** This module is for handling commandline arguments to handle analysis behavior and output *)
module Options = struct
  let to_be_verified = ref true
  let to_be_error_printed = ref false
  let z = ref 0
  let to_be_code_printed = ref false
  let to_be_report_printed = ref false
  let filename = ref ""
  let unknowns: string list ref = ref []
  let manuals : string ref = ref ""
  let functions: string ref = ref ""
  let param: (string * int) option ref = ref None
  let is_old = ref true
  let in_place_check = ref false
  let to_be_entl_printed = ref false
  let old_sat = ref false
  let entailment = ref false
  let show_types = ref false
  let hard_mode = ref false
  let trace_function = ref ""
  let timeout = ref 0.0
  let count = ref 0
  let is_bsf = ref false
  let is_no_evals = ref false
  let debfun = ref ""
  let dump_to = ref ""
  let read_from = ref ""
  let pre_to = ref ""
  let pre_from = ref ""
  let interactive = ref false
  let interactive_ref = ref false
  let recompute_specs = ref false
  let catch_exception = ref false
  let pause = ref false
  let fp_json_file = ref ""
end;;


module Shared = struct

  let get_translated_dir slacDataDir =  slacDataDir ^ "/Translated";;
  let get_fpa_dir slacDataDir =  slacDataDir ^ "/Fpa";;
  let get_fpa_fundef_dir fpaDir =  fpaDir ^ "/Fundef";;
  let get_fpa_global_data_dir fpaDir =  fpaDir ^ "/GlobalData";;
  let get_fpa_profile_dir fpaDir = fpaDir ^ "/Profiles";;
  let init_dir tempdir = tempdir ^ "/fold";;
  let struct_file tempdir = tempdir ^ "/fold/structs.slac";;
  let az_file tempdir = tempdir ^ "/fold/az.slac";;
  let r_file tempdir = tempdir ^ "/fold/r.slac";;
  let ret_file tempdir = tempdir ^ "/fold/ret.slc";;

  let rec sub_str a b =
    if String.length a > 1 && String.length b > 1 then
      if String.get a 0 = String.get b 0 then
        sub_str (String.sub a 1 (String.length a - 1)) (String.sub b 1 (String.length b - 1))
      else
        a
    else
      a
  ;;
  

  let builtins = ["__builtin_bswap32";
                  "__builtin_bswap64";
                  "__builtin_constant_p";
                  "__builtin_object_size";
                  "__builtin_expect";
                  "__builtin_va_arg";
                  "__builtin_va_arg_pack";
                  "__builtin_va_arg_pack_len";
                  "__builtin_va_end";
                  "__builtin_va_start";
                  "__builtin_va_copy";
                  "__builtin___sprintf_chk";
                  "__builtin___vsprintf_chk";
                  "__builtin___snprintf_chk";
                  "__builtin___vsnprintf_chk";
                  "__builtin___memmove_chk";
                  "__builtin___memset_chk";
                  "__builtin___memcpy_chk";
                  "__builtin___mempcpy_chk";
                  "__builtin_mul_overflow_p";
                  "__builtin___strcpy_chk";
                  "__builtin___stpcpy_chk";
                  "__builtin___strncpy_chk";
                  "__builtin___strcat_chk";
                  "__builtin___strncat_chk";
                  "__builtin_strspn";
                  "__builtin_types_compatible_p";
                  "__builtin_unreachable";
                  "__builtin_ctz";
                  "__builtin_ctzll";
                  "__builtin_alloca";
                  "__builtin_strchr";
                  "__builtin_strcmp";
                  "__builtin_strlen";
                  "__builtin_strcspn";
                  "__atomic_fetch_sub";
                  "__atomic_thread_fence";
                  "__atomic_fetch_add";
                  "__atomic_load";
                  "__atomic_store";
                  "__atomic_is_lock_free";
                  "__atomic_add_fetch";
                  "printf";
                  "calloc";
                  "cfree"; "reallocate_array"; "exit"; "strcpy"; "strcmp"; "abs";
                  "__dummy-for-asm-statement__"
                 ];;
end;;

(** Line numbers are handled *)
module Locs = struct
  type t = string * int

  let dummy = ("", 0)

  let pp ppf (fn, ln) =
    Format.fprintf ppf "@[<1>(\"%s\",@,%d)@]" fn ln

  let print (fn, ln) =
    "(\"" ^ fn ^ "\"," ^ (string_of_int ln) ^ ")" 

  let to_str (loc : t) = "line " ^ (string_of_int (snd loc)) ^ " in " ^ (fst loc)

  let fstr () (fn, ln) = Format.sprintf "line %s in %d\n" fn ln 
                       
 	let to_line (_, l) = (string_of_int l)
end;;

(** Various operators. Caution for use in specific place *)
module Op = struct
		type t = ADD | SUB | MUL | DIV | MOD | EQ | NE | LE | OR | AND | DUMMY | SHL | SHR | BOR | BAND | MAPSTO | MIN | MAX | XOR
      [@@deriving show {with_path = false}]
                                                                                                 
		let pprint = function
			| ADD -> "+"
			| SUB -> "-"
			| MUL -> "*"
			| DIV -> "/"
			| MOD -> " % "
			| EQ -> "=="
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

    let pp' ppf op = Format.pp_print_string ppf (pprint op)

    let fstr () op = Format.sprintf "%s" (pprint op)

    let print = function
			| ADD -> "ADD"
			| SUB -> "SUB"
			| MUL -> "MUL"
			| DIV -> "DIV"
			| MOD -> "MOD"
			| EQ -> "EQ"
			| NE -> "NE"
			| LE -> "LE"
			| OR -> "OR"
			| AND -> "AND"
			| SHL -> "SHL"
			| SHR -> "SHR"
			| BAND -> "BAND"
			| BOR -> "BOR"
      | XOR -> "XOR"
			| MAPSTO -> "MAPSTO"
      | MIN -> " MIN"
      | MAX -> " MAX"
			| DUMMY -> "DUMMY"
               
    let smt_print = function
			| ADD -> "+"
			| SUB -> "-"
			| MUL -> "*"
			| DIV -> "/"
			| MOD -> "mod"
			| EQ -> "="
			| NE -> "distinct"
			| LE -> "<"
			| OR -> "or"
			| AND -> "and"
			| SHL -> "<<"
			| SHR -> ">>"
			| BAND -> "and"
			| BOR -> "or"
			| MAPSTO -> "-:"
      | MIN -> " MIN "
      | MAX -> " MAX "
      | XOR
			| DUMMY -> "^^"

end;;

module Exp = struct

  (** Variables with its type/attributes *)
  type attr = PTR | STRUCT of string | EXQ | ARRAY of t list | PARAM | PTRPTR | GLOBAL | HAT | BAR | EXTERN | FUNCPTR of attr list * attr list | TILDE | CHECK | DOT | NESTED | QUESTION | DOTDOT | ACUTE | INDIRECT | STATIC | SIMPLE of int | FUNC of attr list * attr list

  and var_t = string * attr list

  and t =
     NOTHING
   | NEGINF
   | POSINF
   | UNDEF
	 | VAR of var_t
	 | CONST of int
	 | FLOAT of float
   | STRING of string
	 | BINOP of t * Op.t * t
   | INDICES of t list (** We will use indices for Ifz *)
   | ADDR of t
   | REF of t
   | NEG of t
   | ARROW of t * string
   | LBL of string * t
   | FCALL of string * t list
   | OFFSET of string * t
   | SIZEOF of string

  module V = Map.Make(String)

  exception VarNotFound of string;;
           
  let var = fst
          
  let is_ptr = function VAR (_, attrs) -> List.exists (function PTR -> true | _ -> false) attrs | _ -> false
                          
  let is_ptrptr = function VAR (_, attrs) ->  List.exists (fun x -> match x with PTRPTR -> true | _ -> false) attrs | _ -> false
                           
  let is_struct = function VAR (_, attrs) -> List.exists (fun x -> match x with STRUCT _ -> true | _ -> false) attrs | _ -> false

  let is_simple = function VAR (_, attrs) -> List.exists (fun x -> match x with SIMPLE _ -> true | _ -> false) attrs | _ -> false

                                                                                                                     
  let is_array = function VAR (_, attrs) -> List.exists (fun x -> match x with ARRAY _ -> true | _ -> false) attrs | _ -> false
                            
  let is_param = function VAR (_, attrs) -> List.exists (fun x -> match x with PARAM -> true | _ -> false) attrs | _ -> false

  let is_bar = function VAR (_, attrs) -> List.exists (fun x -> match x with BAR -> true | _ -> false) attrs | _ -> false

  let is_hat = function VAR (_, attrs) -> List.length attrs > 0 && List.hd attrs = HAT | _ -> false

  let is_check = function VAR (_, attrs) -> List.exists (fun x -> match x with CHECK -> true | _ -> false) attrs | _ -> false

  let is_tilde  = function VAR (_, attrs) -> List.length attrs > 0 && List.hd attrs = TILDE | _ -> false

  let is_dot = function VAR (_, attrs) -> List.exists (fun x -> match x with DOT -> true | _ -> false) attrs | _ -> false

  let is_global = function VAR (_, attrs) -> List.exists (fun x -> match x with GLOBAL -> true | _ -> false) attrs | _ -> false

  let is_indirect = function VAR (_, attrs) -> List.exists (fun x -> match x with INDIRECT -> true | _ -> false) attrs | _ -> false

  let is_not_local v = is_global v || is_param v

  let is_local v = not (is_not_local v)
 
  let is_extern = function VAR (_, attrs) -> List.exists (fun x -> match x with EXTERN -> true | _ -> false) attrs  | _ -> false

  let is_acute  = function VAR (_, attrs) -> List.exists (fun x -> match x with ACUTE -> true | _ -> false) attrs | _ -> false

  let is_dotdot = function VAR (_, attrs) -> List.exists (fun x -> match x with DOTDOT -> true | _ -> false) attrs | _ -> false

  let is_funcptr = function VAR (_, attrs) -> List.exists (fun x -> match x with FUNCPTR _ -> true | _ -> false) attrs | _ -> false

  let is_func = function VAR (_, attrs) -> List.exists (fun x -> match x with FUNC _ -> true | _ -> false) attrs | _ -> false

  let is_vararg_func = function VAR (_, attrs) -> List.exists (fun x -> match x with FUNC (_, [ARRAY []]) -> true | _ -> false) attrs | _ -> false

  let is_question = function VAR (_, attrs) -> List.exists (fun x -> match x with QUESTION -> true | _ -> false) attrs | _ -> false

  let is_nested = function VAR (_, attrs) -> List.exists (fun x -> match x with NESTED -> true | _ -> false) attrs | _ -> false

  let is_static = function VAR (_, attrs) -> List.exists (fun x -> match x with STATIC -> true | _ -> false) attrs | _ -> false

  let is_void = function VAR (_, attrs) -> List.length attrs = 0 | _ -> false

  let is_const = function CONST _ -> true | _ -> false
                                                                                                                      
  let set_bar = function VAR (v, attr) -> VAR (v, BAR::attr) | _ -> raise (NotAVariable "set_bar")

  let set_tilde = function VAR (v, attr) -> VAR (v, TILDE::attr) | _ -> raise (NotAVariable "set_bar")

  let set_check = function VAR (v, attr) -> VAR (v, CHECK::attr) | _ -> raise (NotAVariable "set_bar")

  let set_dot = function VAR (v, attr) -> VAR (v, DOT::attr) | _ -> raise (NotAVariable "set_bar")

  let set_dotdot = function VAR (v, attr) -> VAR (v, DOTDOT::attr) | _ -> raise (NotAVariable "set_bar")

  let set_acute = function VAR (v, attr) -> VAR (v, ACUTE::attr) | _ -> raise (NotAVariable "set_bar")

  let set_hat = function VAR (v, attr) -> VAR (v, HAT::attr) | _ -> raise (NotAVariable "set_bar")

  let set_nested = function VAR (v, attr) -> VAR (v, NESTED::attr) | _ -> raise (NotAVariable "set_bar")

  let set_question = function VAR (v, attr) -> VAR (v, QUESTION::attr) | _ -> raise (NotAVariable "set_bar")

  let set_indirect = function VAR (v, attr) -> VAR (v, INDIRECT::attr) | _ -> raise (NotAVariable "set_bar")

  let set_static = function VAR (v, attr) -> VAR (v, STATIC::attr) | _ -> raise (NotAVariable "set_bar")

  let rec get_struct_name = function
      VAR (nm, attrs) ->
      begin
        try
          match List.find (fun x -> match x with STRUCT _ -> true | _ -> false) attrs with
            STRUCT struct_name -> struct_name
          | _ -> ""
        with
          _ -> raise (StError ("Base-1: " ^ nm))
      end
    | BINOP (s, Op.ADD, _) -> get_struct_name s
    | _ -> ""
         

  let get_funcptr_ret = function
      VAR (_, attrs) ->
       begin
        try
         match List.find (fun x -> match x with FUNCPTR _ -> true | _ -> false) attrs with
           FUNCPTR (ret, _) -> ret
         | _ -> raise (StError "Base-4")
       with
         _ -> raise (StError "Base-4")
      end
    | _ -> raise (StError "Base-4")

         
  let size_of = function
      VAR (_s, attrs) ->
      begin
        try
          match List.find (fun x -> match x with SIMPLE _ -> true | _ -> false) attrs with
            SIMPLE n -> n
          | _ -> raise (StError ("Base-2 " ^ _s))
        with
          _ -> raise (StError ("Base-2 " ^ _s))
      end
    | _ -> raise (StError "Base-2")
         
  let get_array_length = function
      VAR (_, attrs) ->
       begin
         try
           match List.find (fun x -> match x with ARRAY _ -> true | _ -> false) attrs with
             ARRAY ls -> ls
           | _ -> raise (StError "Base-1")
         with
           _ -> raise (StError "Wrong Array")
       end
    | _ -> raise (StError "Wrong Array")
  ;;
  
  let greater v1 v2 =
    if is_simple v1 then
      v2
    else
      v1  
         
	let var_decode = function
      VAR (v, _) -> v
    | _ -> raise (StError "Not a Variable")

  let var = function
      VAR (a, b) -> (a,b)
    | _ -> raise (StError "Not a Variable")

  let var_encode attr s = VAR (s, attr::[])

  let get_attributes = function
      VAR (_, attrs) -> attrs
    | _ -> []

  let set_attributes ee attrs =
    match ee with
      VAR (x, _) ->
      VAR (x, attrs)
    | _ ->
       ee
  ;;
                                  
  let update_interval intv (v:var_t) (l, u) =
    let nm = List.filter (fun (x, _) -> not (x = v)) intv in
    (v, (l,u))::nm
    
  let var_par_subs subs_pair to_be =
    try
      let (_, by) = List.find (fun (t,_) ->
                        let r = fst t = fst to_be in
                        (* pw (fst t); p "="; pn (fst to_be);
                        pb r; pn ""; *)
                        r) subs_pair in
      by
    with
      Not_found -> to_be
                 
  let string_to_var (s:string) : t = VAR (s, [])
                                   
  let var_compare (x, _) (y, _) = String.compare x y

  let var_fv x = [VAR x]

  let var_add t vv =
    match vv with
      VAR (s, attrs) ->
      if t |<- attrs then vv else VAR (s, t::attrs)
    | _ -> vv

  let vars_add tt vv =
    (fun acc t' -> var_add t' acc) |->> (vv, tt)
             
  let string_to_var_attr str =
    let n = String.length str in
    if n < 5 then
      (str, [])
    else
      let c1 = String.sub str (n-4) 4 in
      let init = String.sub str 0 (n-4) in
    if c1 = "@hat" then
      (init, [HAT])
    else if c1 = "@indirect" then
      (init, [INDIRECT])
    else if c1 = "@bar" then
      (init, [BAR])
    else if c1 = "@dot" then
      (init, [DOT])
    else if c1 = "@tilde" then
      (init, [TILDE])
    else if c1 = "@check" then
      (init, [CHECK])
    else if c1 = "@dotdot" then
      (init, [DOTDOT])
    else if c1 = "@acute" then
      (init, [ACUTE])
    else
      (str, []) 

      
	let var_eq (x, xa) (y, ya) =
    if x = y && ((CHECK |<- xa) = (CHECK |<- ya)) && ((BAR |<- xa) = (BAR |<- ya)) && ((HAT |<- xa) = (HAT |<- ya)) then
      true
    else
      false

  let rec enptr = function
    | VAR (s, a) -> VAR (s,PTR::a)
    | BINOP (a, o, b) -> BINOP (enptr a, o, b)
    | e -> e
      
	let rec toStr = function
		  NOTHING -> "<nothing>"
    | NEGINF -> "-inf"
    | POSINF -> "inf"
    | UNDEF -> "?"
    | VAR (s, a) -> get_str (s,a)
    | CONST (i) -> string_of_int i
    | FLOAT (f) -> begin try string_of_float f with _ -> "Error @ string_of_float" end
    | BINOP (e1, op, e2) -> "(" ^ (toStr e1) ^ (Op.pprint op) ^ (toStr e2) ^ ")"
    | INDICES (e2) -> String.concat "," (toStr |>>| e2)
    | ADDR (e) -> "&(" ^ (toStr e) ^ ")"
    | REF (e) -> "*" ^ (toStr e)
    | NEG (e) -> "!" ^ (toStr e)
    | STRING (s) -> "\"" ^ (String.escaped s) ^ "\""
    | ARROW (t, f) -> toStr t ^ "->" ^  f
    | LBL (lbl, t) -> "(" ^ toStr t ^ ")" ^ lbl
    | FCALL (fn, es) -> fn ^ "(" ^ (String.concat "," (toStr |>>| es)) ^ ")"
    | OFFSET (sn, exp) -> "OFFSET (" ^ sn ^ ", " ^ toStr exp ^ ")"
    | SIZEOF (tp) -> "SIZEOF (" ^ tp ^ ")"

    (* kimura 2021.07.20 small modified version of toStr2 (only difference is VAR) *)
    (* This is used in the translation from Exp into Smt in vcpPrecond2 *)
	and toStr2 = function 
		  NOTHING -> "<nothing>"
    | NEGINF -> "-inf"
    | POSINF -> "inf"
    | UNDEF -> "?"
    | VAR (s, _) -> s
    | CONST (i) -> string_of_int i
    | FLOAT (f) -> begin try string_of_float f with _ -> "Error @ string_of_float" end
    | BINOP (e1, op, e2) -> "(" ^ (toStr2 e1) ^ (Op.pprint op) ^ (toStr2 e2) ^ ")"
    | INDICES (e2) -> String.concat "," (toStr2 |>>| e2)
    | ADDR (e) -> "&(" ^ (toStr2 e) ^ ")"
    | REF (e) -> "*" ^ (toStr2 e)
    | NEG (e) -> "!" ^ (toStr2 e)
    | STRING (s) -> "\"" ^ (String.escaped s) ^ "\""
    | ARROW (t, f) -> toStr2 t ^ "->" ^  f
    | LBL (lbl, t) -> "(" ^ toStr2 t ^ ")" ^ lbl
    | FCALL (fn, es) -> fn ^ "(" ^ (String.concat "," (toStr2 |>>| es)) ^ ")"
    | OFFSET (sn, exp) -> "OFFSET (" ^ sn ^ ", " ^ toStr2 exp ^ ")"
    | SIZEOF (tp) -> "SIZEOF (" ^ tp ^ ")"
                   
  and get_str ?logical:(log=false) (v, attrs) =
    let str =
      if String.length v > 1 then
        let c = String.get v 0 in
        if c = '.' || c = '$' then
            String.sub v 1 ((String.length v)-1)
        else
            v
      else
          v
    in

    let sattr =
      (* if log then *)
        let rec sattr_str = function
          [] -> ""
        | x::xs ->
           match x with
             BAR ->
              print_attr BAR
           | INDIRECT ->
              (print_attr INDIRECT) ^ (print_attr TILDE)
           | HAT ->
              print_attr HAT
           | CHECK ->
              print_attr CHECK
           | DOT ->
              print_attr DOT
           | TILDE ->
              print_attr TILDE
           | DOTDOT ->
              print_attr DOTDOT
           | QUESTION ->
              print_attr QUESTION
           | _ -> sattr_str xs
        in
        sattr_str attrs
    in
    str ^ sattr

  and print_attr = function
	   PTR -> "<x*>"
   | PTRPTR -> "<**>"
	 | STRUCT (s) -> "<{" ^ s ^ "}>"
	 | EXQ -> "<x>"
   | ARRAY el -> "<" ^ ((fun acc s -> acc ^ "[" ^ toStr s ^ "]") |->> ("", el)) ^ ">"
   | PARAM -> "<p>"
   | GLOBAL -> "<g>"
   | HAT -> "@hat"
   | BAR -> "@bar"
   | EXTERN -> "<ext>"
   | FUNCPTR (rets, params) -> "(*" ^ String.concat "" (print_attr |>>| rets) ^  "|" ^ String.concat "" (print_attr |>>| params) ^ ")"
   | CHECK -> "@chk"
   | TILDE -> "@tilde"
   | DOT -> "@dot"
   | NESTED -> "[.]"
   | QUESTION -> "Qx"
   | DOTDOT -> "@dotdot"
   | ACUTE -> "@act"
   | INDIRECT -> "~>"
   | STATIC -> "<STC>"
   | SIMPLE n -> "<"^ (string_of_int n) ^ ">"
   | FUNC (rets, params) -> "(" ^ String.concat "" (print_attr |>>| rets) ^  "|" ^ String.concat "" (print_attr |>>| params) ^ ")"

  let pp_attr' ppf a = Format.pp_print_string ppf @@ print_attr a

  let var_to_str (v, attr) =
    if HAT |<- attr then
      get_str (v, [HAT])
    else if INDIRECT |<- attr then
      get_str (v, [INDIRECT])
    else if CHECK |<- attr then
      get_str (v, [CHECK])
    else if TILDE |<- attr then
      get_str (v, [TILDE])
    else if DOT |<- attr then
      get_str (v, [DOT])
    else if QUESTION |<- attr then
      get_str (v, [QUESTION])
    else if DOTDOT |<- attr then
      get_str (v, [DOTDOT])
    else if ACUTE |<- attr then
      get_str (v, [ACUTE])
    else if BAR |<- attr then
      get_str (v, [BAR])
    else
      get_str (v, [])    

  let rec pp' ppf = function
		  NOTHING -> Format.pp_print_string ppf "<nothing>"
    | NEGINF -> Format.pp_print_string ppf "-inf"
    | POSINF -> Format.pp_print_string ppf "inf"
    | UNDEF -> Format.pp_print_string ppf "?"
    | VAR (s, a) -> pp_var' ppf (s, a)
    | CONST (i) -> Format.fprintf ppf (if i < 0 then "(%d)" else "%d") i
    | FLOAT (f) ->
        let s = try string_of_float f with _ -> "Error @ string_of_float" in
        Format.pp_print_string ppf s
    | BINOP (e1, op, e2) -> Format.fprintf ppf "@[<2>(%a@,%a@,%a)@]" pp' e1 Op.pp' op pp' e2
    | INDICES (e2) -> pps' ppf e2
    | ADDR (e) -> Format.fprintf ppf "@[<2>&(%a)@]" pp' e
    | REF (e) -> Format.fprintf ppf "@[<2>*%a@]" pp' e
    | NEG (e) -> Format.fprintf ppf "@[<2>!%a@]" pp' e
    | STRING (s) -> Format.fprintf ppf "\"%s\"" (String.escaped s)
    | ARROW (t, f) -> Format.fprintf ppf "%a@,->@,%s" pp' t f
    | LBL (lbl, t) -> Format.fprintf ppf "%s@[<2>(%a)@]" lbl pp' t
    | FCALL (fn, es) -> Format.fprintf ppf "%s@[<2>(%a)@]" fn pps' es
    | OFFSET (sn, exp) -> Format.fprintf ppf "@[<2>OFFSET@;(%s,@,%a)@]" sn pp' exp
    | SIZEOF (tp) -> Format.fprintf ppf "@[<2>SIZEOF@;(%s)@]" tp
  and pps' ppf l =
      Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@,") pp' ppf l
  and pp_var' ppf (v, attrs) =
    let str =
      if String.length v > 1 then
        let c = String.get v 0 in
        if c = '.' || c = '$' then
            String.sub v 1 ((String.length v)-1)
        else
            v
      else
          v
    in
    let sattr =
      if !Options.show_types then
        List.fold_left (fun a s -> a ^ (print_attr s)) "" attrs
      else if BAR |<- attrs then
        print_attr BAR
      else if INDIRECT |<- attrs then
        (print_attr INDIRECT) ^ (print_attr TILDE)
      else if HAT |<- attrs then
        print_attr HAT
      else if CHECK |<- attrs then
        print_attr CHECK
      else if DOT |<- attrs then
        print_attr DOT
      else if TILDE |<- attrs then
        print_attr TILDE
      else if DOTDOT |<- attrs then
        print_attr DOTDOT
      else if QUESTION |<- attrs then
        print_attr QUESTION
      else ""
    in
    Format.pp_print_string ppf (str ^ sattr)

  (* let get_printable_string = Format.asprintf "%a" pp' *)
  (* let var_get_printable_string = Format.asprintf "%a" pp_var' *)
  let rec get_printable_string = function
		  NOTHING -> "<nothing>"
    | NEGINF -> "-inf"
    | POSINF -> "inf"
    | UNDEF -> "?"
    | VAR (s, a) -> var_get_printable_string (s, a)
    | CONST (i) -> if i < 0 then "(" ^ string_of_int i ^ ")" else string_of_int i
    | FLOAT (f) -> begin try string_of_float f with _ -> "Error @ string_of_float" end
    | BINOP (e1, op, e2) -> "(" ^ (get_printable_string e1) ^ (Op.pprint op) ^ (get_printable_string e2) ^ ")"
    | INDICES (e2) -> String.concat "," (get_printable_string |>>| e2)
    | ADDR (e) -> "&(" ^ (get_printable_string e) ^ ")"
    | REF (e) -> "*" ^ (get_printable_string e)
    | NEG (e) -> "!" ^ (get_printable_string e)
    | STRING (s) -> "\"" ^ (String.escaped s) ^ "\""
    | ARROW (t, f) -> get_printable_string t ^ "->" ^  f
    | LBL (lbl, t) -> lbl ^ "(" ^ get_printable_string t ^ ")"
    | FCALL (fn, es) -> fn ^ "(" ^ (String.concat "," (get_printable_string |>>| es)) ^ ")"
    | OFFSET (sn, exp) -> "OFFSET (" ^ sn ^ ", " ^ get_printable_string exp ^ ")"
    | SIZEOF (tp) -> "SIZEOF (" ^ tp ^ ")"

  and var_get_printable_string (v, attrs) =
    let str =
      if String.length v > 1 then
        let c = String.get v 0 in
        if c = '.' || c = '$' then
            String.sub v 1 ((String.length v)-1)
        else
            v
      else
          v
    in
    let sattr =
      if !Options.show_types then
        List.fold_left (fun a s -> a ^ (print_attr s)) "" attrs
      else if BAR |<- attrs then
        print_attr BAR
      else if INDIRECT |<- attrs then
        (print_attr INDIRECT) ^ (print_attr TILDE)
      else if HAT |<- attrs then
        print_attr HAT
      else if CHECK |<- attrs then
        print_attr CHECK
      else if DOT |<- attrs then
        print_attr DOT
      else if TILDE |<- attrs then
        print_attr TILDE
      else if DOTDOT |<- attrs then
        print_attr DOTDOT
      else if QUESTION |<- attrs then
        print_attr QUESTION
      else ""
    in
    (* pn "----------"; pn str; pn sattr; *)
    str ^ sattr


  (* let pprint = pp_printable Format.std_formatter *)
  let rec pprint x = p (get_printable_string x)

  (* let var_pprint = pp_var_printable Format.std_formatter *)
  let rec var_pprint x = p (var_get_printable_string x)

  let pp_var ppf (v, attrs) =
    let rec aux ppf = function
	      PTR -> Format.pp_print_string ppf "PTR"
      | PTRPTR -> Format.pp_print_string ppf "PTRPTR"
	    | STRUCT (s) -> Format.fprintf ppf "Var.STRUCT \"%s\"" s
	    | EXQ -> Format.pp_print_string ppf "EXQ"
      | ARRAY l -> Format.fprintf ppf "@[<2>ARRAY@;(%a)@]" (Ftools.pp_print_list pp') l
      | PARAM -> Format.pp_print_string ppf "PARAM"
      | GLOBAL -> Format.pp_print_string ppf "GLOBAL"
      | HAT -> Format.pp_print_string ppf "HAT"
      | BAR -> Format.pp_print_string ppf "BAR"
      | EXTERN -> Format.pp_print_string  ppf "EXTERN"
      | FUNCPTR (rets, params) -> Format.fprintf ppf "@[<2>FUNCPTR@;(%a,@,%a)@]" (Ftools.pp_print_list aux) rets (Ftools.pp_print_list aux) params
      | CHECK -> Format.pp_print_string ppf "CHECK"
      | TILDE -> Format.pp_print_string ppf "TILDE"
      | DOT -> Format.pp_print_string ppf "DOT"
      | NESTED -> Format.pp_print_string ppf "NESTED"
      | QUESTION -> Format.pp_print_string ppf "QUESTION"
      | DOTDOT -> Format.pp_print_string ppf "DOTDOT"
      | ACUTE -> Format.pp_print_string ppf "ACUTE"
      | INDIRECT -> Format.pp_print_string ppf "INDIRECT"
      | STATIC -> Format.pp_print_string ppf "STATIC"
      | SIMPLE n -> Format.fprintf ppf "@[<2>SIMPLE@;(%d)@]" n
      | FUNC (rets, params) -> Format.fprintf ppf "@[<2>FUNC@;(%a,@,%a)@]" (Ftools.pp_print_list aux) rets (Ftools.pp_print_list aux) params
    in
    Format.fprintf ppf "(\"%s\",%a)" v (Ftools.pp_print_list aux) attrs

  let rec pp ppf = function
      NOTHING -> Format.fprintf ppf "NOTHING"
    | NEGINF -> Format.fprintf ppf "NEGINF"
    | POSINF -> Format.fprintf ppf "POSINF"
    | UNDEF  -> Format.fprintf ppf "UNDEF"
    | VAR (s, a) -> Format.fprintf ppf "@[<2>VAR@;%a@]" pp_var (s,a)
    | CONST (i) -> Format.fprintf ppf "@[<2>CONST@;(%d)@]" i
    | FLOAT (f) ->
        let s = try string_of_float f with _ -> "Error @ string_of_float" in
        Format.fprintf ppf "@[<2>FLOAT@;%s@]" s
    | STRING (s) -> Format.fprintf ppf "@[<2>STRING@;\"%s\"@]" (String.escaped s)
    | ADDR (e) -> Format.fprintf ppf "@[<2>ADDR@;(%a)@]" pp e
    | NEG (e) -> Format.fprintf ppf "@[<2>NEG@;(%a)@]" pp e
    | REF (e) -> Format.fprintf ppf "@[<2>REF@;(%a)@]" pp e
    | ARROW (t, f) -> Format.fprintf ppf "@[<2>ARROW@;(%a,@,\"%s\")@]" pp t (String.escaped f)
    | BINOP (e1, op, e2) -> Format.fprintf ppf "@[<2>BINOP@;(%a,@,%a,@,%a)@]" pp e1 Op.pp' op pp e2
    | LBL (lbl, e) -> Format.fprintf ppf "@[<2>Exp.LBL@;(%s,@,(%a))@]" lbl pp e
    | FCALL (fn, es) -> Format.fprintf ppf "@[<2>FCALL@;(%s,@,%a)@]" fn (Ftools.pp_print_list pp) es
    | INDICES (e2) -> Format.fprintf ppf "@[<2>INDICES@;(%a)@]" (Ftools.pp_print_list pp) e2
    | OFFSET (sn, exp) -> Format.fprintf ppf "@[OFFSET@;(%s,@,%a)@]" sn pp exp
    | SIZEOF (tp) -> Format.fprintf ppf "@[<2>SIZEOF@;(%s)@]" tp

  (* let print = pp Format.std_formatter *)
  (* let var_print = pp_var Format.std_formatter *)
  let rec print = function
      NOTHING -> p "NOTHING"
    | NEGINF -> p "NEGINF"
    | POSINF -> p "POSINF"
    | UNDEF  -> p "UNDEF"
    | VAR (s, a) -> p "VAR "; var_print (s,a)
    | CONST (i) -> p "CONST ("; pl i; p ")"
    | FLOAT (f) -> p "FLOAT "; p (begin try string_of_float f with _ -> "Error @ string_of_float" end)
    | STRING (s) -> p "STRING "; p ("\"" ^ (String.escaped s) ^ "\"")
    | ADDR (e) -> p "ADDR ("; print e; p ")"
    | NEG (e) -> p "NEG ("; print e; p ")"
    | REF (e) -> p "REF ("; print e; p ")"
    | ARROW (t, f) -> p "ARROW ("; print t; p ","; p ("\"" ^ (String.escaped f) ^ "\""); p ")"
    | BINOP (e1, op, e2) -> p "BINOP "; p"("; print e1; p ("," ^ (Op.print op) ^ ","); print e2; p ")"
    | LBL (lbl, e) -> p "Exp.LBL ("; p lbl; p ",("; print e; p "))"
    | FCALL (fn, es) -> p "FCALL ("; p fn; p ",["; iterS print ";" es; p "])"
    | INDICES (e2) -> p "INDICES ("; print_list print e2; p ")"
    | OFFSET (sn, exp) -> p ("OFFSET (" ^ sn ^ ", "); print exp; p ")"
    | SIZEOF (tp) -> p ("SIZEOF (" ^ tp ^ ")")

  and var_print (v, attrs) =
    let rec aux = function
	      PTR -> "PTR"
      | PTRPTR -> "PTRPTR"
	    | STRUCT (s) -> "Var.STRUCT \"" ^ s ^ "\""
	    | EXQ -> "EXQ"
      | ARRAY l -> "ARRAY (" ^ string_of_list get_printable_string l ^ ")"
      | PARAM -> "PARAM"
      | GLOBAL -> "GLOBAL"
      | HAT -> "HAT"
      | BAR -> "BAR"
      | EXTERN -> "EXTERN"
      | FUNCPTR (rets, params) -> "FUNCPTR ([" ^ String.concat "" (aux |>>| rets) ^ "],[" ^ String.concat "" (aux |>>| params) ^ "])"
      | CHECK -> "CHECK"
      | TILDE -> "TILDE"
      | DOT -> "DOT"
      | NESTED -> "NESTED"
      | QUESTION -> "QUESTION"
      | DOTDOT -> "DOTDOT"
      | ACUTE -> "ACUTE"
      | INDIRECT -> "INDIRECT"
      | STATIC -> "STATIC"
      | SIMPLE n -> "SIMPLE (" ^ (string_of_int n) ^ ")"
      | FUNC (rets, params) -> "FUNC ([" ^ String.concat "" (aux |>>| rets) ^ "],[" ^ String.concat "" (aux |>>| params) ^ "])"
    in
    let str_attrs = string_of_list aux attrs in
    p ("(\"" ^ v ^ "\"," ^ str_attrs ^ ")")


  let rec fstr () = function
		  NOTHING -> Format.sprintf "<nothing>"
    | NEGINF -> Format.sprintf "-inf"
    | POSINF -> Format.sprintf "inf"
    | UNDEF -> Format.sprintf "?"
    | VAR (s, a) -> fstr_var () (s, a)
    | CONST (i) -> Format.sprintf (if i < 0 then "(%d)" else "%d") i
    | FLOAT (f) ->
        let s = try string_of_float f with _ -> "Error @ string_of_float" in
        Format.sprintf "@[<2>FLOAT@;%s@]" s
    | BINOP (e1, op, e2) -> Format.sprintf "@[<2>(%a@,%a@,%a)@]" fstr e1 Op.fstr op fstr e2
    | INDICES (e2) -> fstrs () e2
    | ADDR (e) -> Format.sprintf "@[<2>&(%a)@]" fstr e
    | REF (e) -> Format.sprintf "@[<2>*%a@]" fstr e
    | NEG (e) -> Format.sprintf "@[<2>!%a@]" fstr e
    | STRING (s) -> Format.sprintf "\"%s\"" (String.escaped s)
    | ARROW (t, f) -> Format.sprintf "%a@,->@,%s" fstr t f
    | LBL (lbl, t) -> Format.sprintf "%s@[<2>(%a)@]" lbl fstr t
    | FCALL (fn, es) -> Format.sprintf "%s@[<2>(%a)@]" fn fstrs es
    | OFFSET (sn, exp) -> Format.sprintf "@[<2>OFFSET@;(%s,@,%a)@]" sn fstr exp
    | SIZEOF (tp) -> Format.sprintf "@[<2>SIZEOF@;(%s)@]" tp
  and fstrs () xs = Format.sprintf "%a" (fstrL fstr ",") xs
  and fstr_attr () = function
      PTR -> Format.sprintf "PTR"
    | PTRPTR -> Format.sprintf "PTRPTR"
	  | STRUCT (s) -> Format.sprintf "Var.STRUCT \"%s\"" s
	  | EXQ -> Format.sprintf "EXQ"
    | ARRAY l -> Format.sprintf "@[<2>ARRAY@;(%a)@]" (fstr_vars) l
    | PARAM -> Format.sprintf "PARAM"
    | GLOBAL -> Format.sprintf "GLOBAL"
    | HAT -> Format.sprintf "HAT"
    | BAR -> Format.sprintf "BAR"
    | EXTERN -> Format.sprintf "EXTERN"
    | FUNCPTR (rets, params) -> Format.sprintf "@[<2>FUNCPTR@;(%a,@,%a)@]" fstr_attrs rets fstr_attrs params
    | CHECK -> Format.sprintf "CHECK"
    | TILDE -> Format.sprintf "TILDE"
    | DOT -> Format.sprintf "DOT"
    | NESTED -> Format.sprintf "NESTED"
    | QUESTION -> Format.sprintf "QUESTION"
    | DOTDOT -> Format.sprintf "DOTDOT"
    | ACUTE -> Format.sprintf "ACUTE"
    | INDIRECT -> Format.sprintf "INDIRECT"
    | STATIC -> Format.sprintf "STATIC"
    | SIMPLE n -> Format.sprintf "@[<2>SIMPLE@;(%d)@]" n
    | FUNC (rets, params) -> Format.sprintf "@[<2>FUNC@;(%a,@,%a)@]" fstr_attrs rets fstr_attrs params
  and fstr_var () (v, attrs) =
    Format.sprintf "(\"%s\",%a)" v fstr_attrs attrs                         
  and fstr_vars () xs = Format.sprintf "%a" (fstrL fstr ",") xs
  and fstr_attrs () xs = Format.sprintf "%a" (fstrL fstr_attr ",") xs

  let fstr () e = Format.sprintf "%s" (get_printable_string e)
                
  let get_func_ret = function
      VAR (_, attrs) as v ->
       begin
        try
         match List.find (fun x -> match x with FUNC _ -> true | _ -> false) attrs with
           FUNC (ret, _) -> ret
         | _ ->  
            raise (StError "Base-3-1")
        with
          _ ->
          print v; pn "";
          raise (StError "Base-3-2")
      end
    | _ -> raise (StError "Base-3-3")


  let rec get_original_type aliases s =
    if V.mem s aliases then
	    let t = V.find s aliases in
	    if t = s then
	      s
	    else
      	get_original_type aliases t 
    else
      s
           
  let simple_size = function
      "char" -> 1
    | "bool" -> 1
    | "short" -> 2
    | "int" -> 4
    | "long" -> 8
    | "int64" -> 8
    | "float" -> 4
    | "double" -> 8
    | "sizet" -> 2
    | x -> raise (StError ("Unknown simple type " ^ x))

  let is_simple_type = function
      "char"
    | "bool"
    | "short"
    | "int"
    | "long"
    | "int64"
    | "float"
    | "double"
    | "sizet" -> true
    | _ -> false


let rec var_be_typed vars to_static loc (x, _) =
  if x = "main" then
    VAR (to_static x, [])
  else if V.mem x vars then
    let attr = V.find x vars in
    let temp = VAR (x, attr) in
    if is_static temp then
      VAR (to_static x, attr)
    else
      temp
  else
    if String.contains x '#' then
      VAR (x, [EXQ])
    else if String.contains x '%' then
      let i = String.rindex x '%' in
      let l = String.length x in
      let part = try
          String.sub x (i+1) (l-i-1)
        with
          Invalid_argument _ as e ->
          pn x;
          raise e
      in
      var_be_typed vars to_static loc (part, [])
    else if x |<- Shared.builtins then
      VAR (x, [])
    else
      (
        VAR (x, [])
      (* raise (VarNotFound ("Variable not found: " ^ x)) *)
      )
  ;;
         
  let rec _struct_size_by_name structures st_name =
    let (_, fields, _) = if V.mem st_name structures then
                           V.find st_name structures
                         else
                           raise (StError ("E1 " ^ st_name))
    in

    match fields with
      [] -> 0
    | (fld,_)::fields' ->
       let fld_sz = _size structures fld in
       (fun acc (fld, _) ->
         let fld_sz = _size structures fld in
         fld_sz + acc
       ) |->> (fld_sz, fields')

  and _struct_size structures v =
    let  st_name = get_struct_name v in
    _struct_size_by_name structures st_name
    
  and _array_size structures v =
           
    let lens : t list = get_array_length v in
    
    match lens with
      CONST n::lens' -> (** It should be extended to Exp.t to cabs.t *)
       let total_len = (fun acc ln ->
           match ln with
             CONST n ->
              acc * n
           | _ -> raise (StError "array length should be constant (3) in size of")
         ) |->> (n, lens')
       in
       let attr = get_attributes v in
       let attr' = (function ARRAY _ -> false | _ -> true) |>- attr in
       let v' = set_attributes v attr' in
       total_len * _size structures v'
    | freshv::_ when is_question freshv ->
       0
    | es ->
       iterS print "-" es; pn " ."; 
       raise (StError "array length should be constant (4) in size of")
    
  and _size structures v =
    if is_ptr v then
      simple_size "long"
    else if is_ptrptr v then
      simple_size "long"
    else
    match is_struct v && not (is_ptr v), is_array v with
      true, true ->
       let st_size = _struct_size structures v in
       let ar_len = try
           _array_size structures v
         with
           e ->
           pprint v;
           raise e
       in
       st_size * ar_len
    | true, false ->
       let st_size = _struct_size structures v in
       st_size
    | false, true ->
       let ar_len = try
           _array_size structures v
         with
           _ ->
           print v;pn "..";
           raise Error
       in
       if is_simple v then
         ar_len * size_of v
       else
         raise (StError ("Size of " ^ toStr v ^ " is not known (1)"))
    | false, false ->
       if is_simple v then
         size_of v
       else
         4 (** Todo:: Need to make sure if it is union *)
         (* raise (StError ("Size of " ^ toStr v ^ " is not known (2)")) *)
         

  let rec __size_by_name (aliases : string V.t) structures s =
    if V.mem s aliases then
      let alias_name : string = V.find s aliases in
      let sz =
        if alias_name = s then
          CONST 4
        else
          __size_by_name aliases structures alias_name in
      sz
    else
      if is_simple_type s then
        CONST (simple_size s)
      else
          if V.mem s structures then
            CONST (_struct_size_by_name structures s)
          else(
          dbg "WARNING" "Undefined Type:" p s;
        (** crypto_ec_ecp_nistz256_c : sizeof bignum_st  *)
          CONST 4
          (* V.iter (fun k v -> pn k) structures; pn "\n@@@@@@"; *)
        (* raise (StError ("Unsupported type for sizeof " ^ s)) *)
        
        );;

  let size_to_type n =
      match n with
        1 -> "char"
      | 2 -> "short"
      | 4 -> "int"
      | 8 -> "long"
      | _ -> raise (StError "Size of Unknown Type")
  ;;
  
  let rec type_of_var ptr_c at =
    try        
      begin
        let dummy = VAR ("",at) in
        if is_funcptr dummy then
          "long"
        else
        match (function STRUCT _ | SIMPLE _ -> true | _ -> false) |>- at with
          STRUCT s::_ -> s
        | SIMPLE n::_ -> size_to_type n
        | _ ->
           if ptr_c =  0 && (PTR |<- at || PTRPTR |<- at) then
             "long"
           else if ptr_c = 1 && PTRPTR |<- at then
             "long"
           else
             "void" (** This must be addressed later. Example: git/http.c*)
               (* raise (StError "No type error") (* tp_c#_13171 *) *)
      end
    with
      e ->
      raise e
  ;;
  
(*
  let rec size_of_expr fvs structures aliases exp =
       begin
         let _attr vname =
           if V.mem vname fvs then
             let attr = V.find vname fvs in
             attr
           else
             raise (StError "Not in FVS")
         in
         
         let _exp v =
           VAR (v, _attr v)
         in
         (*  C.BINARY (C.MUL, sz, C.CONSTANT (C.CONST_INT (string_of_int smp_sz)))   *)

         let rec type_of_struct_field fld = function
             Cabs.VARIABLE sv ->
             let v' = _exp sv in
             let st_name = get_struct_name v' in
             let (_, fields) =
                if V.mem st_name structures then
                  V.find st_name structures
                else
                  (
                    Cprint.print_expression exp; pn "";
                    pi (V.cardinal structures);
                    V.iter (fun k v -> pn k) structures;
                    raise (StError ("size of problem: " ^ st_name))
                  )
              in
              
              
              let field = try
                  List.find (fun f -> toStr f = fld) fields
                with
                  _ -> raise (StError "field is not found")
              in
              begin
                try
                  type_of_var 0 (get_attributes field), v'
                with
                  e ->
                  print field;
                  Cprint.print_expression exp; pn " (2)";
                  raise e
              end
           | Cabs.MEMBEROF (exp, fld1)
             | Cabs.MEMBEROFPTR (exp, fld1) ->

              let fld_tp, v'' = type_of_struct_field fld1 exp in
              let (_, fields) =
                if V.mem fld_tp structures then
                  V.find fld_tp structures
                else
                  (
                    Cprint.print_expression exp; pn " (3)";
                    pi (V.cardinal structures);
                    V.iter (fun k v -> pn k) structures;
                    raise (StError ("size of problem(2): " ^ fld_tp))
                  )
              in
              let field2 = try
                  List.find (fun f -> toStr f = fld) fields
                with
                  _ -> raise (StError "field is not found")
              in
              (* CONST (_size structures (fst field)) *)
              begin
                try
                  type_of_var 0 (get_attributes field2), v''
                with
                  e ->
                  Cprint.print_expression exp; pn " (4)";
                  raise e
              end
           | Cabs.PAREN (exp) ->
              type_of_struct_field fld exp
           | Cabs.UNARY (Cabs.MEMOF, sv) ->
              type_of_struct_field fld sv
           | Cabs.INDEX (exp1, exp2) ->
              type_of_struct_field fld exp1
           | e ->
              Cprint.print_expression e; pn "";
              raise (StError "Unfamiliar Expression as struct")
         in

         
         
         let rec size_of_exp ptr_c exp =
           match exp with
             Cabs.VARIABLE (sv) ->
              begin
                (* CONST (_size structures @@ _exp sv) *)
                let at = _attr sv in
                try
                  SIZEOF (type_of_var ptr_c at), _exp sv
                with
                  e ->
                  pn "STRUCTURES:";
                  V.iter (fun k v -> pw k; p ", ") structures;
                  pn "\nFVS:";
                  (*V.iter (fun k v -> Format.printf "%a|  " pp (VAR (k, v))) fvs; pn " FVS END";*)
                  V.iter (fun k v -> print (VAR (k, v)); pw "| ") fvs; pn " FVS END";
                  (*Format.printf "%d\n" (List.length at);*)
                  pi (List.length at); pn "";
                  Cprint.print_expression exp; pn "";
                  raise e
              end
           | Cabs.MEMBEROF (exp, fld)
             | Cabs.MEMBEROFPTR (exp, fld) ->
              let (a,v'') = type_of_struct_field fld exp in
              SIZEOF a, v''
           | Cabs.UNARY (Cabs.MEMOF, e') ->
              size_of_exp (ptr_c+1) e'
           | Cabs.UNARY (Cabs.ADDROF, exp) ->
              SIZEOF "long", VAR ("",[])
           | Cabs.PAREN (e') ->
              size_of_exp ptr_c e'
           | Cabs.INDEX (e1, e2) ->
              size_of_exp ptr_c e1
           | Cabs.CAST ((specs, dt), Cabs.SINGLE_INIT _) when VCabs.is_cabs_ptr dt ->
              SIZEOF "long", VAR ("",[])
           | Cabs.CAST ((specs, _), Cabs.SINGLE_INIT _) ->
              cabs_size_of_to_sizeof  fvs  structures aliases specs, VAR ("",[])
           (* | Cabs.CAST (_, Cabs.SINGLE_INIT e) ->
              size_of_exp ptr_c e *)
           | Cabs.CONSTANT (Cabs.CONST_STRING s) ->
              CONST (String.length s), CONST 1
           | Cabs.UNARY (op, exp) ->
              size_of_exp ptr_c exp
           | Cabs.BINARY (_, exp1, exp2) ->
              begin
                try
                  size_of_exp ptr_c exp1
                with
                  _ ->
                  size_of_exp ptr_c exp2
              end
           | Cabs.TYPE_SIZEOF (sp, dt) ->
              begin
                let res =
                  try
                    if VCabs.is_cabs_ptr dt then
                      SIZEOF "long"
                    else
                      cabs_size_of_to_sizeof fvs structures aliases sp
                  with
                    e ->
                    pi (List.length sp);
                    Cprint.print_expression exp; pn "";
                    begin
                      (match dt with
                         Cabs.JUSTBASE -> pn "@JUSTBASE";
                       | Cabs.PARENTYPE _ -> pn "@PARENTYPE";
                       | Cabs.PTR _ -> pn "@PTR";
                       | Cabs.PROTO _ -> pn "@PROTO"
                       | Cabs.ARRAY _ -> pn "@ARRAY");
                      raise e
                    end
                in
                res, VAR ("",[])
              end
           | Cabs.CONSTANT _ -> SIZEOF "int", VAR ("",[])
           | Cabs.QUESTION (_, exp1, exp2) ->
              begin
                try
                  size_of_exp ptr_c exp1
                with
                  _ ->
                  size_of_exp ptr_c exp2
              end
           | ee -> (*  CONST 1 *)
              Cprint.print_expression exp; pn " ERR";
              raise (StError "Unsupported Expression for Sizeof_e")
         in

         let sz, v'' = size_of_exp 0 exp in
         if is_array v'' then
           let ln = get_array_length v'' in
           let ln' = (fun acc l -> BINOP (acc, Op.MUL, l)) |->> (List.hd ln, List.tl ln) in
           BINOP (sz, Op.MUL, ln')
         else
           sz
        
       end
     
    and cabs_size_of_to_sizeof  fvs structures aliases sps =
      let rec aux = function
        | Cabs.SpecType x::xs ->
         begin
           match x with
             Cabs.Tvoid -> SIZEOF "void"
           | Cabs.Tchar -> SIZEOF "char"
           | Cabs.Tbool -> SIZEOF "bool"
           | Cabs.Tshort -> SIZEOF "short"
           | Cabs.Tint -> SIZEOF "int"
           | Cabs.Tlong -> SIZEOF "long"
           | Cabs.Tint64 -> SIZEOF "int64"
           | Cabs.Tfloat -> SIZEOF "float"
           | Cabs.Tdouble -> SIZEOF "double"
           | Cabs.Tsizet -> SIZEOF "int"
           | Cabs.Tsigned
           | Cabs.Tunsigned -> begin
               try
                 aux xs
               with
                 _ ->
                 SIZEOF "int"
             end
           | Cabs.Tstruct (s, _, _)
             | Cabs.Tunion (s, _, _)
             | Cabs.Tenum (s, _, _) ->
              SIZEOF s
             | Cabs.Tnamed s ->
              let s' = get_original_type aliases s in
              SIZEOF s'
           | Cabs.TtypeofE (e) ->
              
              size_of_expr fvs structures aliases e
           | Cabs.TtypeofT (specs, dt) when VCabs.is_cabs_ptr dt ->
              SIZEOF "long"
           | Cabs.TtypeofT (specs, dt) ->
              aux specs
           
         end
      | Cabs.SpecCV Cabs.CV_CONST::xs ->
         aux xs
      | [] ->
         raise (StError "Empty Specs")
      | _ ->
         
         raise (StError "Unsupported type in SIZE_OF")
    in
    aux sps
           
    
  let rec cabs_size_of (aliases : string V.t) structures = function
      Cabs.SpecType x::xs ->
      begin
        match x with
          Cabs.Tvoid -> CONST 8
        | Cabs.Tint -> CONST (simple_size "int")
        | Cabs.Tchar -> CONST (simple_size "char")
        | Cabs.Tlong -> CONST (simple_size "long")
        | Cabs.Tstruct (s, _, _)
          | Cabs.Tnamed s ->
           
           __size_by_name aliases structures s
        | _ ->
           cabs_size_of aliases structures xs
      end
    | _ ->
       CONST 1
 *)
  
  let rec map (f : var_t -> t) = function
    | VAR (s) -> f s
    | ADDR (e) -> map f e
    | BINOP (e1, op, e2) -> BINOP (map f e1, op, map f e2)
    | FCALL (s, ts) -> FCALL (s, map f |>>| ts)
    | x -> x

  let rec eq a b =
    match a, b with
      VAR (x), VAR (y) -> var_eq x y
    | ADDR (x), ADDR (y)
    | REF x, REF y -> eq x y
    | NOTHING, NOTHING -> true
    | CONST (x), CONST (y) -> x = y
    | FLOAT (x), FLOAT (y) -> x = y
    | INDICES (x), INDICES (y) -> List.for_all (fun (a,b) -> eq a b) (List.combine x y)
    | BINOP (x1, op1, y1), BINOP (x2, op2, y2) -> eq x1 x2 && op1 = op2 && eq y1 y2
    | _ -> false

  let rec be_typed vars statics loc = function
      VAR x ->
       let v = (var_be_typed vars statics loc x) in
       v
    | BINOP (e1, op, e2) ->
       let e1' = be_typed vars statics loc e1 in
       let e = BINOP (e1', op, be_typed vars statics loc e2) in
       e
    | INDICES (l_e) -> INDICES ((be_typed vars statics loc) |>>| l_e)
    | REF e -> REF (be_typed vars statics loc e)
    | ADDR e -> ADDR (be_typed vars statics loc e)
    | ARROW (e, f) -> ARROW (be_typed vars statics loc e, f)
    | x -> x

  let rec is_pure = function
      NOTHING -> true
    | NEGINF -> true
    | POSINF -> true
    | UNDEF -> true
    | VAR (_, attrs) -> attrs = []
    | CONST (i) -> true
    | FLOAT (f) -> true
    | STRING (s) -> true
    | BINOP (e1, _, e2) -> (is_pure e1) && (is_pure e2)
    | INDICES (e2) -> true
    | ADDR e
    | REF e -> is_pure e
    | NEG e -> is_pure e
    | ARROW (t,f) -> true (** NEED TO CHECK *)
    | LBL (lbl, e) -> true
    | FCALL (_, es) -> List.for_all is_pure es
    | OFFSET _ -> true
    | SIZEOF (tp) -> true

  let rec approx = function
      NOTHING -> UNDEF
    | NEGINF -> UNDEF
    | POSINF -> UNDEF
    | UNDEF -> UNDEF
    | BINOP (e1, o, e2) when o = Op.ADD || o = Op.SUB -> let e1' = approx e1 in let e2' = approx e2 in if e1' = UNDEF || e2' = UNDEF then UNDEF else BINOP (e1', o, e2')
    | BINOP (e1, o, CONST c) when o = Op.MUL || o = Op.DIV || o = Op.MOD -> let e1' = approx e1 in if e1' = UNDEF then UNDEF else BINOP (e1', o, CONST c)
    | BINOP (CONST c, o, e1) when o = Op.MUL -> let e1' = approx e1 in if e1' = UNDEF then UNDEF else BINOP (CONST c, o, e1')
    | BINOP (_, _, _) -> UNDEF
    | INDICES (_)
    | ADDR _
    | REF _
    | NEG _ -> UNDEF
    | x -> x

  let qvar () =
    let nv = (new_prog_var (), [QUESTION]) in
    VAR nv

  let rec string_to_exp_attr = function
    | VAR (v, _) ->
       let n = String.length v in
       let c = String.get v 0 in
       if c = '&' then
         ADDR (VAR (string_to_var_attr (String.sub v 1 (n-1))))
       else
         VAR (string_to_var_attr v)
    | BINOP (e1, o, e2) -> BINOP ((string_to_exp_attr e1), o, (string_to_exp_attr e2))
    | x -> x

  let rec exp_to_string_attr = function
    | VAR v -> VAR (var_to_str v, [])
    | ADDR (VAR v) ->
       let str = var_to_str v in
       VAR ("&" ^ str, [])
    | BINOP (e1, o, e2) -> BINOP ((string_to_exp_attr e1), o, (string_to_exp_attr e2))
    | x -> x


  let rec infer_attributes vars statics loc = function
      NOTHING -> []
    | VAR x ->
       begin
         match var_be_typed vars statics loc x with
           VAR (_, attrs) ->
           attrs
         | _ -> []
       end
    | BINOP (e1, _, _) -> infer_attributes vars statics loc e1
    | _ -> []

  let bnot x = (0-(x+1))

  let bit_not = function
      CONST x -> CONST (bnot x)
    | exp -> BINOP (CONST 0, Op.SUB, BINOP (exp, Op.ADD, CONST 1))

  let size_loc = ("#size", [HAT])

  let power_2 x = 1 lsl x
  (*
  let rec power_2 x =
    match x with
      0 -> 1
    | 1 -> 2
    | 2 -> 4
    | 3 -> 8
    | 4 -> 16
    | 5 -> 32
    | 6 -> 64
    | 7 -> 128
    | 8 -> 256
    | 9 -> 512
    | 10 -> 1024
    | 11 -> 2048
    | 12 -> 4096
    | 13 -> 8192
    | 14 -> 16384
    | 15 -> 32768
    | 16 -> 65536
    | 17 -> 131072
    | 18 -> 262144
    | 19 -> 524288
    | 20 -> 1048576
    | 21 -> 2097152
    | 22 -> 4194304
    | 23 -> 8388608
    | 24 -> 16777216
    | 25 -> 33554432
    | 26 -> 67108864
    | 27 -> 134217728
    | 28 -> 268435456
    | 29 -> 536870912
    | 30 -> 1073741824
    | 31 -> 2147483648
    | 32 -> 4294967296
    | _ -> 2 * power_2 (x-1)
    *)

  let bit_and exp x =
    let rec to_bin x i p acc =
      if i >= 32 then
        acc
      else
        let p' = p * 2 in
        let r = (p',((mod) x 2)) in
        to_bin (x/2) (i+1) p' (r::acc)
    in
    let bits = to_bin x 1 1 [] in
    begin
      match exp with
        CONST t ->
        let r = (fun acc (power, bit) ->
            if bit = 1 then
              let power_1 = power/2 in
              acc + (((t mod power)/power_1)*power_1)
            else
              acc
          ) |->> (0, bits) in
        CONST r
      | _ ->
         let r = (fun acc (power, bit) ->
             if bit = 1 then
               let power_1 = CONST (power/2) in
               BINOP(acc, Op.ADD, BINOP(BINOP (BINOP(exp, Op.MOD, CONST power), Op.DIV, power_1), Op.MUL, power_1))
             else
               acc
           ) |->> (CONST 0, bits) in
         r
    end

  let bit_or exp x =
    let not_exp = bit_not exp in
    let not_x = bnot x in
    let not_exp_and_not_x = bit_and not_exp not_x in
    bit_not not_exp_and_not_x

  let bitwise = function
      BINOP (exp, Op.BAND, CONST x) ->
      bit_and exp x
    | BINOP (exp, Op.OR, CONST x) ->
       bit_or exp x
    | BINOP (exp, Op.XOR, CONST x) ->
       let exp_or_x = bit_or exp x in
       let exp_and_x = bit_and exp x in
       let not_exp_and_x = bit_not exp_and_x in
       BINOP (exp_or_x, Op.BAND, not_exp_and_x)
    | BINOP (exp, Op.SHR, CONST x) ->
      let py = power_2 x in
      BINOP (exp, Op.DIV, CONST py)
    | BINOP (exp, Op.SHL, CONST x) ->
       let py = power_2 x in
      BINOP (exp, Op.MUL, CONST py)
    | exp -> exp


  let eval ?structs:(structures=V.empty) exp =
    (* let adjustPolarity = function
      (* | BINOP (x, Op.ADD, NEG y) -> BINOP (x, Op.SUB, y)
      | BINOP (x, Op.SUB, NEG y) -> BINOP (x, Op.ADD, y)
      | BINOP (x, op, NEG y) -> NEG (BINOP (x, op, y)) *)
      | e -> e
    in *)
    let rec eval1 = function
      BINOP (UNDEF, _, _) -> UNDEF
    | BINOP (_, _, UNDEF) -> UNDEF
    | BINOP (_, _, NOTHING) -> NOTHING
    | BINOP (NOTHING, _, _) -> NOTHING
    | BINOP (e1, op, BINOP(e2, Op.MUL, CONST (-1)))
    | BINOP (e1, op, BINOP(CONST (-1), Op.MUL, e2)) when op = Op.ADD || op = Op.SUB->
       begin
         let op' =
         match op with
           Op.ADD -> Op.SUB 
         | Op.SUB -> Op.ADD
         | _ -> raise Error
         in
         eval1 (BINOP (e1, op', e2))
       end
    | BINOP (BINOP (VAR x, Op.ADD, y), o, z) when o = Op.ADD || o = Op.SUB -> BINOP (VAR x, Op.ADD, eval1 (BINOP(y, o, z))) (** (A+B)+C  = A+(B+C) , (A+B)-C  = A+(B-C) *)
    | BINOP (BINOP (VAR x, Op.SUB, y), Op.SUB, z) -> BINOP (VAR x, Op.SUB, eval1 (BINOP(y, Op.ADD, z))) (** (A-B)-C  = A-B-C = A-(B+C) *)
    | BINOP (BINOP (VAR x, Op.SUB, y), Op.ADD, z) -> BINOP (VAR x, Op.SUB, eval1 (BINOP(y, Op.SUB, z))) (** (A-B)+C  = A-B+C = A-(B-C) *)
    | FCALL (s, ts) -> FCALL (s, eval1 |>>| ts)
    | BINOP (x, o, y) ->
       begin
         
         let x' = eval1 x in
         let y' = eval1 y in
         let res =
         match x', y' with
           NEGINF, _ -> NEGINF
         | _, NEGINF -> NEGINF
         | POSINF, _ -> POSINF
         | _, POSINF -> POSINF
         | CONST a, CONST b ->
            let r =
              begin
                match o with
                  Op.ADD -> a + b
                | Op.SUB -> a - b
                | Op.MUL -> a * b
                | Op.DIV -> a / b
                | Op.MOD -> a mod b
                | Op.EQ ->  if a = b then 1 else 0
                | Op.NE ->  if a = b then 0 else 1
                | Op.LE ->  if a < b then 1 else 0
                | Op.OR ->  if a <> 0 || b <> 0 then 1 else 0
                | Op.AND -> if a =  0 || b =  0 then 0 else 1
                | Op.SHL ->
                   
                   let r = a * (int_of_float ((float_of_int 2) ** (float_of_int b))) in
                   
                   r
                | Op.SHR ->
                   
                   let r = a / (int_of_float ((float_of_int 2) ** (float_of_int b))) in
                   
                   r
                | _ -> 0
              end
            in
            CONST r
         | a, b when o = Op.SUB && a = b -> CONST 0
         | a, CONST 0 when o = Op.SUB -> a
         | CONST 0, b when o = Op.MUL -> CONST 0
         | a, CONST 0 when o = Op.MUL -> CONST 0
         | CONST 1, b when o = Op.MUL -> b
         | a, CONST 1 when o = Op.MUL -> a
         | CONST 0, b when o = Op.ADD -> b
         | a, CONST 0 when o = Op.ADD -> a
         | CONST 0, b when o = Op.DIV -> CONST 0
         | a, CONST 0 when o = Op.DIV -> POSINF
         | a, CONST 1 when o = Op.DIV -> a
         | BINOP (a, Op.ADD, b), c when a=c && o = Op.SUB -> b
         | BINOP (a, Op.ADD, b), c when b=c && o = Op.SUB -> a
         (* | BINOP (a, o2, CONST b), CONST c when -> pn "++"; (BINOP (a, o2, eval1 (BINOP(CONST b, o, CONST c)))) *)
         | a, b ->
            BINOP (a, o, b)
         in
         res
       end
    | REF (ADDR (VAR v)) -> VAR v
    | REF e ->
       begin
         match (eval1 e) with
           ADDR (VAR v) -> VAR v
         | e1 -> REF e1
       end
    | INDICES x -> if List.length x = 1 then List.hd x else INDICES x
    | (SIZEOF s) as x ->
       if is_simple_type s then
         CONST (simple_size s)
       else
         if V.cardinal structures = 0 then
           x
         else
           if V.mem s structures then
             CONST (_struct_size_by_name structures s)
           else(
             dbg "WARNING" "Undefined Type:" p s;
             (** crypto_ec_ecp_nistz256_c : sizeof bignum_st  *)
             CONST 4
           (* V.iter (fun k v -> pn k) structures; pn "\n@@@@@@"; *)
           (* raise (StError ("Unsupported type for sizeof " ^ s)) *)
             
           )
    | x -> x
    in
    let res = bitwise @@ eval1 exp in
    (* match res with
      NEG e -> BINOP (CONST 0, Op.SUB, e)
    | _ ->*)  res
         
             
  let rec sum_of_mul_exp (exp : t) : (t * t) list = (** x+1-x *)
      match eval exp with
        NOTHING -> [(NOTHING, NOTHING)]
      | STRING (s) -> [(NOTHING, NOTHING)]
      | NEGINF -> [(NEGINF, CONST 1)]
      | POSINF -> [(POSINF, CONST 1)]
      | UNDEF -> [(UNDEF, UNDEF)]
      | VAR (s) -> [(VAR s, CONST 1)]
      | CONST (i) -> [(CONST 1, CONST i)]
      | FLOAT (f) -> [(FLOAT f, CONST 1)]
      | NEG e -> [(e, CONST (-1))]
      | BINOP (e1, op, e2) ->
         begin
           let es1 = sum_of_mul_exp e1 in
           let es2 = sum_of_mul_exp e2 in
           match op with
             Op.ADD -> (** (c1v1+c2v2) + (c3v3+c4v4) *)
             es1 @ es2
           | Op.SUB -> (** (c1v1+c2v2) - (c3v3+c4v4) *)
              let es2' = (fun (v,c) -> (v, eval (BINOP (CONST 0, Op.SUB, c)))) |>>| es2 in
              es1 @ es2'
           | Op.MUL ->
              (** (c1v1+c2v2) * (c3v3+c4v4) *)
              List.concat ((fun (v1,c1) -> (fun (v2, c2) -> 
                              (v1, 
                               BINOP (
                                   BINOP (c1, Op.MUL, c2),
                                   Op.MUL,
                                   v2))
                            ) |>>| es2) |>>| es1)
           | Op.DIV ->
              (** (c1v1+c2v2) / (c3v3+c4v4) *)
              List.concat ((fun (v1,c1) -> (fun (v2, c2) -> 
                              (eval (BINOP (v1, Op.DIV, v2)), eval (BINOP (c1, Op.DIV, c2)))
                            ) |>>| es2) |>>| es1)
           | _ -> [(exp, CONST 1)]
         end
      | e -> [(e, CONST 1)]
      
  let norm exp =
    (*
    pf_s' "NORM" @@
      Format.printf "%a :: Original\n%a :: + of *\n"
                    pp_printable exp 
                    (pp_list_with (fun ppf (a,b) -> Format.fprintf ppf "%a*%a" pp_printable a pp_printable b) "+") expanded_exp;
    *)
    pf_s "NORM" pprint exp;
    pn_s "NORM" " :: Original";
    let expanded_exp = sum_of_mul_exp exp in
    pf_s "NORM" (iterS (fun (a,b) -> pprint a; p "*"; pprint b) "+") expanded_exp;
    pn_s "NORM" " :: + of *";
    let (common_collections, _) = (fun ((a : (t * (t*t) list) list),vs) (v,c) ->
        let rec get_coeff v c =
          match v with
          | BINOP (e1, Op.MUL, e2) ->
             let (v'', c'') = get_coeff e1 c in
             (v'', eval (BINOP (c'', Op.MUL, e2)))
          | _ -> (v, c)
        in
        let v', c' = get_coeff v c in

        if v |<- vs then
          ((fun ((x : t), (es : (t * t) list)) -> (x, if x = v' then es @ [(v',c')] else es)) |>>| a, vs)
        else
          (a @ [(v', [(v',c')])], (vs @ [v']))
      ) |->> (([],[]), expanded_exp) in

    let commons = (fun ((v : t), (cs' : (t * t) list)) ->
        (*
        pf_s' "NORM" @@
          Format.printf "v: %a* (%a)"
                        pp_printable v
                        (pp_list_with (fun ppf (_,b) -> pp_printable ppf b) "+") cs';
        *)
        p_s "NORM" "v: ";
        pf_s "NORM" pprint v;
        p_s "NORM" "* (";
        pf_s "NORM" (iterS (fun (a,b) -> pprint b) "+") cs';
        pn_s "NORM" ")";
        match cs' with
          [] -> CONST 0
        | (_, c)::cs ->
           let m = (fun c (_, c') -> BINOP (c, Op.ADD, c')) |->> (c, cs) in
           let r = BINOP (v, Op.MUL, m) in
          (*
           pf_s' "NORM" @@
            Format.printf "r: %a\n" pp r;
           *)
           p_s "NORM" "r: ";
           pf_s "NORM" pprint r;
           pn_s "NORM" "";
           r
      ) |>>| common_collections in

    let r = (fun a e -> eval (BINOP (a, Op.ADD, e))) |->> (CONST 0, commons) in
    (*
    pf_s' "NORM" @@
      Format.printf "res: %a\n" pp r;
    *)
    p_s "NORM" "res: ";
    pf_s "NORM" pprint r;
    pn_s "NORM" "";
    eval r
  (** a + 2*a*b + b = a*(1+2*b) + b OR b*(1+2*a)+a *)
    
(*
	let rec toExp ((fvs, to_statics, arrays, (structures: (string * t list) V.t), (aliases: string V.t)) as packs) exp =
    match exp with
      Cabs.UNARY (unary_operator, expression) ->
      begin
		    match unary_operator with
	        Cabs.MINUS -> BINOP (CONST 0, Op.SUB, toExp packs expression)
        | Cabs.PLUS -> toExp packs expression
        | Cabs.BNOT ->
           let exp = toExp packs expression in
           bit_not exp
           (* let neg_one = Cabs.UNARY (Cabs.MINUS, Cabs.CONSTANT (Cabs.CONST_INT "1")) in
	         toExp (Cabs.BINARY (Cabs.XOR, neg_one, expression)) *)
        | Cabs.MEMOF -> REF (toExp packs expression)
        | Cabs.ADDROF ->
           let exp' = toExp packs expression in
           begin
             match exp' with
               VAR (vname,_) when V.mem vname fvs ->
                let attr = V.find vname fvs in
                if List.exists (function STRUCT _ -> true | _ -> false) attr then
                  exp'
                else
                  ADDR exp'
             | _ -> ADDR exp' 
           end
        | _ -> qvar ()
	    end
    | Cabs.MEMBEROFPTR (exp, field) -> ARROW (toExp packs exp, field)
    | Cabs.LABELADDR (str) -> qvar ()
    | Cabs.BINARY (binary_operator, expression1, expression2) ->
       begin
         match binary_operator with
           Cabs.GT ->
            raise ExpToBExp
         | _ ->
            try
              let exp' = BINOP (toExp packs expression1, Op.toOp binary_operator, toExp packs expression2) in
              bitwise @@ exp'
            with
              e -> Cprint.print_expression exp; pn ""; raise e
       end
    | Cabs.CAST ((specifier, decl_type), init_expression) ->
       begin
  		   match init_expression with
           Cabs.NO_INIT -> qvar ()
         | Cabs.SINGLE_INIT (expression) -> toExp packs expression
         | Cabs.COMPOUND_INIT _ -> qvar ()
  	   end
    | Cabs.CONSTANT (constant) ->
  	   begin
         match constant with
  	       Cabs.CONST_INT ( str ) ->
            
           let l = (String.length str) - 2 in
           if l > 0 && String.sub str 0 2 = "0x" then
             try
               if String.length str > 20 then
                 begin
                   (* pn (String.sub str 0 (l-1));
                   pn (Int64.to_string (Int64.of_string (String.sub str 0 (l-1)))); 
                   (* pn (String.sub str 3 (l-1)); *)
                   CONST (Scanf.sscanf (String.sub str 3 (l-1)) "%x" (fun x -> x)) *)
                   NOTHING
                 end
               else
                 begin
                   (* pn (String.sub str 2 l); *)
                   
                   CONST (Scanf.sscanf (String.sub str 2 l) "%x" (fun x -> x))
                 end
             with
               _ -> CONST 0
           else if l > 0 && String.sub str l 2 = "UL" then
             begin
               try
                 let l' = if l > 5 then 5 else l in  (** Approximation *)
                 let str' = String.sub str 0 l' in
                 CONST (int_of_string str')
               with
                 _ -> CONST 1
             end
	         else
             let l = (String.length str) - 1 in
             if l > 0 && String.get str l = 'L' then
               begin
                 try
                   (* let l' = if l > 5 then 5 else l in  (** Approximation *) *)
                   CONST (int_of_string (String.sub str 0 l))
                 with
                   _ -> CONST 1
               end
             else
               begin
  	             try
  		             CONST (int_of_string str)
  	             with
                 | _ -> (** pw "<BUG>"; pw str;*) CONST 1
  				     end
         | Cabs.CONST_FLOAT ( str ) ->
            let fl = try float_of_string str with _ -> 0.0 in
            let ifl = int_of_float fl in
            if float_of_int ifl = fl then
              CONST ifl
            else
              ( p "WARNING: Found Constant Float:"; pn str; CONST ifl ) (* qvar () *) (* FLOAT (float_of_string str) *)
         | Cabs.CONST_CHAR (l_int64) ->
            begin
              try
                CONST (Int64.to_int (List.hd l_int64))
              with
                _ -> raise (StError("toExp - Cabs.CONST_CHAR"))
            end
         | Cabs.CONST_WCHAR (l_int64) -> qvar ()
         | Cabs.CONST_STRING ( str ) -> qvar ()
         | Cabs.CONST_WSTRING (l_int64) -> qvar ()
  	   end
    | Cabs.PAREN (expression) -> toExp packs expression
    | Cabs.VARIABLE ( str ) -> (string_to_var str)
    | Cabs.INDEX (expression1, expression2) ->
       begin
         let exp1 = toExp packs expression1 in
	       let exp2 = toExp packs expression2 in
         match exp1 with
           BINOP (var, Op.ADD, INDICES (indices)) -> BINOP (var, Op.ADD, INDICES (indices @ [exp2]))
         | var -> BINOP (var, Op.ADD, INDICES ([exp2]))
	     end
    | Cabs.TYPE_SIZEOF (sp, dt) ->
       begin
         let res =
           try
             if VCabs.is_cabs_ptr dt then
               SIZEOF "long"
             else
               cabs_size_of_to_sizeof fvs structures aliases sp
           with
             e ->
             pi (List.length sp);
             Cprint.print_expression exp; pn "";
             begin
               (match dt with
                 Cabs.JUSTBASE -> pn "@JUSTBASE";
               | Cabs.PARENTYPE _ -> pn "@PARENTYPE";
               | Cabs.PTR _ -> pn "@PTR";
               | Cabs.PROTO _ -> pn "@PROTO"
               | Cabs.ARRAY _ -> pn "@ARRAY");
                 raise e
              end
         in
         res
       end
    | Cabs.EXPR_SIZEOF (exp) ->
       size_of_expr fvs structures aliases exp
    | Cabs.CALL (Cabs.VARIABLE (s), exp) when String.length s > 0 && String.sub s 0 1 = "@" -> 
       let exp' = try toExp packs (List.hd exp) with _ -> raise (StError "toExp CALL" ) in
       LBL (s, exp')
    | Cabs.COMMA (l_exp) ->
       toExp packs @@ List.hd (List.rev l_exp)
    | Cabs.QUESTION (exp1, exp2, exp3) ->
       let exp1' = toExp packs exp1 in
       let exp1'' = eval exp1' in
       begin
         match exp1'' with
           CONST x ->
            if x = 1 then
              begin
                let exp2'  = toExp packs exp2 in
                let exp2'' = eval exp2' in
                exp2''
              end
            else
              begin
                let exp3'  = toExp packs exp3 in
                let exp3'' = eval exp3' in
                exp3''
              end
         | _ ->
            CONST 1
       end
    | _ -> qvar ()
 *)

    (*
	let toIndices packs indices =
		let rec ti = function
			| [] -> []
			| x::xs ->
				let xs' = ti xs in
				((toExp packs x)::xs')
		in
		INDICES (ti indices)
     *)
    
	let encode s =
    VAR s

	let rec decode = function
	  | VAR s -> s
    | INDICES (e2) ->
       let x = decode (List.hd e2) in
       x
    | BINOP (x1, _, _) -> decode x1
    | CONST x -> (string_of_int x, [])
		| _ -> ("", [])

	let compare a b = match a, b with
  	| VAR x, VAR y -> var_compare x y
    | FCALL (s1,t1), FCALL (s2, t2) -> let r = String.compare s1 s2 in r
    | _, FCALL (_, _) -> -1
    | FCALL (_, _), _ -> 1
  	| _ -> 0

 let rec fv = function
   | VAR x ->
      if x = size_loc then
        []
      else
        (var_fv x)
   | INDICES (e2) -> List.concat ( fv |>>| e2 )
   | ADDR e -> fv e
   | REF e -> fv e
   | NEG e -> fv e
   | LBL (lbl, e) -> fv e
   | BINOP (x, _, y) -> (fv x) @@@ (fv y)
   | FCALL (_, ts) -> List.concat (fv |>>| ts)
   | ARROW (e, _) -> fv e 
   | _ -> []


 let rec does_contain_ifz = function
    INDICES xs when List.length xs >=3 -> true
  | BINOP (p1, _, p2) -> (does_contain_ifz p1) || (does_contain_ifz p2)
  | _ -> false

 let rec add_attr attr = function
		| (VAR _) as x -> var_add attr x
		| BINOP (x, Op.ADD, y) -> BINOP (add_attr attr x, Op.ADD, y)
		| x -> x

 let rec substitute to_be by e =
    match e with
	  | VAR v    ->
       if eq (VAR v) to_be then
         begin
           by
         end
       else
         begin
           VAR v
         end
	  | BINOP (o1, op, o2) -> BINOP (substitute to_be by o1, op, substitute to_be by o2)
	  | INDICES tl -> INDICES ( (substitute to_be by) |>>| tl)
    | REF e ->
       begin
         match by with
           ADDR v when e = to_be -> v
         | _ -> REF (substitute to_be by e)
       end
    | ADDR e ->
       begin
         match by with
           REF v when e = to_be -> v
         | _ -> ADDR (substitute to_be by e)
       end
    | FCALL (s, ts) ->
       if eq e to_be then
			   by
		   else
			   FCALL (s, (substitute to_be by) |>>| ts)
    | x -> x

 let rec par_subs subs_pairs e' =
   match e' with
   | VAR v ->
      VAR (var_par_subs subs_pairs v)
	 | BINOP (o1, op, o2) -> BINOP (par_subs subs_pairs o1, op, par_subs subs_pairs o2)
	 | INDICES tl -> INDICES ( (par_subs subs_pairs) |>>| tl)
   | REF e ->
      REF (par_subs subs_pairs e)
   | ADDR e ->
      ADDR (par_subs subs_pairs e)
   | FCALL (s, ts) ->
      FCALL (s, (par_subs subs_pairs) |>>| ts)
   | NEG (e) -> NEG (par_subs subs_pairs e)
   | ARROW (t, f) -> ARROW (par_subs subs_pairs t, f)
   | LBL (lbl, e) -> LBL (lbl, par_subs subs_pairs e)
   | _ -> e'
         
(** Interval logic *)

  let min x y =
    match x, y with
      NEGINF, _ -> NEGINF
    | _, NEGINF -> NEGINF
    | x, POSINF -> x
    | POSINF, x -> x
    | CONST a, CONST b -> if a < b then x else y
    | BINOP (a, op, CONST 1), b when a = b -> BINOP (a, Op.SUB, CONST 1)
    | b, BINOP (a, op, CONST 1) when a = b ->
       BINOP (a, Op.SUB, CONST 1)
    | BINOP (a, Op.ADD, CONST 1), b when a = b -> b
    | b, BINOP (a, Op.ADD, CONST 1) when a = b -> b
    | _, _ -> (BINOP (x, Op.MIN, y))

  let max x y =
    match x, y with
      NEGINF, x -> x
    | x, NEGINF -> x
    | _, POSINF -> POSINF
    | POSINF, _ -> POSINF
    | CONST a, CONST b -> if a > b then x else y
    | BINOP (a, Op.SUB, CONST 1), b when a = b ->
      b
    | b, BINOP (a, Op.SUB, CONST 1) when a = b ->
      b
    | BINOP (a, Op.ADD, CONST 1), b when a = b ->
       BINOP (a, Op.ADD, CONST 1)
    | b, BINOP (a, Op.ADD, CONST 1) when a = b ->
      BINOP (a, Op.ADD, CONST 1)
    | _, _ -> BINOP (x, Op.MIN, y)


  let rec get_interval (intv : (var_t * (t * t)) list) ys = function
    NOTHING -> (UNDEF, UNDEF)
  | NEGINF  -> (NEGINF, UNDEF)
  | POSINF  -> (UNDEF, POSINF)
  | VAR y when y |<- ys -> (VAR y, VAR y)
  | VAR x ->
    begin
      try
        snd (List.find (fun (y, _) -> y = x) intv)
      with
      _ -> (NEGINF, POSINF)
    end
  | BINOP (c, o, d) -> (** Negetive number is not handled for multiplication *)
    let (l1, r1) = get_interval intv ys c in
    let (l2, r2) = get_interval intv ys d in
    let l3 = eval (BINOP (l1, o, l2)) in
    let r3 = eval (BINOP (r1, o, r2)) in
    (l3, r3)
  | x -> (x, x)
;;

let op p q o =
  eval (norm (BINOP (p, o, q)))

let is_fp = function
    FCALL (_, _) -> true
  | _ -> false

let rec with_head = function
    VAR (x) -> true
  | BINOP (x, Op.ADD, _) -> with_head x
  | FCALL (_, _) -> false
  | e -> false

let rec head errmsg = function
    (VAR _) as x -> x
  | BINOP (x, _, _) -> head errmsg x
  | CONST 0 -> raise (NullPointerException errmsg)
  (* | ARROW (t, _) -> head errmsg t *)
  | REF e -> head errmsg e (** temp fix 
                               for (bb = b[3]; *bb == '0'; bb++) ;
                               @ line 11038 in apps.c *)
  (* | ADDR (e) -> head errmsg e (* TODO: Recheck *) *)
  | e -> let est = toStr e in raise (StError ("No head found:" ^ errmsg ^ ": " ^ est))


let get_int loc = function
    CONST i -> i
  | e ->
     p "Error at: ";
     print_endline (Locs.print loc);
     pprint e;
     raise (StError "Illegal Integer (Exp)")
end;;


module Term = struct
	type t =
		| EXP of Exp.t [@printer fun ppf -> fprintf ppf "EXP (%a)" Exp.pp]
    (* | FCALL of string * t list *)
		| NULL
    [@@deriving show {with_path = false}]

  let pp' ppf = function
    | NULL -> Format.pp_print_string ppf "nil"
    | EXP x -> Exp.pp' ppf x

  let fstr () = function
    | NULL -> Format.sprintf "nil"
    | EXP x -> Format.sprintf "%a" Exp.fstr x
             
  (* let pprint = pp' Format.std_formatter *)
	let pprint = function
		| NULL -> p "nil"
		| EXP x -> Exp.pprint x

  (* let print = pp Format.std_formatter *)
  let print = function
    | NULL -> p "NULL"
		| EXP x -> p "EXP ("; Exp.print x; p ")"
   (* | FCALL (s, ts) -> p s; p "("; iterS pprint "," ts; p ")" *)
               
	let toStr = function
		| NULL -> "nil"
		| EXP x -> Exp.toStr x
  (*  | FCALL (s, ts) -> s ^ "(...)" *)
             
	let toExp = function
		| NULL -> Exp.CONST 0
		| EXP x -> x
   (* | FCALL (_,_) -> Exp.NOTHING *)

  let approx = function
		| EXP x -> EXP (Exp.approx x)
    | t -> t

	let zero = EXP (Exp.CONST 0)
  let one = EXP (Exp.CONST 1)
  let size_loc = EXP (Exp.VAR Exp.size_loc)

  let is_const = function
      NULL -> false
    | EXP e -> Exp.is_const e;;
  
  let eq a b =
    match a, b with
    | NULL, NULL -> true
    | EXP (x), EXP (y) -> Exp.eq x y
    | _ -> false

  let be_typed vars to_static loc = function
    | EXP x ->
      EXP (Exp.be_typed vars to_static loc x)
    | x -> x

  let infer_attributes vars to_static loc = function
      NULL -> [Exp.PTR]
    | EXP exp -> Exp.infer_attributes vars to_static loc exp

  let term__string (tr_exp : Exp.t -> Exp.t) = function
    | NULL -> NULL
    | EXP exp -> EXP (tr_exp exp)

  let enptr = function
      NULL -> NULL
    | EXP e -> EXP (Exp.enptr e)
  
	let encode x =
	 	EXP (Exp.encode x)

	let encode_str x = EXP (Exp.string_to_var x)

  let decode = function
		| NULL -> ("nil", [])
		| EXP (s) -> Exp.decode s

	let rec substitute (to_be_replaced:t) (replaced_by:t) (_t:t) : t =
    match replaced_by with
      NULL ->
      if eq _t to_be_replaced then
			  replaced_by
		  else
			  _t
    | EXP e_by ->
       begin
         match _t, to_be_replaced with
           NULL, _ -> _t
         | _, NULL -> _t
         | EXP _e_t, EXP e_to_be -> EXP (Exp.substitute e_to_be e_by _e_t)
       end
      
  let par_subs subs_pairs t =
    match t with
      NULL -> NULL
    | EXP e ->
       let subs_e (e : Exp.t) =
         try
           let (_, by) = List.find (fun (tobe, _) ->
                             tobe = EXP e) subs_pairs in
           match by with
             EXP eby -> eby
           | _ -> raise (StError "substitution by NULL is confusing")
         with
           _ -> e
       in
       let rec aux e =
         let e' = subs_e e in
         match e' with
         | Exp.VAR v ->
            subs_e e
(*            begin
              try
                let (_, by) = List.find (fun (x,_) -> fst (decode x) = fst v) subs_pairs in
                match by with
                  NULL -> raise (StError "substitution by NULL is confusing")
                | EXP e -> e
              with
                Not_found -> e 
            end *)
	       | Exp.BINOP (o1, op, o2) ->
            let o1' = subs_e o1 in
            let o2' = subs_e o2 in
            Exp.BINOP (aux o1', op, aux o2')
	       | Exp.INDICES tl -> Exp.INDICES (aux |>>| tl)
         | Exp.REF e ->
            Exp.REF (aux e)
         | (Exp.ADDR e as fc) ->
            begin
              try
                let (_, by) = List.find (fun (to_be,_) ->
                                  match to_be with
                                    EXP e -> e = fc
                                  | _ -> false
                                ) subs_pairs in
                Exp.VAR (decode by)  
              with
                _ -> Exp.ADDR (aux e)
            end
         | (Exp.FCALL (s, ts) as fc) ->
            begin
              try
                let (_, by) = List.find (fun (to_be,_) ->
                                  match to_be with
                                    EXP e -> e = fc
                                  | _ -> false
                                ) subs_pairs in
                Exp.VAR (decode by)  
              with
                _ -> Exp.FCALL (s, aux |>>| ts)
            end
         | Exp.NEG (e) -> Exp.NEG (aux e)
         | Exp.ARROW (t, f) -> Exp.ARROW (aux t, f)
         | Exp.LBL (lbl, e) -> Exp.LBL (lbl, aux e)
         | _ -> e
       in
       EXP (aux e)
 (*
	let toIndices packs exp = EXP (Exp.toIndices packs exp)
  *)
  let find term strs : t =
  		try
			List.find (eq term) (encode |>>| strs)
  		with
			| _ -> raise (StError "Base-2")

	let add_attr attr = function
		| EXP x -> EXP (Exp.add_attr attr x)
    | x -> x

	let rec compare x y = match x, y with
  	| NULL, _ -> 1
  	| _, NULL -> -1
  	| EXP a, EXP b -> Exp.compare a b


  let rec fv = function
		| NULL -> []
		| EXP x -> Exp.fv x

  let rec eval = function
      NULL -> NULL
    | EXP x -> EXP (Exp.eval x)

  let rec map f = function
    | NULL -> NULL
    | EXP x -> EXP (Exp.map f x)

    (* | FCALL (s, ts) -> pw "FCALL" *)

  let get_interval intv ys = function
    NULL -> (Exp.UNDEF, Exp.UNDEF)
  | EXP x -> Exp.get_interval intv ys x
  
  let op p q o =
    match p, q with
      EXP p', EXP q' ->
      let r = Exp.norm (Exp.BINOP (p', o, q')) in
      (*
      pf_s' "NORM" @@
        Format.printf "norm(p o q)%a\n" Exp.pp r;
      *)
      p_s "NORM" "norm(p o q)";
      pf_s "NORM" Exp.pprint r;
      pn_s "NORM" "";
      eval (EXP r)
    | _ -> NULL

  let bin_op p q o =
    match p, q with
      EXP p', EXP q' ->
      let r = (Exp.BINOP (p', o, q')) in
      
      EXP r
    | _ -> NULL
         

  let with_head = function
      NULL -> false
    | EXP x -> Exp.with_head x

  let is_fp = function
      NULL -> false
    | EXP x -> Exp.is_fp x

  let head errmsg = function
      NULL -> raise (StError "No NULL expected")
    | EXP x -> Exp.head errmsg x

  let get_int loc = function
      EXP e -> Exp.get_int loc e
    | _ ->
       print_endline (Locs.print loc);
       raise (StError "Illegal Integer")

end;;

module Field = struct
	type t = string

  let compare = String.compare
  (* let to_str = Exp.var_to_str *)
end;;

module BExp = struct
  type t =
    | UNIT of Term.t * Op.t * Term.t [@printer fun ppf (e1,op,e2) -> fprintf ppf "BExp.UNIT (%a,%a,%a)" Term.pp e1 Op.pp op Term.pp e2]
		| OP of t * Op.t * t
		| BLOCK of Exp.t * Term.t [@printer fun ppf (s, e) -> fprintf ppf "BExp.BLOCK (%a,%a)" Exp.pp s Term.pp e]
    | LBL of string * t
		| NOTHING
    [@@deriving show]

  let rec pp' ppf = function
      NOTHING -> Format.pp_print_string ppf "NOT SUPPORTED"
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1 = t4 && t2 = t3 ->
        Format.fprintf ppf "@[<2>(%a@,<=@,%a)@]" Term.pp' t3 Term.pp' t4
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1=t3 && t2=t4 ->
        Format.fprintf ppf "@[<2>(%a@,<=@,%a)@]" Term.pp' t3 Term.pp' t4
    | OP (UNIT(t1, Op.LE, t2), Op.OR, UNIT(t3, Op.EQ, t4)) when t1=t3 && t2=t4 ->
        Format.fprintf ppf "@[<2>(%a@,<=@,%a)@]" Term.pp' t1 Term.pp' t2
    | OP (e1, op, e2) ->
        Format.fprintf ppf "@[<2>(%a@;%a@;%a)@]" pp' e1 Op.pp' op pp' e2
    | UNIT (e1, op, e2) when op != Op.MAPSTO -> 
        Format.fprintf ppf "@[<2>(%a@,%a@,%a)@]" Term.pp' e1 Op.pp' op Term.pp' e2
    | BLOCK (str, exp) -> 
        Format.fprintf ppf "@[<2>Block@,<%a,@,%a>@]" Exp.pp' str Term.pp' exp
    | UNIT (e1, op, e2) ->
        Format.fprintf ppf "@[(%a@,%a@,%a)@]" Term.pp' e1 Op.pp' op Term.pp' e2
    | LBL (st, b) ->
        Format.fprintf ppf "@[<2>%s@;%a@]" st pp' b

  let rec fstr () = function
      NOTHING -> "NOT SUPPORTED"
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1 = t4 && t2 = t3 ->
        Format.sprintf "@[<2>(%s@,<=@,%s)@]" (Term.toStr t3) (Term.toStr t4)
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1=t3 && t2=t4 ->
        Format.sprintf "@[<2>(%s@,<=@,%s)@]" (Term.toStr t3) (Term.toStr t4)
    | OP (UNIT(t1, Op.LE, t2), Op.OR, UNIT(t3, Op.EQ, t4)) when t1=t3 && t2=t4 ->
        Format.sprintf "@[<2>(%s@,<=@,%s)@]" (Term.toStr t1) (Term.toStr t2)
    | OP (e1, op, e2) ->
        Format.sprintf "@[<2>(%s@;%s@;%s)@]" (fstr () e1) (Op.pprint op) (fstr () e2)
    | UNIT (e1, op, e2) when op != Op.MAPSTO -> 
        Format.sprintf "@[<2>(%s@,%s@,%s)@]" (Term.toStr e1) (Op.pprint op) (Term.toStr e2)
    | BLOCK (str, exp) -> 
        Format.sprintf "@[<2>Block@,<%s,@,%s>@]" (Exp.toStr str) (Term.toStr exp)
    | UNIT (e1, op, e2) ->
        Format.sprintf "@[(%s@,%s@,%s)@]" (Term.toStr e1) (Op.pprint op) (Term.toStr e2)
    | LBL (st, b) ->
        Format.sprintf "@[<2>%s@;%s@]" st (fstr () b)

    (* let pprint = pp' Format.std_formatter *)
  let rec pprint = function
      NOTHING -> p "NOT SUPPORTED"
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1=t4 && t2=t3 -> p "("; Term.pprint t3; p "<="; Term.pprint t4; p ")"
    | OP (UNIT(t1, Op.EQ, t2), Op.OR, UNIT(t3, Op.LE, t4)) when t1=t3 && t2=t4 -> p "("; Term.pprint t3; p "<="; Term.pprint t4; p ")"
    | OP (UNIT(t1, Op.LE, t2), Op.OR, UNIT(t3, Op.EQ, t4)) when t1=t3 && t2=t4 -> p "("; Term.pprint t1; p "<="; Term.pprint t2; p ")"
    | OP (e1, op, e2) -> p "("; pprint e1; pw (" " ^ (Op.pprint op)); pprint e2; p ")"
    | UNIT (e1, op, e2) when op != Op.MAPSTO -> p "("; Term.pprint e1;  p (Op.pprint op); Term.pprint e2; p ")"
    | BLOCK (str, exp) -> p "Block<"; p (Exp.get_printable_string str); p ","; Term.pprint exp; p ">"
    | UNIT (e1, op, e2) -> p "("; Term.pprint e1;  p (Op.pprint op); Term.pprint e2; p ")"
    | LBL (st, b) -> p (st ^ " "); pprint b

  (* let print = pp Format.std_formatter *)
  let rec print = function
      NOTHING -> p "NOTHING"
    | UNIT (e1, op, e2) ->
       p "UNIT (";
       Term.print e1;
       p ",";
       p (Op.print op);
       p ",";
       Term.print e2;
       p ")"
    | LBL (st, b) ->
       p "BExp.LBL (";
       p ("\"" ^ st ^ "\"");
       p ",";
       print b;
       p ")"
    | OP (e1, op, e2) ->
       p "OP (";
       print e1;
       p ",";
       p (Op.print op);
       p ",";
       print e2;
       p ")"
    | BLOCK (str, exp) ->
       p "BLOCK (";
       Exp.print str;
       p ",";
       Term.print exp;
       p ")"
    
                     
  let _T = UNIT (Term.zero, Op.EQ, Term.zero)

  let _F = UNIT (Term.zero, Op.NE, Term.zero)

  let (==) a b =
    (* Term.print a; pn "";
    Term.print b; pn "\n"; *)
    if a = b then
      _T
    else
      UNIT (a, Op.EQ, b)

  let (=.) a b = UNIT (a, Op.EQ, b)
    
  let (<.) a b =
    match a, b with
    | Term.EXP (Exp.CONST a'), Term.EXP (Exp.CONST b') when a' < b' ->
       _T
    | Term.EXP (Exp.CONST a'), Term.EXP (Exp.CONST b') ->
       _F
    | _ ->
       UNIT (a, Op.LE, b)      
      
  let (=/) a b =
    match a, b with
    | Term.EXP (Exp.CONST a'), Term.EXP (Exp.CONST b') when a' <> b' ->
       _T
    | Term.EXP (Exp.CONST a'), Term.EXP (Exp.CONST b') ->
       _F
    | _ ->
       UNIT (a, Op.NE, b)

  let unit a o b =
    match o with
      Op.EQ -> a == b
    | Op.LE -> a <. b
    | Op.NE -> a =/ b
    | _ -> raise Error

  let (&.) a b =
    if a = b || a = _T then
      b
    else if b = _T then
      a
    else if a = _F || b = _F then
      _F
    else
      OP (a, Op.AND, b)

  let (|.) a b =
    if a = b || a = _F then
      b
    else if b = _F then
      a
    else if a = _T || b = _T then
      _T
    else
      OP (a, Op.OR, b)

  let (<=.) (a:Term.t) (b:Term.t) =
    (a <. b) |. ( a =. b)

    
  let op a o b =
    match o with
      Op.AND -> a &. b
    | Op.OR ->  a |. b
    | _ -> raise Error

 let (&&.) bs a =
   if a |<- bs then
     bs
   else if a = _T then
     bs
   else if a = _F then
     [_F]
   else
     a::bs

 let (&&~) bs cs =
   (&&.) |->> (bs, cs)

 let non_det = UNIT (Term.EXP (Exp.VAR (":(",[])), Op.EQ, Term.EXP (Exp.VAR (":)",[])));;
   
 let rec has_fsymb = function
      UNIT (Term.EXP Exp.FCALL _, _, _) -> true
    | UNIT (_, _, Term.EXP Exp.FCALL _) -> true
    | UNIT _ -> false
    | OP (x, _, y) -> has_fsymb x || has_fsymb y
    | _ -> false

  let rec bexp__string (tr_exp : Exp.t -> Exp.t) = function
    | OP (e1, op, e2) -> OP (bexp__string tr_exp e1, op, bexp__string tr_exp e2)
    | UNIT (e1, op, e2) -> UNIT (Term.term__string tr_exp e1, op, Term.term__string tr_exp e2)
    | BLOCK (str, exp) -> BLOCK (str, Term.term__string tr_exp exp)
    | LBL (lbl, bexp) -> LBL (lbl, bexp__string tr_exp bexp)
    | x -> x

  let rec map f = function
    | OP (e1, op, e2) -> OP (map f e1, op, map f e2)
    | UNIT (e1, op, e2) -> UNIT (Term.map f e1, op, Term.map f e2)
    | BLOCK (str, exp) -> BLOCK (str, Term.map f exp)
    | LBL (lbl, bexp) -> LBL (lbl, map f bexp)
    | x -> x

  let rec be_typed vars to_static loc = function
    | UNIT  (x, op, y) -> UNIT  (Term.be_typed vars to_static loc x, op, Term.be_typed vars to_static loc y)
    | OP    (x, op, y) -> OP    (     be_typed vars to_static loc x, op,      be_typed vars to_static loc y)
    | BLOCK (var,  e)  -> BLOCK ( Exp.be_typed vars to_static loc var,   Term.be_typed vars to_static loc e)
    | LBL (lbl, b)     -> LBL   (lbl, be_typed vars to_static loc b)
    | NOTHING -> NOTHING

  let rec complement = function
      UNIT (a, Op.EQ, b) -> UNIT (a, Op.NE, b)
    | UNIT (a, Op.NE, b) -> UNIT (a, Op.EQ, b)
    | UNIT (a, Op.LE, b) -> OP (UNIT (b, Op.EQ, a), Op.OR, UNIT (b, Op.LE, a))
    | OP (UNIT (a, Op.EQ, b), Op.OR, UNIT (c, Op.LE, d)) when (Term.eq a c) && (Term.eq b d) -> UNIT (b, Op.LE, a)
    | OP (UNIT (b, Op.EQ, a), Op.OR, UNIT (c, Op.LE, d)) when (Term.eq a c) && (Term.eq b d) -> UNIT (b, Op.LE, a)
    | OP (UNIT (c, Op.LE, d), Op.OR, UNIT (a, Op.EQ, b)) when (Term.eq a c) && (Term.eq b d) -> UNIT (b, Op.LE, a)
    | OP (UNIT (c, Op.LE, d), Op.OR, UNIT (b, Op.EQ, a)) when (Term.eq a c) && (Term.eq b d) -> UNIT (b, Op.LE, a)
    | OP (a, Op.AND, b) -> OP (complement a, Op.OR, complement b)
    | OP (a, Op.OR, b) ->  OP (complement a, Op.AND, complement b)
    | LBL (lbl, b) -> LBL (lbl, complement b)
    | x -> x
(*
  let rec toBExp packs loc = function
      Cabs.VARIABLE (str) -> UNIT (Term.toTerm packs (Cabs.VARIABLE (str)), Op.NE, Term.zero)
    | Cabs.CONSTANT (Cabs.CONST_INT (str)) -> UNIT (Term.toTerm packs (Cabs.CONSTANT (Cabs.CONST_INT (str))), Op.NE, Term.zero)
    | Cabs.UNARY (Cabs.NOT, Cabs.VARIABLE (str)) -> UNIT (Term.EXP (Exp.string_to_var str), Op.EQ, Term.zero)
    | Cabs.UNARY (Cabs.NOT, Cabs.UNARY (Cabs.NOT, exp)) -> toBExp packs loc exp
    | Cabs.UNARY (Cabs.NOT, Cabs.PAREN (exp)) -> toBExp packs loc (Cabs.UNARY (Cabs.NOT, exp))
    | Cabs.UNARY (Cabs.NOT, exp) ->
       
       let exp' = toBExp packs loc exp in
       let c_exp = complement exp' in
       c_exp
    | Cabs.BINARY (Cabs.AND, exp1, exp2) -> OP (toBExp packs loc exp1, Op.AND, toBExp packs loc exp2)
    | Cabs.BINARY (Cabs.OR, exp1, exp2) -> OP (toBExp packs loc exp1, Op.OR, toBExp packs loc exp2)
    | Cabs.BINARY (Cabs.XOR, exp1, exp2) ->
      toBExp packs loc (Cabs.BINARY (Cabs.OR, Cabs.BINARY (Cabs.AND, Cabs.UNARY (Cabs.NOT, exp1), exp2), Cabs.BINARY (Cabs.AND, exp1, Cabs.UNARY (Cabs.NOT, exp2))))
    | Cabs.BINARY (Cabs.EQ, exp1, exp2) ->
       UNIT (Term.toTerm packs exp1, Op.EQ, Term.toTerm packs exp2)
    | Cabs.BINARY (Cabs.ASSIGN, exp1, exp2) ->
       UNIT (Term.toTerm packs exp1, Op.EQ, Term.toTerm packs exp2)
    | Cabs.BINARY (Cabs.NE, exp1, exp2) ->
       UNIT (Term.toTerm packs exp1, Op.NE, Term.toTerm packs exp2)
    | Cabs.BINARY (Cabs.LT, exp1, exp2) ->
       let res2 = Term.toTerm packs exp2 in
       let res = UNIT (Term.toTerm packs exp1, Op.LE, res2) in
       res
    | Cabs.BINARY (Cabs.GT, exp1, exp2) -> UNIT (Term.toTerm packs exp2, Op.LE, Term.toTerm packs exp1)
    | Cabs.BINARY (Cabs.LE, exp1, exp2) ->
      OP (UNIT (Term.toTerm packs exp1, Op.EQ, Term.toTerm packs exp2), Op.OR, UNIT (Term.toTerm packs exp1, Op.LE, Term.toTerm packs exp2))
    | Cabs.BINARY (Cabs.GE, exp1, exp2) ->
      OP (UNIT (Term.toTerm packs exp1, Op.EQ, Term.toTerm packs exp2), Op.OR, UNIT (Term.toTerm packs exp2, Op.LE, Term.toTerm packs exp1))
    | Cabs.BINARY (op, exp1, exp2) -> toBExp packs loc (Cabs.BINARY (Cabs.NE, Cabs.BINARY (op, exp1, exp2), Cabs.CONSTANT (Cabs.CONST_INT "0")))
    | Cabs.PAREN (x) -> toBExp packs loc x
    | Cabs.CALL (Cabs.VARIABLE s, exp) when String.length s > 0 && String.sub s 0 1 = "@" ->
       let b = try toBExp packs loc (List.hd exp)  with _ ->
                 dbg "EXCEP" "@L(...) Exception:" Cprint.print_expression (Cabs.CALL (Cabs.VARIABLE s, exp));
                 raise (StError "toBExp CALL" ) in
       LBL (s, b)
    | Cabs.COMMA xs when List.length xs = 1 ->
         toBExp packs loc (List.hd xs)
    | be ->
       begin
         match be with
         | Cabs.COMMA xs ->
            pn (Locs.to_str loc);
            pi (List.length xs);
            raise (StError "Unexpected Boolean Expression 2")
       (* try
         let t = Term.toTerm be in
         UNIT (t, Op.NE, Term.zero)
       with
         _ -> *)
         | _ ->
         Cprint.print_expression be; pn ""; raise (StError "Unexpected Boolean Expression ELSE")
       end
 *)
         
  let concat_result r1 r2 =
    match r1, r2 with
      None, _ -> r2
    | _, None -> r1
    | Some rr1, Some rr2 -> Some (rr1@rr2)
  ;;

  (*
  let rec substitute_stmt to_be by stmt =
    match stmt with
      Cabs.COMPUTATION (Cabs.BINARY (Cabs.ASSIGN, Cabs.VARIABLE nv, exp1'), cabsloc) when nv = to_be ->
       let stmt' = Cabs.COMPUTATION (Cabs.BINARY (Cabs.ASSIGN, Cabs.VARIABLE by, exp1'), cabsloc) in
       (* pn "------------==---------";
       pn to_be;
       pn by;
       Cprint.print_statement stmt;
       pn "==>"; 
       Cprint.print_statement stmt'; *)
       stmt'
    | Cabs.IF (bexp, stmt1, stmt2, cabsloc) ->
       Cabs.IF (bexp, substitute_stmt to_be by stmt1, substitute_stmt to_be by stmt2, cabsloc)
    | Cabs.BLOCK (block, cabsloc) ->
        Cabs.BLOCK ({Cabs.blabels=block.Cabs.blabels; battrs=block.Cabs.battrs; bstmts = substitute_stmt to_be by |>>| block.Cabs.bstmts}, cabsloc)
    | _ ->
       (* pn "------------==---------";
       pn to_be;
       pn by;
       Cprint.print_statement stmt; *)
       
       stmt
  ;;
   *)
  (*
  let build_cond s_nv bexp (r1, exp1) (r2, exp2) cabsloc =
    let module C = Cabs in
    let stmt1 =
      match r1, exp1 with
        Some (C.DEFINITION (C.DECDEF ((_, ((snv, _, _, _),_)::[]), _)):: stmts), C.VARIABLE sv when sv=snv ->
         let stmts' = substitute_stmt sv s_nv |>>| stmts in
         C.BLOCK ({C.blabels=[]; battrs=[]; bstmts = stmts'}, cabsloc)
      | _ ->
         C.COMPUTATION (C.BINARY (C.ASSIGN, C.VARIABLE s_nv, exp1), cabsloc)
    in
    let stmt2 =
      match r2, exp2 with
        Some (C.DEFINITION (C.DECDEF ((_, ((snv, _, _, _),_)::[]), _)):: stmts), C.VARIABLE sv when sv=snv ->
         (* pn "******"; pi (List.length stmts); *)
         let stmts' = substitute_stmt sv s_nv |>>| stmts in
         C.BLOCK ({C.blabels=[]; battrs=[]; bstmts = stmts'}, cabsloc)
      | _ ->
         C.COMPUTATION (C.BINARY (C.ASSIGN, C.VARIABLE s_nv, exp2), cabsloc)
    in
    let decl = C.DEFINITION (C.DECDEF (([C.SpecType C.Tvoid], [((s_nv, C.JUSTBASE, [], cabsloc), C.NO_INIT)]), cabsloc)) in
    let if_stmt = C.IF (bexp, stmt1, stmt2, cabsloc) in
    let r3 = Some [decl;if_stmt] in
    r3, C.VARIABLE s_nv
  ;;

  let rec extract_if_from_exp (pc : Cabs.expression list) (cabsloc : Cabs.cabsloc) (expr : Cabs.expression) =
    let module C = Cabs in
    let build_new_if exp r =
      let s_nv = new_prog_var () in
      let nv = Cabs.VARIABLE (s_nv) in
      let prs = 
          [C.DEFINITION (C.DECDEF (([C.SpecType C.Tint], [((s_nv, C.JUSTBASE, [], cabsloc), C.NO_INIT)]), cabsloc));
                  C.IF (exp,
                        C.COMPUTATION (C.BINARY (C.ASSIGN, nv, C.CONSTANT (C.CONST_INT "1")), cabsloc),
                        C.COMPUTATION (C.BINARY (C.ASSIGN, nv, C.CONSTANT (C.CONST_INT "0")), cabsloc),
                        cabsloc
                 )]
      in
      let r0 = Some prs in
      (* Cprint.print_expression exp; pn "   ORG"; *)
      let r' = concat_result r r0 in
      (*      Cprint.print_expression nv; pn "   NEW"; *)
      (r', nv)
    in
    (*
    let build_new_assign exp r =
      let s_nv = new_prog_var () in
      let nv = Cabs.VARIABLE (s_nv) in
      let prs =
        [C.DEFINITION (C.DECDEF (([C.SpecType C.Tint], [((s_nv, C.JUSTBASE, [], cabsloc), C.NO_INIT)]), cabsloc));
         C.COMPUTATION (C.BINARY (C.ASSIGN, nv, exp), cabsloc)]
      in
      let r0 = Some prs in
      (* Cprint.print_expression exp; pn "   ORG"; *)
      let r' = concat_result r r0 in
      (*      Cprint.print_expression nv; pn "   NEW"; *)
      (r', nv)
    in

    let one = C.CONSTANT (C.CONST_INT "1") in
     *)

    let rec get_if pc expr =
      (** if they are term, it is fine, otherwise make new if *)
      (* Cprint.print_expression expr; pn " TO_GET_IF_FROM"; *)
      match expr with
      | Cabs.VARIABLE (str) -> (None, expr)
      | Cabs.CONSTANT _ -> (None, expr)
      | Cabs.UNARY (Cabs.NOT, exp) ->
         let (r, exp') = get_if pc exp in
         build_new_if (Cabs.UNARY (Cabs.NOT, exp')) r
      (* | C.UNARY (op, exp) when op=C.PREINCR || op=C.PREDECR || op=POSINCR || op=POSDECR ->
         begin
           let s_nv = new_prog_var () in
           let nv = Cabs.VARIABLE (s_nv) in
           let dcl =
             C.DEFINITION (C.DECDEF (([C.SpecType C.Tint], [((s_nv, C.JUSTBASE, [], cabsloc), C.NO_INIT)]), cabsloc)) in    
           let (r, exp') = get_if exp in
           let r0 =
             match op with
               C.PREINCR -> (** ++x  --> x = x+1; v = x *)
               let pr = C.COMPUTATION (C.BINARY (C.ADD, exp', one)) in
               ()
           in
           let r' = concat_result r (Some r0) in
           (r', nv)
         end *)
      | Cabs.UNARY (op, exp) ->
         let (r, exp') = get_if pc exp in
         let exp0 = Cabs.UNARY (op, exp') in

         (* Cprint.print_expression exp0; pn "  :get_if(UNARY)"; *)
         
         (r, exp0)
      (*| Cabs.BINARY (op, exp1, exp2) when op |<- [Cabs.NE;Cabs.EQ] ->
         let (r1, exp1') = get_if exp1 in
         let (r2, exp2') = get_if exp2 in
         if r1=None && r2=None then
           (None, expr)
         else
           build_new_if (Cabs.BINARY (op, exp1', exp2')) (concat_result r1 r2) *)
      | Cabs.EXPR_SIZEOF (exp) ->
         
         let (r, exp') = get_if pc exp in
         let exp0 = Cabs.EXPR_SIZEOF (exp') in

         (* Cprint.print_expression exp0; pn "  :get_if(UNARY)"; *)
         
         (r, exp0)
      | Cabs.BINARY (op, exp1, exp2)
           when op |<- [Cabs.AND;Cabs.OR] ->
         begin
           
           let (r1, exp1') = extract_if_from_bexp pc cabsloc exp1 in
           let (r2, exp2') = extract_if_from_bexp pc cabsloc exp2 in
           let r = concat_result r1 r2 in
           build_new_if (Cabs.BINARY (op, exp1', exp2')) r
         end
      | Cabs.BINARY (op, exp1, exp2)
           when op |<- [Cabs.NE;Cabs.EQ;Cabs.LT;Cabs.GT;Cabs.LE;Cabs.GE] ->
         begin
           let (r1, exp1') = get_if pc exp1 in
           let (r2, exp2') = get_if pc exp2 in
           let r = concat_result r1 r2 in
           build_new_if (Cabs.BINARY (op, exp1', exp2')) r
       end
    | Cabs.BINARY (op, exp1, exp2) ->
       begin
         let (r1, exp1') = get_if pc exp1 in
         let (r2, exp2') = get_if pc exp2 in
         let r = concat_result r1 r2 in
         (r, Cabs.BINARY (op, exp1', exp2'))
       end
    | Cabs.PAREN (x) ->
       let (r, x') = get_if pc x in
       (r, Cabs.PAREN (x'))
    | Cabs.CALL (Cabs.VARIABLE s, exps) when String.length s > 0 && String.sub s 0 1 = "@" ->
       (* Cprint.print_expression expr; pn ""; *)
       let (r, exps') =
         (fun (acc,exps) exp ->
           let (r, exp') = extract_if_from_bexp pc cabsloc exp in
           let r' = concat_result acc r in
           (r', exp'::exps)
         ) |->> ((None, []), exps) in
       build_new_if (Cabs.CALL (Cabs.VARIABLE s, List.rev exps')) r
    | Cabs.CALL (exp, exps) ->
       let (r0, exp') = get_if pc exp in
       let (r, exps') =
         (fun (acc,exps) exp ->
           let (r, exp') = get_if pc exp in
           let r' = concat_result acc r in
           (r', exp'::exps)
         ) |->> ((r0, []), exps) in
       (r, Cabs.CALL (exp', List.rev exps'))
    | Cabs.MEMBEROF (exp, str) ->
       let (r, exp') = get_if pc exp in
       (r, Cabs.MEMBEROF (exp', str))
    | Cabs.MEMBEROFPTR (exp, str) ->
       let (r, exp') = get_if pc exp in
       (r, Cabs.MEMBEROFPTR (exp', str))
    | C.CAST (([C.SpecType C.Tsizet], C.JUSTBASE), C.SINGLE_INIT (C.UNARY (C.ADDROF, e))) ->
       (None, expr)
    | Cabs.CAST (_, Cabs.SINGLE_INIT ((Cabs.CONSTANT _) as e)) ->
       (None, e)
    | Cabs.CAST (a, b) ->
       begin
         let rec aux = function
             Cabs.NO_INIT ->
              (None, Cabs.NO_INIT)
           | Cabs.SINGLE_INIT e ->
              let (r, e') = get_if pc e in
              (r, Cabs.SINGLE_INIT e')
           | Cabs.COMPOUND_INIT l_iw_ie ->
              let (r, l_iw_ie') = (fun (r, l_iw_ie') (iw, ie) ->
                  let (r', ie') = aux ie in
                  (concat_result r r', (iw, ie')::l_iw_ie')
                ) |->> ((None,[]), l_iw_ie)
              in
              
              (r, (Cabs.COMPOUND_INIT (List.rev l_iw_ie')))
         in
         let (r, b') = aux b in
         (r, Cabs.CAST (a, b'))
       end
    | Cabs.QUESTION (bexp, exp1, exp2) ->
              
       let (r0, bexp') = extract_if_from_bexp pc cabsloc bexp in
       let bexp'1 =
         (* if List.length pc = 0 then *)
           bexp'
         (* else
           Cabs.BINARY (Cabs.AND, bexp', List.hd pc) *)
       in
       
       (* let bexp'2 = 
         if List.length pc = 0 then
           Cabs.UNARY (Cabs.NOT, bexp')
         else
           Cabs.BINARY (Cabs.AND, Cabs.UNARY (Cabs.NOT, bexp'), List.hd pc)
       in *)
       
       let (r1, exp1') = get_if [] exp1 in
       let (r2, exp2') = get_if [] exp2 in

       (* let r' = concat_result (concat_result r0 r1) r2 in *)
       let s_nv = new_prog_var () in
       (* let nv = Cabs.VARIABLE s_nv in
       
       let decl = C.DEFINITION (C.DECDEF (([C.SpecType C.Tvoid], [((s_nv, C.JUSTBASE, [], cabsloc), C.NO_INIT)]), cabsloc)) in
       let if_stmt = Cabs.IF (bexp'1, Cabs.COMPUTATION (Cabs.BINARY (Cabs.ASSIGN, nv, exp1'), cabsloc), Cabs.COMPUTATION (Cabs.BINARY (Cabs.ASSIGN, nv, exp2'), cabsloc), cabsloc) in
       let r3 = Some [decl;if_stmt] in *)

       let r, nv = build_cond s_nv bexp'1 (r1, exp1') (r2, exp2') cabsloc in
       (* let r = concat_result r' r3 in *)

       (* pn "Condition encountered";
       Cprint.print_expression (Cabs.QUESTION (bexp, exp1, exp2)); pn "";
       Cprint.print_expression (nv); pn "";
       begin
         match r with
           Some sl -> iterS Cprint.print_statement "" sl
         | _ -> ()
       end;
       pn "---------"; *)
       (r, nv)
    | Cabs.INDEX (exp1, exp2) ->
       let (r1, exp1') = get_if pc exp1 in
       let (r2, exp2') = get_if pc exp2 in
       (concat_result r1 r2, Cabs.INDEX (exp1', exp2'))
    | Cabs.GNU_BODY _
      | Cabs.EXPR_PATTERN _
      (* | Cabs.EXPR_SIZEOF _ *)
      | Cabs.TYPE_SIZEOF _
      | Cabs.EXPR_ALIGNOF _
      | Cabs.TYPE_ALIGNOF _    
      | Cabs.NOTHING
      | Cabs.LABELADDR _ ->
        (None, expr)
    | Cabs.COMMA exps ->
       let r, exps' =
       (fun (r, all) exp ->
         let (r', exp') = get_if pc exp in
         concat_result r r', exp'::all
        ) |->> ((None, []), exps)
       in
       (r, Cabs.COMMA (List.rev exps'))

       (*
    and condtional_to_statement pc = function
      | Cabs.QUESTION (bexp, exp1, exp2) ->
         
      | e ->
         let (r, v) = get_if pc e in
         ([], [], []) *)
    in
    get_if pc expr

    
  and extract_if_from_bexp (pc : Cabs.expression list) (cabsloc : Cabs.cabsloc) (expr : Cabs.expression) =

    (* Cprint.print_expression expr; pn "   :@LLL"; *)
    
    let not_0 e =
      Cabs.BINARY (Cabs.NE, e, Cabs.CONSTANT (Cabs.CONST_INT "0"))
    in

    let rec bexp_to_if expr =
      match expr with
      | Cabs.NOTHING -> (None, expr)
      | Cabs.VARIABLE _ -> (None, Cabs.BINARY(Cabs.NE, expr, Cabs.CONSTANT (Cabs.CONST_INT "0")))
      | Cabs.CONSTANT _ -> (None, Cabs.BINARY(Cabs.NE, expr, Cabs.CONSTANT (Cabs.CONST_INT "0")))
      | Cabs.UNARY (Cabs.NOT, exp) ->
         (** exp is itself a logical term. It is alright. *)
         let (r, exp') = bexp_to_if exp in
         (r, Cabs.UNARY (Cabs.NOT, exp'))
      | Cabs.BINARY (op, exp1, exp2)
           when op |<- [Cabs.ASSIGN;Cabs.EQ;Cabs.NE;Cabs.LT;Cabs.GT;Cabs.LE;Cabs.GE] ->
         (** exp1 and exp2 are supposed to be non boolean expression. So we call get_if *)
         begin
           (* if op = Cabs.NE then
             (Cprint.print_expression expr; pn "   NOT(ORG)"); *)
           let (r1, exp1') = extract_if_from_exp pc cabsloc exp1 in
           let (r2, exp2') = extract_if_from_exp pc cabsloc exp2 in
           let r = concat_result r1 r2 in
           let exp0 = Cabs.BINARY (op, exp1', exp2') in
           (* if op = Cabs.NE then
             (Cprint.print_expression exp0; pn "  NOT(RES)"); *)
           
           (* Cprint.print_expression exp1; pn "  BIN";
           Cprint.print_expression exp2; pn "  BIN";
           Cprint.print_expression exp0; pn "  RES"; *)
           (r, exp0)
         end
      | Cabs.BINARY (op, exp1, exp2)
           when op |<- [Cabs.AND;Cabs.OR;Cabs.XOR] ->
         begin
           let (r1, exp1') = bexp_to_if exp1 in
           let (r2, exp2') = bexp_to_if exp2 in
           let r = concat_result r1 r2 in
           (* Cprint.print_expression expr; pn "  @binary";
           Cprint.print_expression (Cabs.BINARY (op, exp1', exp2')); pn ""; *)
           
           (r, Cabs.BINARY (op, exp1', exp2'))
         end
      | Cabs.BINARY (op, exp1, exp2) ->
         (None, not_0 expr)
      | Cabs.PAREN (x) ->
         (* Cprint.print_expression expr; pn "   :@paren"; *)
         let (r, exp') = bexp_to_if x in
         r, Cabs.PAREN (exp')
      | Cabs.CALL (Cabs.VARIABLE s, exps) when String.length s > 0 && String.sub s 0 1 = "@" ->
           
         (* Cprint.print_expression expr; pn "   :@L1";
         Cprint.print_expression (List.hd exps); pn "   :@L1"; *)
         let (r, exp') = bexp_to_if (List.hd exps) in
         (* Cprint.print_expression exp'; pn "    :2nd"; *)
         (* if s = "@L0" then
           raise Error; *)
         (* (r, Cabs.BINARY(Cabs.NE, Cabs.CALL (Cabs.VARIABLE s, [exp']), Cabs.CONSTANT (Cabs.CONST_INT "0"))) *)
         (* Cprint.print_expression expr; pn "   :@L2";
         Cprint.print_expression (Cabs.CALL (Cabs.VARIABLE s, [exp'])); pn ""; *)
         let res = Cabs.CALL (Cabs.VARIABLE s, [exp']) in
         (* Cprint.print_expression res; pn "    :3rd"; *)
                              
         (r, res)
      | Cabs.CALL (Cabs.VARIABLE s, exps) when String.length s = 0->
         if List.length exps > 0 then
           bexp_to_if (List.hd exps)
         else
           raise (StError "empty function name with no argument")
      | Cabs.COMMA exps ->
         begin
           match List.rev exps with
             last::rev_init ->
              (* Cprint.print_expression last; pn " ::COMAM"; *)
              let r =
                (fun r i ->
                  (* Cprint.print_expression i; pn " ###"; *)
                  (* match i with
                    Cabs.BINARY (op, lhs, rhs) ->
                     let (r', rhs') = extract_if_from_exp cabsloc rhs in
                     let r0 = concat_result r' (Some [Cabs.COMPUTATION (Cabs.BINARY (op, lhs, rhs'), cabsloc)]) in
                     concat_result r r0
                     
                  | _ -> *)
                     let (r', i') = extract_if_from_exp pc cabsloc i in
                     let r0 = concat_result r' (Some [Cabs.COMPUTATION (i', cabsloc)]) in
                     (concat_result r r0)
                ) |->> (None, rev_init) in
              let (r', last') = bexp_to_if last in

              let r0 = concat_result r r' in
              (* begin
                match r0 with
                Some rr -> iterS Cprint.print_statement " " rr;
              | _ -> ()
              end; *)
              let res = last' in
              (r0, res)
           | _->
              raise (StError "Comma has 0 sequence");
              (* (None, expr) *)
         end
      | _ ->
         (* Cprint.print_expression expr; pn "  ELSE(1)"; *)
         let (r, expr') = extract_if_from_exp pc cabsloc expr  in
         let expr0 = not_0 expr' in
         (* Cprint.print_expression expr0; pn "  ELSE(2)"; *)
         (r, expr0)
    in
    let (r, bexp') = bexp_to_if expr in
    (* Cprint.print_expression bexp'; pn " :@at last"; *)
    (r, bexp')
   *)
  
  let rec substitute (to_be_replaced : Term.t) (replaced_by : Term.t) = function
    | NOTHING -> NOTHING
    | UNIT (_ta, Op.MAPSTO, _tb) ->
        UNIT (
          Term.substitute to_be_replaced replaced_by _ta,
          Op.MAPSTO,
          _tb)
    | UNIT (_ta, op, _tb) ->
        UNIT (
          Term.substitute to_be_replaced replaced_by _ta,
          op,
          Term.substitute to_be_replaced replaced_by _tb)
    | OP (_ta, op, _tb) ->
        OP (
          substitute to_be_replaced replaced_by _ta,
          op,
          substitute to_be_replaced replaced_by _tb)
    | BLOCK (str, exp) ->
      let exp' = Term.substitute to_be_replaced replaced_by exp in
      BLOCK (str, exp')
    | LBL (lbl, b) ->
       let b' = substitute to_be_replaced replaced_by b in
       LBL (lbl, b')

  let rec par_subs subs_pairs = function
    | NOTHING -> NOTHING
    | UNIT (_ta, Op.MAPSTO, _tb) ->
        UNIT (
          Term.par_subs subs_pairs _ta,
          Op.MAPSTO,
          Term.par_subs subs_pairs _tb)
    | UNIT (_ta, op, _tb) ->
       (*Term.pprint _ta; p "...."; Term.pprint _tb; pn "";
       pb (_ta = _tb); pn "";
       iterS (fun (a,b) -> Term.pprint a; p "->"; Term.pprint b) "," subs_pairs; *)
        UNIT (
          Term.par_subs subs_pairs _ta,
          op,
          Term.par_subs subs_pairs _tb)
    | OP (_ta, op, _tb) ->
        OP (
          par_subs subs_pairs _ta,
          op,
          par_subs subs_pairs _tb)
    | BLOCK (str, exp) ->
      let exp' = Term.par_subs subs_pairs exp in
      BLOCK (str, exp')
    | LBL (lbl, b) ->
       let b' = par_subs subs_pairs b in
       LBL (lbl, b')

  let rec substitutes eqformulas = function
    | [] -> eqformulas
    | x::xs ->
      let eqformulas' =
      match x with
      | UNIT (Term.EXP (Exp.CONST y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.CONST y))) |>>| eqformulas
      | UNIT (Term.EXP (Exp.FLOAT y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.FLOAT y))) |>>| eqformulas
      | UNIT (Term.EXP (Exp.VAR y) as y', Op.EQ, z) -> (substitute y' z) |>>| eqformulas
      | UNIT (y, Op.MAPSTO, z) -> (substitute z y) |>>| eqformulas
      | _ -> eqformulas
      in
      substitutes eqformulas' xs

  let rec rev = function
    | UNIT (a, op, b) -> UNIT (b, op, a)
    | OP (a, op, b) -> OP (rev b, op, rev a)
    | LBL (lbl, b) -> LBL (lbl, rev b)
    | x -> x

  let rec fv = function
    | UNIT (a, _, b) when a = b -> Term.fv a
    | UNIT (a, _, b) -> (Term.fv a) @@@ (Term.fv b)
    | OP (a, _, b) -> (fv a) @@@ (fv b)
    | BLOCK (a, b) -> (Exp.fv a) @@@ (Term.fv b)
    | LBL (lbl, b) -> fv b
    | NOTHING -> []

  let rec is_consistent = function
    | UNIT (a, Op.EQ, b) -> Term.eq a b
    | UNIT (a, Op.NE, b) -> not (Term.eq a b)
    | OP (a, Op.AND, b) -> (is_consistent a) && (is_consistent b)
    | OP (a, Op.OR, b) -> (is_consistent a) || (is_consistent b)
    | LBL (lbl, b) -> is_consistent b
    | x -> true

  let rec is_contradict = function
    | UNIT (a, Op.EQ, b) -> false
    | UNIT (a, Op.NE, b) -> Term.eq a b
    | OP (a, Op.AND, b) -> (is_contradict a) || (is_contradict b)
    | OP (a, Op.OR, b) -> (is_contradict a) && (is_contradict b)
    | LBL (lbl, b) -> is_contradict b
    | x -> false

  let rec non_trivial t_exqs (pre_locations, post_locations) pointers eq =
    match eq with
    | UNIT (a, Op.EQ, b) when a = b -> false
    | UNIT (a, Op.EQ, b) when (a |<- t_exqs) || (b |<- t_exqs) -> false
    | UNIT (a, Op.NE, b) when (a |<- t_exqs) || (b |<- t_exqs) -> false
    | UNIT (a, Op.NE, b) when
      ((a |<- pre_locations) && (b |<- pre_locations)) && a<>b -> false
    | UNIT (a, Op.NE, Term.NULL) | UNIT (Term.NULL, Op.NE, a) ->
      if a |<- pre_locations then
        false
      else
        true
    | LBL (_, b) -> non_trivial t_exqs (pre_locations, post_locations) pointers b
    | _ -> true

  let rec compare x y = match x, y with
      UNIT (a, Op.EQ, b), UNIT (c,Op.EQ, d) -> if (a = c && b = d) || (a = d && b = c) then 0 else -1
    | UNIT (a, Op.NE, b), UNIT (c,Op.NE, d) -> if (a = c && b = d) || (a = d && b = c) then 0 else -1
    | LBL (a, b), LBL (c, d) -> if a = c then compare b d else String.compare a c
    | _ -> 0

  let get_val b =
    match b with
    | UNIT (Term.EXP Exp.NEGINF, Op.LE, _)
      | UNIT (_, Op.LE, Term.EXP Exp.POSINF) ->  Some true
    | UNIT (x, Op.EQ, y) when x = y ->
      dbg "SATT" "1.TRUE: " pprint b;
      Some true
    | UNIT (x, Op.NE, y) when x = y ->
      dbg "SATT" "2.FALSE: " pprint b;
      Some false
    | UNIT (Term.EXP x, Op.EQ, Term.EXP y) when Exp.is_hat x && Exp.is_hat y ->
       dbg "SATT" "3.FALSE: " pprint b;
       Some false
    | UNIT (Term.EXP x, Op.NE, Term.EXP y) when Exp.is_hat x && Exp.is_hat y ->
      dbg "SATT" "4.TRUE: " pprint b;
       Some true

(*    | UNIT (_, Op.EQ, Term.EXP (Exp.VAR x)) when Exp.Var.is_hat x ->
       dbg "SATT" "5.FALSE: " pprint b;
       Some false
    | UNIT (_, Op.NE, Term.EXP (Exp.VAR x)) when Exp.Var.is_hat x ->
       dbg "SATT" "6.TRUE: " pprint b;
       Some true *)
    (* | UNIT (Term.EXP (Exp.BINOP (Exp.VAR x, Op.ADD, _)), Op.EQ, _) when Exp.Var.is_hat x ->
       dbg "SATT" "7.FALSE: " pprint b;
       Some false
    | UNIT (Term.EXP (Exp.BINOP (Exp.VAR x, Op.ADD, _)), Op.NE, _) when Exp.Var.is_hat x ->
       dbg "SATT" "8.TRUE: " pprint b;
       Some true
    | UNIT (_, Op.EQ, Term.EXP (Exp.BINOP (Exp.VAR x, Op.ADD, _))) when Exp.Var.is_hat x ->
       dbg "SATT" "9.FALSE: " pprint b;
       Some false
    | UNIT (_, Op.NE, Term.EXP (Exp.BINOP (Exp.VAR x, Op.ADD, _))) when Exp.Var.is_hat x ->
       dbg "SATT" "10.TRUE: " pprint b;
       Some true *)
    | _ ->
       None

  let get_sym = function
      true ->
      _T
    | false ->
       UNIT (Term.EXP (Exp.CONST 0), Op.NE, Term.EXP (Exp.CONST 0))

  let rec eval = function
    | UNIT (x, o, y) ->
      begin
      let x' = Term.eval x in
      let y' = Term.eval y in
        match x', o, y' with
          Term.EXP (Exp.CONST x1), Op.EQ, Term.EXP (Exp.CONST y1) -> get_sym (x1 = y1)
        | Term.EXP (Exp.CONST x1), Op.NE, Term.EXP (Exp.CONST y1) -> get_sym (not (x1 = y1))
        | Term.EXP (Exp.CONST x1), Op.LE, Term.EXP (Exp.CONST y1) -> get_sym (x1 < y1)
        | xx, Op.EQ, yy when xx = yy -> get_sym true
        | _ ->
           let r = UNIT (x', o, y') in
           match get_val r with
             Some x -> get_sym x
           | None -> r
      end
    | OP (x, o, y) ->
       begin
         let x' = eval x in
         let y' = eval y in
       (*
         match x', o, y' with
         | UNIT (Term.EXP (Exp.CONST 0), Op.EQ, Term.EXP (Exp.CONST 0)), Op.AND, x -> x
         | x, Op.AND, UNIT (Term.EXP (Exp.CONST 0), Op.EQ, Term.EXP (Exp.CONST 0)) -> x
         | UNIT (Term.EXP (Exp.CONST 0), Op.NE, Term.EXP (Exp.CONST 0)), Op.AND, _ -> bexp_false
         | _, Op.AND, UNIT (Term.EXP (Exp.CONST 0), Op.NE, Term.EXP (Exp.CONST 0)) -> bexp_false
         | ...
         | _ -> OP (x', o, y') *)
         match get_val x', o, get_val y' with
         | Some x1, Op.AND, Some y1 -> get_sym (x1 && y1)
         | Some true, Op.OR,  _  -> _T
         | _, Op.OR,  Some true  -> _T
         | Some x1, Op.OR,  Some y1 -> get_sym (x1 || y1)
         | Some true, Op.AND, None  -> y'
         | Some false, Op.OR, None  -> y'
         | None, Op.AND, Some true  -> x'
         | None, Op.OR,  Some false -> x'
         | Some false, Op.AND, None -> _F
         | None, Op.AND, Some false -> _F
         | _ ->
            OP (x', o, y')
       end
    | LBL (lbl, b) -> LBL (lbl, eval b)
    | x -> x


  let rec bexp_to_list = function
    | OP (a, Op.AND, b) -> bexp_to_list a @ bexp_to_list b
    | b -> [b]

  let list_to_bexp bs =
    if bs = [] then _T else
      (fun acc b -> OP (acc, Op.AND, b)) |->> (List.hd bs, List.tl bs)



    (*
  let uniq xs =
  let rec uniq' ys = function
      [] -> ys
    | x::xs -> if List.exists (fun y ->
                      let b = x = y in
                      pprint x; p " == "; pprint y; p " ";
                      pb b; pn "";
                      b) ys then uniq' ys xs else uniq' (x::ys) xs
  in
  pn "";
  List.rev (uniq' [] xs)
     *)
    
  let evals bs =
    
    
    if !Options.is_no_evals then
      bs
    else
      let bs'' = List.concat (bexp_to_list |>>| bs) in
      let bs''' = eval |>>| bs'' in
      if List.exists ((=) _F) bs''' then
        [_F]
      else
        begin
          let bs' = uniq bs''' in
          (fun b -> not (b = _T)) |>- bs'
        end

(** [A1;A2;A3] U [A1;A4;A3] = [A1;A3;(A2|A4)]
    [A1;A3;(A2|A4)] U [A1;A2;A5] = [A1;]
 *)
(*
  let disjunc xs ys =
    let common, uncommon = (
        fun 
      ) |->> ((ys, [], []), xs) in
 *)

  let rec approx = function
    | UNIT (x, op, y) ->
       let x' = Term.approx x in
       let y' = Term.approx y in
       if x' = Term.EXP Exp.UNDEF || y' = Term.EXP Exp.UNDEF then
         _T
       else
         UNIT  (x', op, y')
    | OP    (x, op, y) -> eval (OP (approx x, op, approx y))
    | BLOCK (var,  e)  -> _T
    | LBL (lbl, b) -> LBL (lbl, approx b)
    | NOTHING -> NOTHING

(*
  let rec dis_norm (bs : t list) =
    let rec aux = function
        UNIT (x, o, y) -> UNIT (x, o, y)
      | OP (z, Op.AND, OP (x, Op.OR, y)) -> OP (aux (OP (z, Op.AND, x)), Op.OR, aux (OP (z, Op.AND, y)))
      | OP (x, o, y) -> OP (aux x, o, aux y)
      | LBL (lbl, b) -> LBL (lbl, aux b)
      | x -> x
    in

    match  bs with
      [] -> []
    | [x] -> x
    | x::xs ->
       begin
         let xs' = dis_norm xs in
         match x with
           OP (x, Op.OR, y) ->
           OP (aux (OP (x, Op.AND, xs')), Op.OR, aux ( OP (y, Op.AND, xs')))
         | y ->
            OP (y, Op.AND, xs')
       end
 *)
  let rec dis_norm (bs : t list) =
    let rec aux acc bs : t list list =
      match bs with
        [] -> acc
      | b::bs' ->
         begin
           match b with
             OP (x, Op.OR, y) ->
             let x' = aux acc (x::bs') in
             let y' = aux acc (y::bs') in
             x' @ y'
           | OP (x, Op.AND, y) ->
              aux acc (x::y::bs')
           | _ ->
              let acc' = (fun a -> (a@[b])) |>>| acc in
              aux acc' bs'
         end
    in
    aux [[]] bs


  let rec ll_to_bexp ll =
    match ll with
      [] ->
      _T
    | l::ls ->
       let lb = list_to_bexp in
       (fun b l -> OP (b, Op.OR, lb l)) |->> (lb l, ls)
    
(*    
 *)

  let normalize (bs : t list) =
    (fun (a:t list) (b:t) ->
    match get_val (eval b) with
      Some true -> a
    | Some false -> [get_sym false]
    | None -> a @ [b]
    ) |->> ([], bs)

  let rec get_interval intv ys = function
    UNIT (Term.EXP Exp.VAR x, o, t) when (not (x |<- ys)) ->
    begin
      match o with
        Op.EQ ->
        let (l,u) = Term.get_interval intv ys t in
        (x, (l,u))
      | Op.LE ->
        let (l,u) = Exp.get_interval intv ys (Exp.VAR x) in
        let (_,u0) = Term.get_interval intv ys t in
        let min_u = Exp.min u (Exp.eval (Exp.BINOP (u0, Op.SUB, Exp.CONST 1))) in
        (x, (l, min_u))
      | _ -> (x, (Exp.NEGINF, Exp.POSINF))
    end
  | UNIT (t, o, Term.EXP Exp.VAR x) when (not (x |<- ys)) -> (** x=t | x/=t | x<t *)
    begin
      match o with
        Op.EQ ->
        let (l,u) = Term.get_interval intv ys t in
        (x, (l,u))
      | Op.LE ->
        let (l,u) = Exp.get_interval intv ys (Exp.VAR x) in
        let (l0,u0) = Term.get_interval intv ys t in
        let max_l = Exp.max l (Exp.eval (Exp.BINOP (l0, Op.ADD, Exp.CONST 1))) in
        (x, (max_l, u))
      | _ -> (x, (Exp.NEGINF, Exp.POSINF))
    end
  | OP (UNIT( x1, o1, t1), o, UNIT(x2, o2, t2)) when x1 = x2 ->
    let (_, (lx, ux)) = get_interval intv ys (UNIT(x1, o1, t1)) in
    let (_, (ly, uy)) = get_interval intv ys (UNIT(x2, o2, t2)) in
    if o = Op.OR then
      (Term.decode x1, (Exp.min lx ly, Exp.max  ux uy))
    else if o = Op.AND then
      (Term.decode x1, (Exp.max lx ly, Exp.min  ux uy))
    else
      (Term.decode x1, (Exp.NEGINF, Exp.POSINF))
  | _ -> raise (StError "Interval in bad condition")

  let rec get_intervals (intv: (Exp.var_t * (Exp.t * Exp.t)) list) ys bexp =
  match get_interval intv ys bexp with
    (_, (Exp.NEGINF, Exp.POSINF)) -> intv
  | _ ->
  let (x, (l,u)) = get_interval intv ys bexp in
  Exp.update_interval intv x (l,u)


   let lte a b = (** a <= b *)
     OP (UNIT(a, Op.LE, b), Op.OR, UNIT(a, Op.EQ, b))

   let gte a b = (** a >= b *)
     OP (UNIT(Term.EXP b, Op.LE, Term.EXP a), Op.OR, UNIT(Term.EXP a, Op.EQ, Term.EXP b))

   let lt a b = (** a < b *)
     UNIT(Term.EXP a, Op.LE, Term.EXP b)
     
   let gt a b = (** a > b *)
     UNIT(Term.EXP b, Op.LE, Term.EXP a)


end;;

module Pointer = struct
  type t = Term.t * (Field.t * Term.t) list

  let location (l, _) = l

                          (*
  let translate packs (loc, fields, elems) =
    (Term.EXP (Exp.var_encode Exp.PTR loc), List.map2 (fun f e -> (f, Term.toTerm packs e)) fields elems)
                           *)
                      
  let pp_print_fields ppf elements =
    Ftools.pp_list_with (fun ppf (a,b) -> Format.fprintf ppf "%s:%a" a Term.pp' b) ",@;" ppf elements

  let print_fields elements =
    iterS (fun (a,b) ->  p a; p ":"; Term.pprint b) ", " elements;
    ;;

  let pp ppf (loc, elements) =
    Format.fprintf ppf "@[<1>(%a,@,@[<1>[%a]@])@]" Term.pp' loc pp_print_fields elements

  (* let print = pp Format.std_formatter *)
  let print (location, elements) =
    pw "(";
    Term.print location;
    pw ",[";
    iterS (fun (a,b) -> pw a; pw ":"; Term.pprint b) ";" elements;
    pw "])";;

  let pp' ppf (loc, elements)=
    Format.fprintf ppf "%a@,->@,(%a)" Term.pp' loc pp_print_fields elements

  (* let pprint = pp_pprint Format.std_formatter *)
  let pprint (location, elements) =
    Term.pprint location;
    p "->(";
    print_fields elements;
    p ")";;

  let fstr () (loc, elements) =
    let fstr_fld () (lbl, vl) = Format.sprintf "%s:%a" lbl Term.fstr vl in
    Format.sprintf "%a->(%a)" Term.fstr loc (fstrL fstr_fld ",") elements

  let substitute (to_be_replaced : Term.t) (replaced_by : Term.t) ((location, elements) : t) : t =
    (
      Term.substitute to_be_replaced replaced_by location,
      (fun (a, b) -> (a, Term.substitute to_be_replaced replaced_by b)) |>>| elements
    )

  let rec substitutes ptr_list = function
    | [] -> ptr_list
    | x::xs ->
      let ptr_list' =
      match x with
      | BExp.UNIT (Term.EXP (Exp.CONST y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.CONST y))) |>>| ptr_list
      | BExp.UNIT (Term.EXP (Exp.FLOAT y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.FLOAT y))) |>>| ptr_list
      | BExp.UNIT (Term.EXP (Exp.VAR y), Op.EQ, z) -> (substitute (Term.EXP (Exp.VAR y)) z) |>>| ptr_list
      | BExp.UNIT (y, Op.MAPSTO, z) -> (substitute z y) |>>| ptr_list  (** Special consideration *)
      | _ -> ptr_list
      in
      substitutes ptr_list' xs

  let par_subs pair_subs (ptr, elements) =
    (Term.par_subs pair_subs ptr, (fun (fld, t) -> (fld, Term.par_subs pair_subs t)) |>>| elements)

  let map f (ptr, fields) =
    (Term.map f ptr, (fun (fld, v) -> (fld, Term.map f v)) |>>| fields)

  let is_consistent ((location, _) : t) = location <> Term.NULL

  let contained_in s_var (location, elements) =
    let t_var = Term.encode s_var in
    let velements = snd |>>| elements in
    t_var = location || (List.exists ((=) t_var) velements)

  (**
    Input : [x y z], (a, [b x c z]), (a, [b d c y])
    Output: true
  *)
                        (*
  let partial_match post_exqs pre_exqs ((location_b, elements_b):t) ((location_a, elements_a):t) : bool =
    let t_pre_exqs = Term.encode_str |>>| pre_exqs in
    let t_post_exqs = Term.encode_str |>>| post_exqs in
    let f_match = fun a b ->  if b |<- t_post_exqs then
                                (true, Some (b, a))
                              else if a |<- t_pre_exqs then
                                (true, None)  (** (isMatching, Some (Quantifier Term, Actual value)) *)
                              else
                                (a = b, None)
    in
    let rec is_match_consistent matches =
      match matches with
      | [] -> true
      | (x,y)::t -> (fun (z,w) -> z <> x || y = w) |>>| t >> l_and && (is_match_consistent t)
    in
    let location_match = f_match location_a location_b in
    let result = if List.length elements_a =  List.length elements_b then
        let elements_match = List.map2 f_match (snd |>>| elements_a) (snd |>>| elements_b) in
        if fst |>>| (location_match :: elements_match) >> l_and then
          let valid_matches = (snd |>>| (location_match :: elements_match)) >> valids_only in
          if is_match_consistent valid_matches then
            begin true (* valid_matches *) end
          else
            begin false (* [] *) end
        else
          begin false (* [] *) end
      else
        begin false (* [] *) end
    in
    result
    *)


  let get_value var (loc, elems) (m_loc, m_elems) : Term.t =
    let tvar = Term.encode var in
    let f a b c =
      begin
      match a with
      | Term.NULL -> if snd b = tvar then snd c else a
      | _ -> a
      end
    in
    if tvar = loc then
      m_loc
    else
      List.fold_left2 f Term.NULL elems m_elems

  (*
  let realize_value_from_ptr (exqs:string list) (b_exqs:string list) (var: Exp.var_t) (base_ptrs:t list) ((loc, elems):t) : Term.t list =
    let tvar = Term.encode var in
    if tvar = loc || (tvar |<- (snd |>>| elems)) then
      begin
      (* pprint (loc, elems); *)
      let matched_ptrs : t list = List.filter (partial_match exqs b_exqs (loc, elems)) base_ptrs in
      pw (string_of_int (List.length matched_ptrs)); pw ": ";
      let possible_values : Term.t list = get_value var (loc, elems) |>>| matched_ptrs in
      possible_values
      end
    else
      []


      (* a variable and its possible values *)
  let realize_value exqs b_exqs bases ptrs var : Exp.var_t * Term.t list =
    let x : Term.t list = realize_value_from_ptr exqs b_exqs var bases |>>| ptrs >> List.concat in
    let y : Term.t list = List.sort_uniq Term.compare x in
    (* if y <> [] then pw (Term.decode (List.hd y)); pw "\n"; *)
    (var, y)

*)

  let fvs ((pt, ds):t) : Exp.t list = (Term.fv pt) @ List.concat (Term.fv|>>| (snd |>>| ds))

  let root (pt, _) = [pt]

  let to_emp_field (ptr, fields) =
    (ptr, (fun (a, _) -> (a, Term.NULL)) |>>| fields)
end;;

module Predicate = struct
  (**
     In the current settings, Predicate is the either an Array or Any.
  *)
  type t = string * Term.t list [@@deriving show]
  (* Predicate.pp Format.std_formatter = Predicate.print *)

  let substitute (to_be_replaced : Term.t) (replaced_by : Term.t) ((name, elements) : t) : t =
    ( name,
      (Term.substitute to_be_replaced replaced_by) |>>| elements
    )

  let rec substitutes pred_list = function
    | [] -> pred_list
    | x::xs ->
      let pred_list' =
      match x with
      | BExp.UNIT (Term.EXP (Exp.CONST y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.CONST y))) |>>| pred_list
      | BExp.UNIT (Term.EXP (Exp.FLOAT y), Op.EQ, z) -> (substitute z (Term.EXP (Exp.FLOAT y))) |>>| pred_list
      | BExp.UNIT (Term.EXP (Exp.VAR y), Op.EQ, z) -> (substitute (Term.EXP (Exp.VAR y)) z) |>>| pred_list
      | BExp.UNIT (y, Op.MAPSTO, z) -> (substitute z y) |>>| pred_list  (** Special consideration *)
      | _ -> pred_list
      in
      substitutes pred_list' xs

  let par_subs pair_subs (name, elements) =
    (name, (Term.par_subs pair_subs) |>>| elements)

  let map f (pn, data) : t =
    (pn, (Term.map f) |>>| data)
(*
  let translate packs (name, params) =
    (name, Term.toTerm packs |>>| params)
 *)
    
  let fvs (_, t_list) = Term.toExp |>>| t_list

  let root (name, datas) =
    match name with
      "Array" -> [List.hd datas]
    | _   -> []

  let filter tag preds =
    (fun (nm, _) -> nm = tag) |>- preds
           
  (* let print = pp Format.std_formatter *)
  let print (name, arguments) =
    pw "(";
    ps name;
    pw ",[";
    iterS Term.print ";" arguments;
    pw "])";;
  
  let pp' ppf (name, arguments) =
    Format.fprintf ppf "@[<2>%s@;(%a)@]" name (Ftools.pp_list_with Term.pp' ",@;") arguments

  let fstr () (name, arguments) =
    let fstr_terms () = concatS "," (Term.fstr ()) in
    Format.sprintf "@[<2>%s@;(%a)@]" name fstr_terms arguments
    
  (* let pprint = pp' Format.std_formatter *)
  let pprint (name, arguments) =
    pw name;
    p "(";
    iterS Term.pprint ", " arguments;
    p ")";;

end;;


module Formula = struct
  type ut = string list * BExp.t list * Pointer.t list * Predicate.t list [@@deriving show]
  type t = ut list [@@deriving show]

  let uempty = ([], [], [], [])
  let empty = [uempty]

  let trueempty = []

  let (&~) (exs, pures, ptrs, preds) pure =
    (exs, BExp.(&&.) pures pure, ptrs, preds)

  let (&~~) (exs, pures, ptrs, preds) pures' : ut =
    (exs, BExp.(&&.) |->> (pures, pures'), ptrs, preds)

  let ( *~ ) (exs, pures, ptrs, preds) ptr =
         if fst ptr |<- (fst |>>| ptrs) then
           ([],[BExp._F],[],[])
         else
           (exs, pures, ptr::ptrs, preds);;

  let ( *~~ ) f ptrs = ( *~ ) |->> (f, ptrs)

  let ( #~ ) (exs, pures, ptrs, preds) pred =
         if snd pred |<- (snd |>>| preds) then
           ([],[BExp._F],[],[])
         else
           (exs, pures, ptrs, pred::preds);;

  let ( #~~ ) f preds : ut = ( #~ ) |->> (f, preds)

  let ( *** ) f1 (exs, pures, ptrs, preds) =
    ((f1 &~~ pures) *~~ ptrs) #~~ preds

  let _Ex x (exs, pures, ptrs, preds) =
    let fvs' = List.concat (BExp.fv |>>| pures) @ List.concat (Pointer.fvs |>>| ptrs) @ List.concat (Predicate.fvs |>>| preds) in
    let fvs = Exp.toStr |>>| fvs' in
    if x |<- fvs && not (x |<- exs) then
      (x::exs, pures, ptrs, preds)
    else
      (exs, pures, ptrs, preds)

  let _EX x fs =
    (_Ex x) |>>| fs

  let _Exs f xs = (fun f x -> _Ex x f) |->> (f, xs)

  let new_ptr = ref []

  (* let print = pp Format.std_formatter *)
  let print (exqs, eqformulas, pointers, predicates) =
    pw "([";
    iterS ps ";" exqs;
    pw "],[";
    iterS BExp.print ";" eqformulas;
    pw "],[";
    iterS Pointer.print ";" pointers;
    pw "],[";
    iterS Predicate.print ";" predicates;
    pw "])"

  let pp_ut' ppf (exqs, eqformulas, pointers', predicates') =
    let (pointers, sizes) = pointers', []  in
    let (predicates, mallocblocks) = predicates', [] in
    let b = List.length exqs > 0 in
    Format.fprintf ppf "@[<2>%s@,%a%s@,%a@;%s@,%a@;%s@,%a@;%s@,%a@]"
      (if b then "Ex " else "")
      (Ftools.pp_list_with Exp.pp_var' ",@,") ((fun x -> (x,[])) |>>| exqs)
      (if b then ". " else "")
      (Ftools.pp_list_with BExp.pp' "@;&@;") eqformulas
      (if List.length eqformulas > 0 then "& " else "")
      (Ftools.pp_list_with Pointer.pp' "@;*@;") pointers
      (if List.length pointers > 0 && List.length predicates > 0 then
        "* "
       else if List.length pointers = 0 && List.length predicates = 0 && List.length mallocblocks = 0 then
        "Emp"
       else "")
      (Ftools.pp_list_with Predicate.pp' "@;*@;") predicates
      (if (List.length pointers > 0 || List.length predicates > 0) && List.length mallocblocks > 0 then
        "* "
       else "")
      (Ftools.pp_list_with (fun ppf (st,en) -> Format.fprintf ppf "@[<2>MallocBlock(%a,@,%a)@]" Exp.pp' st Exp.pp' en) "@;*@;") mallocblocks

  let fstr () ((exqs : string list), eqformulas, pointers', predicates') =
    let (pointers, sizes) = pointers', []  in
    let (predicates, mallocblocks) = predicates', [] in
    let b = List.length exqs > 0 in
    let fs s f () = concatS s (f ()) in
    let sss () = Format.sprintf "%s" in
    Format.sprintf "@[<2>%s@,%a%s@,%a@;%s@,%a@;%s@,%a@;%s@,%a@]"
      (if b then "Ex " else "")
      (fs "," sss) exqs
      (if b then ". " else "")
      (fs " & " BExp.fstr) eqformulas
      (* (Ftools.pp_list_with BExp.pp' "@;&@;") eqformulas *)
      (if List.length eqformulas > 0 then "& " else "")
      (fs " & " Pointer.fstr) pointers
      (* (Ftools.pp_list_with Pointer.pp' "@;*@;") pointers *)
      (if List.length pointers > 0 && List.length predicates > 0 then
        "* "
       else if List.length pointers = 0 && List.length predicates = 0 && List.length mallocblocks = 0 then
        "Emp"
       else "")
      (fs " & " Predicate.fstr) predicates
      (* (Ftools.pp_list_with Predicate.pp' "@;*@;") predicates *)
      (if (List.length pointers > 0 || List.length predicates > 0) && List.length mallocblocks > 0 then
        "* "
       else "")
      (let fstr_mb () (st,en) = Format.sprintf "@[<2>MallocBlock(%a,@,%a)@]" Exp.fstr st Exp.fstr en in
       fs " * " fstr_mb) mallocblocks
      

    
  (* let upprint = pp_ut' Format.std_formatter *)
  let upprint (exqs, eqformulas, pointers', predicates') =
    let (pointers, sizes) = pointers', []  in
    let (predicates, mallocblocks) = predicates', [] in
    if List.length exqs > 0 then
      begin
      pw "Ex";
      iterS p "," ((fun x -> Exp.var_get_printable_string (x, [])) |>>| exqs);
      pw ".";
      end
    else
      p "";
    iterS BExp.pprint " & " eqformulas;
    if List.length eqformulas > 0 then
      p " & "
    else
      p "";
    iterS Pointer.pprint " * " pointers;
    if List.length pointers > 0 && List.length predicates > 0 then
      p " * "
    else if List.length pointers = 0 && List.length predicates = 0 && List.length mallocblocks = 0 then
      p "Emp"
    else
      p "";
    iterS Predicate.pprint " * " predicates;
    if (List.length pointers > 0 || List.length predicates > 0) && List.length mallocblocks > 0 then
      p " * ";
    iterS (fun (st,en) -> p "MallocBlock("; Exp.pprint st; p ","; Exp.pprint en; p ")") " * " mallocblocks
;;

  let pp' ppf = function
      [] -> Format.pp_print_string ppf "0 = 0"
    | form -> Ftools.pp_list_with pp_ut' "&;||&;" ppf form
  
  (* let pprint = pp' Format.std_formatter *)
  let pprint = function
      [] -> p "0 = 0"
    | form ->
      iterS upprint " || " form
      (* p " & ";
      iterS (fun (x, (l,u)) -> Exp.Var.pprint x; p ":["; Exp.pprint l; p ","; Exp.pprint u; p "]") " & " intv *)

  let pp_intv ppf intv =
    Ftools.pp_list_with
      (fun ppf (x,(l,u)) -> Format.fprintf ppf "%a@[<2>:[%a,@,%a]@]" Exp.pp_var' x Exp.pp' l Exp.pp' u)
      "@;&@;" ppf intv

  (* let pprint_intv = pp_intv Format.std_formatter *)
  let pprint_intv intv = iterS (fun (x, (l,u)) -> Exp.var_pprint x; p ":["; Exp.pprint l; p ","; Exp.pprint u; p "]") " & " intv

  let pp_pairs ppf pairs =
    Ftools.pp_list_with
      (fun ppf (x,t) -> Format.fprintf ppf "%a@,=@,%a" Term.pp' x Term.pp' t)
      "@;,@;" ppf pairs

  (* let pprint_pairs = pp_pairs Format.std_formatter *)
  let pprint_pairs pairs = iterS (fun (x, t) -> Term.pprint x; p "="; Term.pprint t) " , " pairs

  let pp_entl ppf (pre, post) =
    Format.fprintf ppf "%a&;|-@;%a@." pp' pre pp' post

  (* let pprint_entl = pp_entl Format.std_formatter *)
  let pprint_entl (pre, post) =
    pprint pre; p " |- "; pprint post; pn ""
(*
  let utranslate packs loc (exqs, exprs, _, ptrs, preds) =
    (exqs,
    BExp.toBExp packs loc |>>| exprs,
    Pointer.translate packs |>>| ptrs,
    Predicate.translate packs |>>| preds
    )
 *)
    
  let map f (exs, pures, ptrs, preds) =
    let pures' = (BExp.map f) |>>| pures in
    let ptrs'  = (Pointer.map f) |>>| ptrs in
    let preds' = (Predicate.map f) |>>| preds in
    (exs, pures', ptrs', preds')
    (*
  let uformula__string (tr_exp : Exp.t -> Exp.t) (exs,pures,pointers,preds) =
    (exs,
     BExp.bexp__string tr_exp |>>| pures,
     (fun (p, data) ->
       Term.term__string tr_exp p,
       (fun (f, s) -> (f, Term.term__string tr_exp s)) |>>| data
     ) |>>| pointers,
     (fun (s, data) ->
       s, Term.term__string tr_exp |>>| data
     ) |>>| preds
    )
     
  let enc_to_string = uformula__string Exp.exp_to_string_attr;;

  let dec_from_string = uformula__string Exp.string_to_exp_attr;;
     *)
(*
  let translate packs form : t = ((utranslate packs Locs.dummy) |>>| form)
 *)
    
  let ( *** ) (a1,b1,c1,d1) (a2,b2,c2,d2) = (a1@a2, b1@b2, c1@c2, d1@d2)

  let merge form1 form2 = (List.map2 ( *** ) form1 (fst form2), snd form2)

  let uis_equal (exqs1, eqfs1, ptrs1, preds1) (exqs2, eqfs2, ptrs2, preds2) =
    List.length exqs1 = List.length exqs2 &&
    ptrs1 |==| ptrs2 &&
    preds1 |==| preds2 &&
    List.length eqfs1 = List.length eqfs2

  let is_equal form1 form2 = List.for_all (fun x -> x) (List.map2 uis_equal form1 form2)

  let rec uis_in_term x = function
      BExp.UNIT (a, _, b) -> x = a || x = b
    | BExp.OP (a, _, b) -> (uis_in_term x a) || (uis_in_term x b)
    | BExp.NOTHING -> false
    | BExp.LBL (_, b) -> uis_in_term x b
    | BExp.BLOCK (a, b) -> raise SError

  let is_in_term x form = List.exists (fun f -> uis_in_term x f) form

  let rec (===) (eqlist: BExp.t list) (x: Term.t) (y: Term.t)  =
    if x = y then
      true
    else if (Term.toStr x) = (Term.toStr y) then
      true
    else if (List.exists (fun z ->
        match z with
        | BExp.UNIT (x', Op.MAPSTO, y')
        | BExp.UNIT (x', Op.EQ, y') ->
          let b = ((x' = x) && y' = y) || ((x' = y) && (y' = x)) in
          b
        | _ -> false) eqlist) (** Direct structural match*)
    then
      true
    else
      let (eqs,rest) =
        List.partition (fun z ->
            match z with
            | BExp.UNIT (x',Op.EQ, y') -> (x' = y) || (y' = y)
            | _ -> false) eqlist
      in (** Partition all z=y or y=z vs rest *)
      let es = List.map (fun z ->
          match z with
          | BExp.UNIT (x', Op.EQ, y') -> if x' = y then y' else x'
          | _ -> Term.NULL) eqs
      in       (** Collect the zs *)
      List.exists (rest === x) es       (** a=b, b=c implies a=c *)

  let fresh_variable (x:int) : int = x + 1

  let is_not_null eqs pt =
    let s_pt = Term.toStr pt in
    if pt |<- !new_ptr then
      true
    else
      let e_pt = Term.toExp pt in
      if (not (Exp.is_ptr e_pt)) && Exp.is_struct e_pt then
        true
      else
        let f = function
          | BExp.UNIT (Term.NULL, Op.NE, a) when (Term.toStr a) = s_pt -> true
          | BExp.UNIT (a, Op.NE, Term.NULL) when (Term.toStr a) = s_pt -> true
          | BExp.UNIT (b, Op.NE, a) when (Term.toStr a) = s_pt && b == Term.zero -> true
          | BExp.UNIT (a, Op.NE, b) when (Term.toStr a) = s_pt && b == Term.zero -> true
          | BExp.UNIT (a, Op.NE, Term.EXP (Exp.CONST 0)) -> (Term.toStr a) = s_pt
          | BExp.UNIT (Term.EXP (Exp.CONST 0), Op.NE, a) -> (Term.toStr a) = s_pt
          | _ -> false
        in
        List.exists f eqs

  let uadd_to_formula (var_list, eq_list, ptr_list, pred_list) (c: BExp.t) =
    (var_list, c::eq_list, ptr_list, pred_list)

  let add_to_formula (form) (c: BExp.t) : t =
    ((fun x -> uadd_to_formula x c) |>>| form)

  let uadd_to_spatial (var_list, eq_list, ptr_list, pred_list) (c: Pointer.t) =
    (var_list, eq_list, c::ptr_list, pred_list)

  let add_to_spatial (form) (c: Pointer.t) : t =
    ((fun x -> uadd_to_spatial x c) |>>| form)

  let rec get_real_var eq_list ptr =
    let isnotvar = match ptr with
        Term.EXP (Exp.VAR _) -> false
      | _ -> true
    in
    let sptr = Term.toStr ptr  in
    if String.get sptr 0 = '.' || isnotvar then
      None
    else
      Some (sptr)

  let is_memory_leak eq_list ptr_list exp right wrong loc =
    begin
      let ptrs = List.filter (fun (ptr, _) -> (eq_list === ptr) exp) ptr_list in
      match ptrs with
      | [] -> right
      | x::_ ->
        let s_exp = Term.toStr exp in
        if s_exp = "$ret" || not (Exp.is_ptr (Term.toExp exp)) then
          right
        else
          match get_real_var eq_list exp with
            Some (sexp) ->
            if not (Exp.is_param (Term.toExp exp)) then
              warn ("Memory Leak (re-assignment) by " ^ sexp) loc wrong
            else
              wrong
          | None -> wrong
    end

  let is_null_pointer_dereferenced (pt: Term.t) (loc:Locs.t) eq_list default =
    match pt with
      Term.NULL -> default
    | Term.EXP Exp.NOTHING -> default
    | Term.EXP (Exp.BINOP (Exp.NOTHING, _, _)) -> default
    | _ ->
      match get_real_var eq_list pt with
        Some (spt) ->
        if Exp.is_param (Term.toExp pt) then
          warn ("Null Pointer Dereferenced by parameter " ^ spt) loc default
        else
        warn ("Null Pointer Dereferenced by pointer " ^ spt) loc default
      | None -> default


(*
  let uget_from ((a, eq_list, ptr_list, b)) (pt: Term.t) (i:Field.t) (loc:Locs.t)  : Term.t =
    let (matched, _) = List.partition (fun (p,_) -> (eq_list === p) pt) ptr_list in
    match matched with
    | [] ->
      if is_not_null eq_list pt then
        Term.encode (Exp.Var.string_to_var (newvar ()))
      else
        is_null_pointer_dereferenced pt loc eq_list Term.NULL
    | (_, elements)::_ ->
      let (x1, _) = List.partition (fun (a,_) -> a = i) elements in
      match x1 with
      | [] ->
        Term.NULL
      | (_, element)::_ -> element

  let get_from (form: t) (pt: Term.t) (i:Field.t) (loc:Locs.t)  : Term.t list =
    (fun x -> uget_from x pt i loc) |>>| form
 *)

  let uset_to  ((a, eq_list, ptr_list, d) as formula) (pt: Term.t) (field:Field.t) (v:Term.t) (loc:Locs.t)  =
    (** Check the pointer. *)
    let (matched, unmatched) = List.partition (fun (p ,_) ->
        let b = (eq_list === p) pt in
        (* if b then
          begin
            (*iterS BExp.pprint " & " eq_list;*)
            pn "true";
           end; *)
        b
      ) ptr_list in
    match matched with
    | [] ->
      begin
        (*  iterS BExp.pprint " & " eq_list; *)
        if is_not_null eq_list pt then
          formula
        else
        (** pt is not found in the heap. So give a abort condition. *)
          is_null_pointer_dereferenced pt loc eq_list formula
      end
    | (p, ps)::[] ->
      (** Check the field. *)
      begin
      match (fun (a, _) -> a = field) |>- ps with
      | (_, existing_val)::[] ->
        let x = (p, (fun (a, b) -> if a = field then (a, v) else (a, b)) |>>| ps) in
        let _ = is_memory_leak eq_list ptr_list existing_val (x::unmatched) ptr_list loc in
        (a, eq_list, x::unmatched, d)
      | [] ->
        (* warn ("# ABORT: " ^ field  ^ " is not a known field of " ^ (Term.toStr p)) loc*) formula
      | _ ->
        warn ("ABORT: too many " ^ (field) ^ " fields") loc formula
      end
    | _ ->
      match get_real_var eq_list pt with
        Some (spt) -> warn ("Memory Leak (Unreferenced Memory) by " ^ spt) loc (a, eq_list, ptr_list, d)
      | None -> (a, eq_list, ptr_list, d)

  let set_to  (formula: t) (pt: Term.t) (field:Field.t) (vs: Term.t list) (loc:Locs.t) : t =
    List.map2 (fun x y -> uset_to x pt field y loc) formula vs

  let udelete_from ((a, eq_list, ptr_list, d)) (pt: Term.t) (loc:Locs.t)  =
    let (matched, unmatched) = List.partition (fun (p, ps) -> ((eq_list === pt) p)) ptr_list in

    match matched with
    | [] ->
      (* if is_not_null eq_list pt then *)
        (a, eq_list, ptr_list, d)
          (* else
             is_null_pointer_dereferenced pt loc eq_list (a, eq_list, ptr_list, d) *)
    | (p,_)::_ ->
      (a, eq_list, unmatched, d)

  let delete_from (formula:t) (pt: Term.t) (loc:Locs.t)  : t =
    (fun x -> udelete_from x pt loc) |>>| formula

  (** This is blind substitution *)
  let usubstitute (to_be_replaced : Term.t) (replaced_by : Term.t)
                 ((exqs, bexps, pointers, predicates)) (* (l:Locs.t)*) =
    let bexps'      = (BExp.substitute to_be_replaced replaced_by)      |>>| bexps in
    let pointers'   = (Pointer.substitute to_be_replaced replaced_by)   |>>| pointers in
    let predicates' = (Predicate.substitute to_be_replaced replaced_by) |>>| predicates in
    let bexps'' = uniq bexps' in
    (* let exqs' = (fun x ->
      let tbr = Exp.Var.decode (Term.decode to_be_replaced) in
      if replaced_by <> Term.NULL && tbr = x then
        Exp.Var.decode (Term.decode replaced_by)
      else
        x
      ) |>>| existential_quantifiers
       in *)
    (exqs, bexps'', pointers', predicates')

  let substitute (to_be_replaced : Term.t) (replaced_by : Term.t) (formula : t) (l:Locs.t): t =
    let formula' = (fun x -> usubstitute to_be_replaced replaced_by x) |>>| formula in
    (* let intv' =
      match to_be_replaced, replaced_by with
        Term.EXP (Exp.VAR x), Term.EXP (Exp.VAR y) ->
        (fun (v, (l, r)) ->
          let v' = if Exp.Var.eq v x then y else v in
          let l' = Exp.substitute (Exp.VAR x) (Exp.VAR y) l in
          let r' = Exp.substitute (Exp.VAR x) (Exp.VAR y) r in
          (v', (l', r'))) |>>| intv
      | _ -> intv in *)
    formula'

  (** This is blind substitution *)
  let upar_subs subs_pairs
                 ((exqs, bexps, pointers, predicates)) (* (l:Locs.t)*) =
    let bexps'      = (BExp.par_subs subs_pairs)      |>>| bexps in
    let pointers'   = (Pointer.par_subs subs_pairs)   |>>| pointers in
    let predicates' = (Predicate.par_subs subs_pairs) |>>| predicates in
    let bexps'' = uniq bexps' in
    (* let exqs' = (fun x ->
      let tbr = Exp.Var.decode (Term.decode to_be_replaced) in
      if replaced_by <> Term.NULL && tbr = x then
        Exp.Var.decode (Term.decode replaced_by)
      else
        x
      ) |>>| existential_quantifiers
       in *)
    (exqs, bexps'', pointers', predicates')

  let par_subs subs_pairs (formula : t) : t =
    let formula' = (upar_subs subs_pairs) |>>| formula in
    formula'
    
  (** Substitution with alpha conversion *)
  let usubs ((existential_quantifiers, eqformulas, pointers, predicates))
                      (to_be_replaced : Term.t) (replaced_by : Term.t) =
    let _formula =
      (** Alpha conversion steps *)
      (** Term.decode does not work well with NULL, so, precautionary NULL checking *)
      if replaced_by <> Term.NULL then
        (* let s_replaced_by = Exp.Var.decode (Term.decode replaced_by) in *)
        (** Only to be replaced by quantifier variables *)
        (* if s_replaced_by |<- existential_quantifiers then
          (** New variable for alpha conversion *)
          let _s_replaced_by = new_log_var s_replaced_by in
          let _existential_quantifiers = (s_replaced_by := _s_replaced_by) |>>| existential_quantifiers in
          let _replaced_by = Term.encode (Exp.Var.string_to_var _s_replaced_by) in
          (** Alpha conversion *)
          usubstitute  replaced_by _replaced_by
                      (_existential_quantifiers, eqformulas, pointers, predicates)
        else *)
        (existential_quantifiers, eqformulas, pointers, predicates)
      else
      (existential_quantifiers, eqformulas, pointers, predicates)
    in
    (** Actual substitution *)
    usubstitute  to_be_replaced replaced_by _formula

  let (:=) (formula: t) (to_be_replaced : Term.t) (replaced_by : Term.t) : t =
    let formula' = (fun x -> usubs x to_be_replaced replaced_by) |>>| formula in
    formula'

  (** Context free consistency checking *)
  let uis_consistent ((existential_quantifiers, eqformulas, pointers, predicates)) : bool =
    let is_eqformula_consistent = BExp.is_consistent |>>| eqformulas |> l_and in
    let is_pointers_consistent = Pointer.is_consistent |>>| pointers |> l_and in
    is_eqformula_consistent && is_pointers_consistent

  let is_consistent (formula : t) : bool =
    List.exists uis_consistent formula

  let unfold (args : Term.t list) (params : string list) (formula : t) : t =
    let term_params = Term.encode_str |>>| params in
    List.fold_left2 (:=) formula term_params args

  let matched (args : Term.t list) (params : string list) (formula : t) : t option =
    let applied_formula = unfold args params formula in
    if is_consistent applied_formula then
      Some applied_formula
    else
      None

  let udrop_pure lpointers all_pointers ((exqs, eqformulas, pointers, predicates)) =
    let t_exqs = Term.encode_str |>>| exqs in
    let _eqformulas = List.filter (BExp.non_trivial t_exqs lpointers all_pointers) eqformulas in
    (exqs, _eqformulas, pointers, predicates)

  let drop_pure lpointers all_pointers formula =
    (udrop_pure lpointers all_pointers) |>>| formula


  let i_to_v (x:int) : string = "ex" ^ (string_of_int x)

  let root (_, _, pointers, predicates) = (List.concat (Pointer.root |>>| pointers)) @ (List.concat (Predicate.root |>>| predicates))

  let type_check vars to_static attrs texp loc =
    (* let attrs =
      try
        let var = List.find (fun (x, _) -> x = svar) vars in
        Exp.Var.get_attributes var
      with
        Not_found -> []
       in *)
    match texp with
    | Term.NULL -> true (* Exp.Var.PTR |<- attrs *)
    | Term.EXP Exp.FCALL (_,_) -> true
    | Term.EXP exp ->
      match exp with
        Exp.VAR var ->
        if Exp.EXQ |<- (snd var) then
          true
        else
          begin
            match Exp.var_be_typed vars to_static loc var with
              Exp.VAR (_, attrs_exp) ->
               List.length attrs_exp >= List.length attrs &&
                 List.for_all (fun x -> x |<- attrs_exp) attrs
            | _ -> false
          end
      | Exp.BINOP (Exp.VAR var, Op.ADD, exp2) ->
         begin
           match Exp.var_be_typed vars to_static loc var with
             Exp.VAR (_, attrs_exp) ->
              List.length attrs_exp >= List.length attrs &&
                List.for_all (fun x -> x |<- attrs_exp) attrs &&
                  Exp.is_pure exp2
           | _ -> false
         end
        | _ -> not (Exp.PTR |<- attrs)

  (** x=exp[x:=x'] & (PI & SIGMA)[x:=x'] *)
  let uassign ?bypass:(bp=false) vars to_static ((exq, eqs, ptrs, preds) as ass) (x: Exp.var_t) (e:Term.t) (fv:int) (loc:Locs.t) =
    match Exp.var_be_typed vars to_static loc x with
      Exp.VAR (svar, attrs) ->
      let e             = Term.be_typed vars to_static loc e in
      (* Exp.Var.pprint x; *)
      if bp || type_check vars to_static attrs e loc || (Exp.var_to_str x = "$ret") || (Term.toStr e = "$ret") then
        let ix'   = fresh_variable fv in
        let i_s   = i_to_v ix' in
        let vx'   = Exp.set_attributes (Exp.string_to_var i_s) attrs in
        let tx    = Term.encode (svar, attrs) in                        (** x *)
        let tx'   = Term.EXP vx' in                                  (** x' *)
        let e'    = Term.substitute tx tx' e in                         (** exp[x:=x'] *)
        let ass'  = usubstitute tx tx' (i_s::exq, eqs, ptrs, preds) in                        (** (PI & SIGMA)[x:=x'] *)
        let ass1  = uadd_to_formula ass' (BExp.UNIT (tx, Op.EQ, e')) in  (** x=exp[x:=x'] & (PI & SIGMA)[x:=x'] *)

        (* pprint ass; pn "[0]"; pprint ass'; pn "[1]";  pprint ass1; pn ""; *)
        is_memory_leak eqs ptrs tx (ass1, ix') (ass, fv) loc
      else
        (ass, fv)
    | _ -> (ass, fv)
  (* warn ("TYPE MISMATCH: " ^ (Exp.var_toStr x) ^ " and " ^ (Term.toStr e) ^ " have different type") loc (ass, fv) *)


  let assign ?bypass:(bp=false) vars to_static (formula: t) (x: Exp.var_t) (es:Term.t list) (fv:int) (loc:Locs.t): t * int =
    let z = List.map2 (fun form e -> uassign ?bypass:(Some bp) vars to_static form x e fv loc) formula es in
    fst |>>| z, (snd (List.hd z))

  let ube_typed (a, b, c, d) vars loc =
    let b' = (BExp.be_typed vars loc) |>>| b in
    let c' = (fun (x,y) -> (Term.be_typed vars loc x, (fun (l,t) -> (l, Term.be_typed vars loc t)) |>>| y)) |>>| c in
    let d' = (fun (x,y) -> (x, (Term.be_typed vars loc) |>>| y)) |>>| d in
    (a, b', c', d')

  let be_typed form vars loc =
    (fun f  -> ube_typed f vars loc) |>>| (fst form), snd form

  let to_emp_fields formulas =
    (fun (a,b,c,d) -> (a,b,Pointer.to_emp_field |>>| c,d)) |>>| formulas

  let fv (a,b,c,d) =
    (List.concat (BExp.fv |>>| b)) @
      ((List.concat (Pointer.fvs |>>| c))) @
        ((List.concat (Predicate.fvs |>>| d))) 

  let fv_no_field (a,b,c,d) =
    (List.concat (BExp.fv |>>| b)) @
      ((List.concat ((fun (pt, _) -> Term.fv pt) |>>| c))) @
        ((List.concat (Predicate.fvs |>>| d))) 


  let fpurify formula =
    let pur = (fun (a,b,c,d) ->
            let b' = (fun x ->
                match x with
                | BExp.UNIT (x, _, _) -> let sx = Term.toStr x in if sx = "$return" || sx = "$continue" || sx = "$break" then false else true
                | _ -> false
              ) |>- b in
            let fvs = fv (a,b',c,d) in
            let a' = List.sort_uniq (String.compare) ((fun v -> v |<- (Exp.toStr |>>| fvs)) |>- a) in
            (a',b',c,d)
      ) in
    (pur |>>| formula)

  let purify (fstx, sndx) =
       (fpurify fstx, fpurify sndx)



(* ) |>>| entls *)
(*    in
      entls' *)
end;;
 
