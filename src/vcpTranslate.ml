open Ftools
open VcpBase
open Ctransform
open VcpExtract

module C = Cabs
module V = Map.Make(String)

         
module Block = struct
  
  type init = INIT_E | INIT_S of Term.t | INIT_M of init list
                                                                   
  type t =
    | SKIP
    | ASSERT of Formula.t * t * Locs.t
    | ASSIGN of Exp.t * Term.t * t * Locs.t
    | IF of BExp.t * t * t * t * Locs.t
    | WHILE of BExp.t * BExp.t list * t * Formula.t * t * Locs.t
    | PROCCALL of Term.t * Term.t list * int * t * Locs.t
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

  let nopointermode = ref false
  let ormode = ref false
  let noformulamode = ref false

  let ptr_structs : (string * string * Exp.t list) list ref = ref []

  let structures : (string * (Exp.t * Term.t) list * Exp.t list option) V.t ref = ref (V.empty)
                   
  let unions = ref []

  let enums = ref []

  (* let bb : int ref = ref 0 *)
  (* let func_name : Exp.var_t ref = ref ("", []) *)
  let func_ptrs = ref []
  let func_ptr_types : (string * Exp.attr list) list ref = ref []

  (** from, to, array_dimension *)
  let array_aliases : (string * Exp.t list) V.t ref = ref V.empty
  let s_aliases : (string V.t) ref = ref V.empty

  let in_func = ref "";;
  let local_statics : string V.t ref = ref V.empty;;

  let to_static ?local_static:(ls=false) name =
    if String.contains name '%' then
      name
    else
      if !in_func <> "" && (ls || (V.mem name !local_statics && V.find name !local_statics = !in_func))  then 
        begin
          let static_name = ("^" ^ !in_func ^ "^" ^ !cfunc ^ "%" ^ name) in
          if ls then local_statics := V.add name !in_func !local_statics;
          static_name
        end
      else
        (!cfunc ^ "%" ^ name)
    

  let is_static = List.exists (function Exp.STATIC -> true | _ -> false)
                     
  let packs fvs  =
    let sts = V.map (fun (a, b, c) -> (a, (fun (p,_) -> p) |>>| b)) !structures in
    (fvs, to_static, !array_aliases, sts, !s_aliases)
  let dummy_packs = (V.empty, to_static, V.empty, V.empty, V.empty)

  let is_func_ptr_type nm =
    List.exists (fun (a,_) -> a=nm) !func_ptr_types

  let get_func_ptr_attr nm =
    try
      snd @@ List.find (fun (a,_) -> a=nm) !func_ptr_types
    with
      _ -> raise (StError "get_func_ptr_attr")
    
  let rec get_original_type s =
    Exp.get_original_type !s_aliases s
                  
  let rec print_init = function
      INIT_E -> ()
    | INIT_S t -> Term.pprint t
    | INIT_M il -> p "["; iterS print_init ";" il; p "]"
  ;;
  
  let rec print_decl_type = function
    | C.JUSTBASE -> pw "JUSTBASE"                              (* Prints the declared name *)
    | C.PARENTYPE (attribute_list1, decl_type, attribute_list2) ->
       pw "PARENTYPE"; print_decl_type decl_type
    | C.ARRAY (decl_type, attribute_list, expression) ->
       pw "ARRAY ("; Cprint.print_expression expression; p ") "; print_decl_type decl_type; 
    | C.PTR (attribute_list, decl_type) ->
       pw "PTR"; print_decl_type decl_type
    | C.PROTO (decl_type, single_name_list, bool) -> 
       pw "PROTO"; print_decl_type decl_type; p " "; pb bool;;
  
  let print_type_spec = function
      C.Tvoid -> pw "void"                             (* Type specifier ISO 6.7.2 *)
    | C.Tchar -> pw "char"
    | C.Tbool -> pw "bool"
    | C.Tshort -> pw "short"
    | C.Tint -> pw "int"
    | C.Tlong -> pw "long"
    | C.Tint64 -> pw "int64"
    | C.Tfloat -> pw "float"
    | C.Tdouble -> pw "double"
    | C.Tsigned -> pw "signed"
    | C.Tsizet -> pw "size_t"   (* used temporarily to translate offsetof() *)
    | C.Tunsigned -> pw "unsigned"
    | C.Tnamed string -> pw ("named " ^ string)
    (* each of the following three kinds of specifiers contains a field 
     * or item list iff it corresponds to a definition (as opposed to
     * a forward declaration or simple reference to the type); they
     * also have a list of __attribute__s that appeared between the
     * keyword and the type name (definitions only) *)
    | C.Tstruct (string, field_group_list_option, _) -> pw ("struct " ^ string)
    | C.Tunion (string, field_group_list_option, _) -> pw ("union " ^ string)
    | C.Tenum (string, field_group_list_option, _) -> pw ("enum " ^ string)
    | C.TtypeofE  expression -> pw "typeofE "; pw (VCabs.c_print expression)                     (* GCC __typeof__ *)
    | C.TtypeofT (specifier, decl_type) -> pw "typeofT"       (* GCC __typeof__ *)

                                         
  let print_spec_elem = function
      C.SpecTypedef -> pw "SpecTypedef"         
    | C.SpecCV cvspec -> pw "SpecCV"            (* const/volatile *)
    | C.SpecAttr attribute -> pw "SpecAttr"      (* __attribute__ *)
    | C.SpecStorage storage -> pw "SpecStorage"
    | C.SpecInline -> pw "SpecInline"
    | C.SpecType typeSpecifier -> pw "SpecType"; print_type_spec typeSpecifier
    | C.SpecPattern string -> pw "SpecPattern"    

  let rec print_initwhat = function
    | C.NEXT_INIT -> pw "NEXT_INIT"
    | C.INFIELD_INIT (ss, iw) -> pw "INFIELD_INIT"; pw ss; print_initwhat iw 
    | C.ATINDEX_INIT (_, iw) -> pw "ATINDEX_INIT"; print_initwhat iw
    | _ -> p "ATINDEXRANGE_INIT"
                            
  let rec print_init_exp init_expression =
    match init_expression with
    | C.NO_INIT -> pw "NO_INIT"
    | C.SINGLE_INIT (exp) -> pw "SINGLE_INIT "; Cprint.print_expression exp
    | C.COMPOUND_INIT (iw_ie_l) ->
       p "COMPOUND_INIT [";
       iterS (fun (iw, ie) -> p "("; print_initwhat iw; p ","; print_init_exp ie; p ")") ";" iw_ie_l; pw "]";;

  let print_name (nm, dt, _, _) =
    pw nm;
    p "{";
    print_decl_type dt;
    p "}";;

  let print_init_name (name, ie) =
    print_name name;
    pw "-";
    print_init_exp ie;
    pn "";;

  let print_fvs fvs =
    V.iter (fun k v -> Exp.print (Exp.VAR (k,v)); pw ", ") fvs; pn "";;
  
  let func_pos = ref 0;;

  let new_func () =
    func_pos := !func_pos + 1;
    !func_pos

  let __E x = Exp.VAR x

  let __V = function Exp.VAR v -> v | _ -> raise (StError "Not a variable")

  let __A = function Exp.VAR (_,a) -> a | _ -> raise (StError "Not a variable")

  let __N = function Exp.VAR (n,_) -> n | _ -> raise (StError "Not a variable")
    
  let dummy_loc = {C.lineno = 0; C.filename = ""; C.byteno = 0; C.ident = 0}
                                                                                               
  let update_structures sts =
    (* if V.mem "child_process" sts then
    (  let (_, flds, _) = V.find "child_process" sts in
      pn "child_process";
      pi (List.length flds)
    ); *)
    structures := sts
    
  let one = Term.EXP (Exp.CONST 1)

  let zero = Term.zero
  let zero_g = C.CONSTANT (C.CONST_INT "0")
  let true_f = BExp.UNIT (zero, Op.EQ, zero)
  let true_g = C.BINARY (C.EQ, zero_g, zero_g)
             
  let t_u_op = function
    | C.MINUS  -> "-"
    | C.PLUS -> "+"
    | C.NOT -> "!"
    | C.BNOT -> "~"
    | C.MEMOF -> "*"
    | C.ADDROF -> "&"
    | C.PREINCR -> "++"
    | C.PREDECR -> "--"
    | C.POSINCR -> "++"
    | C.POSDECR -> "--"

  let t_b_op = function
    | C.ADD -> "+"
    | C.SUB -> "-"
    | C.MUL -> "*"
    | C.DIV -> "/"
    | C.MOD -> "%"
    | C.AND -> "&&"
    | C.OR -> "||"
    | C.BAND -> "&"
    | C.BOR -> "|"
    | C.XOR -> "^"
    | C.SHL -> "<<"
    | C.SHR -> ">>"
    | C.EQ -> "=="
    | C.NE -> "!="
    | C.LT -> "<"
    | C.GT -> ">"
    | C.LE -> "<="
    | C.GE -> ">="
    | C.ASSIGN -> "="
    | C.ADD_ASSIGN -> "+="
    | C.SUB_ASSIGN -> "-="
    | C.MUL_ASSIGN -> "*="
    | C.DIV_ASSIGN -> "/="
    | C.MOD_ASSIGN -> "%="
    | C.BAND_ASSIGN -> "&="
    | C.BOR_ASSIGN -> "|="
    | C.XOR_ASSIGN -> "^="
    | C.SHL_ASSIGN -> "<<="
    | C.SHR_ASSIGN -> ">>="

  let extra sl =
    match (String.length sl) with
    | 0 -> "       "
    | 1 -> "    "
    | 2 -> "   "
    | 3 -> "  "
    | 4 -> " "
    | _ -> ""

  (** Print line numbers. *)
  let printl (_, l) =
    let sl = string_of_int l in
    p "["; p sl; p "]"; p (extra sl)

  let rec pprint t = function
    | SKIP -> p ""
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
       p (extra "");
       pt "}" t;
       pn "";
       pprint t y
    | DECL (a, len, init_data, y, l) ->
       begin
         printl l;
         pt "decl " t;
         (if Exp.is_struct a then
            let st = Exp.get_struct_name a in
            pw st;
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
    | ASSERT (a, y, l) ->
       begin
         printl l;
         pn "assert" ;
         pprint t y
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
         if List.length bs > 0 then
           (iterS BExp.pprint "&" bs)
         else
           pw "{True}";
         pn "";
         pprint t b; pprint t y
       end
    | PROCCALL (a, b, i, y, l) ->
       begin
         printl l;
         pt "" t;
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


  let rec print = function
    | SKIP ->
       p "SKIP"
    | RETURN (i, y, l) ->
       p "RETURN (";
       Term.print i;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | CONTINUE (y, l) ->
       p "CONTINUE (";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | BLOCK (a, y, l) ->
       p "BLOCK ((";
       print a;
       p "),(";
       print y;
       p "),";
       p (Locs.print l);
       p ")"
    | DECL (a, len, init_data, y, l) ->
       p "DECL (";
       Exp.print a;
       p ",";
       print_list Exp.print len;
       p ",";
       (* print_list Term.print init_data;
       p ","; *)
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | ASSIGN (a, b, y, l) ->
       p "ASSIGN (";
       Exp.print a;
       p ",";
       Term.print b;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")" 
    | ASSERT (a, y, l) ->
       p "ASSERT ([([],[],[],[])],";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | IF (a, b, c, y, l) ->
       p "IF (";
       BExp.print a;
       p ",";
       print b;
       p ",";
       print c;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | WHILE (a, bs, b, c, y, l) ->
       p "WHILE (";
       BExp.print a;
       p ",";
       print_list BExp.print bs;
       p ",";
       print b;
       p ",";
       p "[([],[],[],[])]";
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | PROCCALL (a, b, i, y, l) ->
       p "PROCCALL (";
       Term.print a;
       p ",";
       print_list Term.print b;
       p ",";
       pl i;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"     
    | CONS (a, b, y, l) ->
       p "CONS (";
       Exp.print a;
       p ",";
       print_list (fun (q,r) -> p "(";Exp.print q; p ","; Term.print r; p ")") b;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")" 
    | MUTATION (a, b, c, y, l) ->
       p "MUTATION (";
       Term.print a;
       p ",";
       p ("\"" ^ b ^ "\"");
       p ",";
       Term.print c;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | LOOKUP (a, b, c, y, l) ->
       p "LOOKUP (";
       Exp.print a;
       p ",";
       Term.print b;
       p ",";
       p ("\"" ^ c ^ "\"");
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | DISPOSE (a, y, l) ->
       p "DISPOSE (";
       Term.print a;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | MAPS (a, b, y, l) ->
       p "MAPS (";
       Exp.print a;
       p ",";
       Exp.print b;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | PARALLEL (b, c, y, l) ->
       p ""
    | MALLOC (a, tl, y, l) ->
       p "MALLOC (";
       Exp.print a;
       p ",";
       Exp.print tl;
       p ",";
       print y;
       p ",";
       p (Locs.print l);
       p ")"
    | SARRAY (a, b, tl, y, l) ->
       p ""
    | LABEL (l, el, y, ll) ->
       p "LABEL (";
       p ("\"" ^ l ^ "\"");
       p ",";
       print y;
       p ",";
       p (Locs.print ll);
       p ")"
    | BREAK (y, ll) ->
       p "BREAK (";
       print y;
       p ",";
       p (Locs.print ll);
       p ")"
    | FAIL ->
       p "FAIL"

  let rec fv_init = function
      INIT_E -> []
    | INIT_S t -> Term.fv t
    | INIT_M ids -> List.concat (fv_init |>>| ids)
      
  let rec fv = function
    | SKIP
      | FAIL -> []
    | ASSERT (_, p, _) -> fv p
    | ASSIGN (v, e_term, p, _) -> (Exp.fv v) @@@ (Term.fv e_term) @@@ (fv p)
    | IF (b, p1, p2, p, _) -> (BExp.fv b) @@@ (fv p1) @@@ (fv p2) @@@ (fv p)
    | WHILE (b, _, p1, _, p, _) -> (BExp.fv b) @@@ (fv p1) @@@ (fv p)
    | PROCCALL (fn, ps, _, p, _) -> (Term.fv fn) @@@ ((List.flatten (Term.fv |>>| ps)) @@@ (fv p))
    | CONS (e, _, p, _) -> (Exp.fv e) @@@ (fv p)
    | MUTATION (t1, _, t2, p, _) -> (Term.fv t1) @@@ (Term.fv t2) @@@ (fv p)
    | LOOKUP (v, t1, _, p, _) -> (Exp.fv v) @@@ (Term.fv t1) @@@ (fv p)
    | DISPOSE (t, p, _) -> (Term.fv t) @@@ (fv p)
    | MAPS (v1, v2, p, _) -> (Exp.fv v1) @@@ (Exp.fv v2) @@@ (fv p)
    | PARALLEL (p1, p2, p, _) -> (fv p1) @@@ (fv p2) @@@ (fv p)
    | SARRAY (v, t, tl, p, _) -> (List.concat (Term.fv |>>| (snd |>>| tl))) @ (Exp.fv v) @@@ (Term.fv t) @@@ (fv p)
    | MALLOC (v, tl, p, _) -> Exp.fv tl @ Exp.fv v @@@ (fv p)
    | BLOCK (t, p, _) -> fv t @ fv p
    | DECL (_, len, init_data, t, _) -> (fv t) @ List.concat (Exp.fv |>>| len) @ (fv_init init_data)
    | RETURN (i, y, _) -> Term.fv i @@@ fv y
    | BREAK (y, _) -> fv y
    | CONTINUE (y, _) -> fv y
    | LABEL (_, _, y, _) -> fv y
                       
  let rec mv = function
    | SKIP | FAIL -> []
    | ASSERT (_, p, _) -> []
    | ASSIGN (v, e_term, p, _) -> (Exp.fv v) @@@ (mv p)
    | IF (b, p1, p2, p, _) -> (mv p1) @@@ (mv p2) @@@ (mv p)
    | WHILE (b, _, p1, _, p, _) -> (mv p1) @@@ (mv p)
    | PROCCALL (_, ps, _, p, _) -> (mv p)
    | CONS (e, _, p, _) -> (Exp.fv e) @@@ (mv p)
    | MUTATION (t1, _, t2, p, _) -> (mv p)
    | LOOKUP (v, t1, _, p, _) -> (Exp.fv v) @@@ (mv p)
    | DISPOSE (t, p, _) -> (mv p)
    | MAPS (v1, v2, p, _) ->  (mv p)
    | PARALLEL (p1, p2, p, _) -> (mv p1) @@@ (mv p2) @@@ (mv p)
    | MALLOC (v, tl, p, _) -> mv p
    | SARRAY (v, t, tl, p, _) -> (mv p)
    | BLOCK (t, p, _) -> mv t @ mv p
    | DECL (_, _, _, t, _) -> mv t 
    | RETURN (i, y, _) -> mv y
    | BREAK ( y, _) -> mv y
    | CONTINUE (y, _) -> mv y
    | LABEL (_, _, y, _) -> mv y

  let rec substitute x y = function
    | SKIP -> SKIP
    | FAIL -> FAIL
    | ASSERT (a, p, b) -> ASSERT (a, substitute x y  p, b)
    | ASSIGN (v, e_term, p, l) ->
       if Term.EXP v = x then
         ASSIGN (Term.toExp y, e_term, substitute x y  p, l)
       else
         ASSIGN (v, Term.substitute x y e_term, substitute x y  p, l)
    | IF (b, p1, p2, p, l) ->
       IF (BExp.substitute x y b, substitute x y  p1, substitute x y  p2, substitute x y  p, l)
    | WHILE (b, bs, p1, a, p, l) ->
       WHILE (BExp.substitute x y b, (BExp.substitute x y) |>>| bs, substitute x y  p1, a, substitute x y  p, l)
    | PROCCALL (fname, ps, i, p, l) ->
       let ps' = (Term.substitute x y) |>>| ps in (* TO CHECK *)
       if fname = x then
         PROCCALL (y, ps', i, substitute x y  p, l)
       else
         PROCCALL (fname, ps', i, substitute x y  p, l)
    | CONS (e, a, p, l) ->
       CONS (Exp.substitute (Exp.VAR (Term.decode x)) (Exp.VAR (Term.decode y)) e, (fun (field, exp) -> (field, Term.substitute x y exp)) |>>| a, substitute x y  p, l)
    | MUTATION (t1, a, t2, p, l) ->
       MUTATION (Term.substitute x y t1, a, Term.substitute x y t2, substitute x y  p, l)
    | LOOKUP (v, t1, a, p, l) ->
       if v = Term.toExp x then
         LOOKUP (Term.toExp y, Term.substitute x y t1, a, substitute x y   p, l)
       else
         LOOKUP (v, Term.substitute x y t1, a, substitute x y  p, l)
    | DISPOSE (t, p, l) ->
       DISPOSE (Term.substitute x y t, substitute x y  p, l)
    | MAPS (v1, v2, p, l) ->
       MAPS (v1, v2, substitute x y  p, l)
    | PARALLEL (p1, p2, p, l) ->
       PARALLEL (substitute x y  p1, substitute x y  p2, substitute x y  p, l)
    | SARRAY (v, t, tl, p, l) ->
       SARRAY (v, Term.substitute x y t, (fun (a,b) -> (a, Term.substitute x y b)) |>>| tl, substitute x y  p, l)
    | MALLOC (v, tl, p, l) ->
       MALLOC (v, Exp.substitute (Term.toExp x) (Term.toExp y) tl, substitute x y  p, l)
    | BLOCK (t, p, l) -> BLOCK (substitute x y  t, substitute x y  p, l)
    | DECL (a, len, init_data, t, l) as dc ->
       if a = Term.toExp x then
         dc
       else
         DECL (a, len, init_data, substitute x y t, l)
    | RETURN (i, p, l) -> RETURN (Term.substitute x y i, substitute x y p, l)
    | BREAK (p, l) -> BREAK (substitute x y p, l)
    | CONTINUE (p, l) -> CONTINUE (substitute x y p, l)
    | LABEL (l, el, p, ll) -> LABEL (l, (fun e -> if e = Term.toStr x then Term.toStr y else e) |>>| el, substitute x y p, ll)

  let max a b = if a > b then a else b

  let rec maxi = function
    | [] -> 0
    | [x] -> x
    | x::xs -> max x (maxi xs);;


  let rec has_static =
    List.exists (function
        | C.SpecStorage (C.STATIC) -> true
        | _ -> false)
             
  (** Auxiliary function for is_with_ref' and also used directly from other function. *)
  let rec is_with_ref' pointers = function
    | C.NOTHING -> 0
    | C.UNARY (unary_operator, expression) ->
       if unary_operator = C.MEMOF || unary_operator = C.ADDROF then
         is_with_ref' pointers expression
       else
         0
    | C.LABELADDR (str) -> 0
    | C.BINARY (_, expression1, expression2) ->
       max (is_with_ref' pointers expression1) (is_with_ref' pointers expression2)
    | C.QUESTION (expression1, expression2, expression3) ->
       max (is_with_ref' pointers expression1) (max (is_with_ref' pointers expression2) (is_with_ref' pointers expression3))
    | C.CAST ((specifier, decl_type), init_expression) ->
       begin
         match init_expression with
         | C.NO_INIT -> 0
         | C.SINGLE_INIT (expression) ->
            is_with_ref' pointers expression
         | C.COMPOUND_INIT _ -> 0
       end
    | C.CALL (expression, l_expression) ->
       maxi ((is_with_ref' pointers) |>>| l_expression)
    | C.COMMA (l_expression) ->
       maxi ((is_with_ref' pointers) |>>| l_expression)
    | C.CONSTANT (constant) -> -1
    | C.PAREN (expression) -> is_with_ref' pointers expression
    | C.VARIABLE ( str ) -> if (str, false) |<- pointers then 1 else 0
    | C.EXPR_SIZEOF (expression) -> is_with_ref' pointers expression
    | C.TYPE_SIZEOF (specifier, decl_type) -> 0
    | C.EXPR_ALIGNOF (expression) -> is_with_ref' pointers expression
    | C.TYPE_ALIGNOF (specifier, decl_type) -> 0
    | C.INDEX (expression1, expression2) ->  max (is_with_ref' pointers expression1) (is_with_ref' pointers expression2)
    | C.MEMBEROF (expression, str) -> is_with_ref' pointers expression
    | C.MEMBEROFPTR (expression, str) -> is_with_ref' pointers expression
    | C.GNU_BODY (block) -> 0
    | C.EXPR_PATTERN (str) -> 0 (* not used *)     (* pattern variable, and name *)

  (** Whether x contains reference/pointer variable *)
  let is_with_ref pointers x = (is_with_ref' pointers x) > 0

  (** Return type Term.t *)
  let rec exp_ref pointers _exp =
    if !nopointermode then _exp else
      match _exp with
      | C.NOTHING -> _exp
      | C.UNARY (unary_operator, expression) as exp ->
         if unary_operator = C.NOT && not (is_with_ref pointers expression) then
           true_g
         else if unary_operator = C.MEMOF then
           C.BINARY(C.NE, exp, C.CONSTANT (C.CONST_INT "0"))
         else
           C.UNARY (unary_operator, exp_ref pointers expression)
      | C.BINARY (op, expression1, expression2) ->
         begin
           let is1 = is_with_ref' pointers expression1 in
           let is2 = is_with_ref' pointers expression2 in
           match op with
           | C.OR | C.AND | C.XOR | C.EQ
             | C.NE | C.LT  | C.GT  | C.LE | C.GE ->
              if is1*is1=1 && is2*is2=1 then
                C.BINARY (op, exp_ref pointers expression1, exp_ref pointers expression2)
              else if is1=1 then
                exp_ref pointers expression1
              else if is2=1 then
                exp_ref pointers expression2
              else
                true_g
           | C.ADD | C.SUB | C.MUL | C.DIV | C.MOD | C.BAND | C.BOR | C.SHL | C.SHR | C.SHL_ASSIGN | C.SHR_ASSIGN
             | C.ADD_ASSIGN | C.SUB_ASSIGN | C.MUL_ASSIGN | C.DIV_ASSIGN | C.MOD_ASSIGN | C.BAND_ASSIGN | C.BOR_ASSIGN | C.XOR_ASSIGN ->
              if is1*is1=1 && is2*is2=1 then
                C.BINARY (op, exp_ref pointers expression1, exp_ref pointers expression2)
              else if is1=1 then
                exp_ref pointers expression1
              else if is2=1 then
                exp_ref pointers expression2
              else
                zero_g
           | C.ASSIGN -> pw "EXCEPTION OP"; C.BINARY (op, expression1, expression2)
         end
      | C.CAST ((specifier, decl_type), init_expression) ->
         begin
           match init_expression with
           | C.NO_INIT -> zero_g
           | C.SINGLE_INIT (expression) -> exp_ref pointers expression
           | C.COMPOUND_INIT _ -> zero_g
         end
      | C.CALL (expression, l_expression) ->
         C.CALL (expression, (exp_ref pointers) |>>| ((fun x -> is_with_ref pointers x) |>- l_expression))
      | C.COMMA (l_expression) ->
         C.COMMA ((exp_ref pointers) |>>| ((fun x -> is_with_ref pointers x) |>- l_expression))
      | C.CONSTANT (constant) -> _exp
      | C.PAREN (expression) -> C.PAREN (exp_ref pointers expression)
      | C.VARIABLE ( str ) ->
         zero_g
      | C.EXPR_SIZEOF (expression) ->
         zero_g
      | C.TYPE_SIZEOF (specifier, decl_type) -> C.TYPE_SIZEOF (specifier, decl_type)
      | C.EXPR_ALIGNOF (expression) ->
         zero_g
      | C.TYPE_ALIGNOF (specifier, decl_type) -> _exp
      | x -> x


    
           
  let build_dec is_fresh (v:Exp.t) dim (init : init) pr loc specs  =
   
    let v' =
      if not is_fresh && Exp.is_static v then
        (
          match v with
            Exp.VAR (name, attrs) ->
             let vname =
               if !in_func <> "" then
                 to_static ~local_static:true name
               else
                 to_static name (* make_path !funcdir (fst loc) name *) in
               let v' = __E (vname, attrs) in
            v'
          | _ -> raise (StError "Not a variable")
        )
      else
        v
    in
    
    let decl =
      if List.length dim > 0 then
        let v'' = Exp.var_add (Exp.ARRAY dim) v in
        let v''' = Exp.var_add (Exp.ARRAY dim) v' in
          
        (DECL (v''', dim, init, pr, loc), v'')
      else
        (DECL (v', dim, init, pr, loc), v)
    in
    (* if Exp.toStr v = "tp_c#_13171" then(
      Exp.print v; pn "";
      pprint 2 (fst decl); pn "";
      Exp.print (snd decl);
      raise Error); *)
    decl;; 
    
    
  let build_call proc_name params body  loc =
    let pos = new_func () in
    let proc_name' =
      match proc_name with
        Term.EXP (Exp.VAR (vn, va) as var) ->
         if Exp.is_static var then
           Term.encode (to_static vn, va)
         else
           proc_name
      | _ -> proc_name
    in
    PROCCALL (proc_name', params, pos, body, loc)


  let build_assign x e pr l =
    let x' =
      if Exp.is_void x && Term.with_head e then
        let h = Term.head "build assign" e in
        let attrs1 = __A h in
        let attrs2 = (function Exp.GLOBAL -> false
                             | Exp.PARAM -> false
                             | Exp.STATIC -> false
                             | _ -> true
                     ) |>- attrs1 in
        if Exp.is_func h then
          let f_attr = (function Exp.FUNC (a,b) -> Exp.FUNCPTR (a,b) | attr -> attr) |>>| attrs2 in
          __E (__N x, f_attr)
        else
          __E (__N x, __A h)
      else
        x
    in

    let exp =
      match e with
        Term.EXP ((Exp.VAR (vn, attr) as v)) ->
         if Exp.is_void v then
           let attrs' = Exp.get_attributes x' in
           Term.EXP (Exp.VAR (vn, attrs'))
         else
           e
      | _ -> e
    in

    let new_fv = if x=x' then [] else [x'] in
    ASSIGN (x', exp, pr, l), new_fv
    
    (*
  let build_assign x e pr l =
    let e' = match e with
        
        Term.EXP (Exp.ADDR (Exp.VAR _ as v)) ->
         if Exp.is_struct v && not (Exp.is_ptr v) then
           Term.EXP v
         else
           e
      | _ -> e
    in
    ASSIGN (x, e', pr, l)
     *)
    
  let rec join last = function
    | [] -> [], last
    | x::xs ->
       let nvs, y = join last xs in
                    (**  *)
       match x, y with
       | ASSIGN (b', d, _, l1), ASSIGN (a, b, c, l2) when b = Term.EXP b' ->
          let attr = Exp.get_attributes b' in
          let a' = Exp.set_attributes a attr in
          a'::nvs, ASSIGN (a', d, c, l1)
       | LOOKUP (b', d, e, _, l1), ASSIGN (a, b, c, l2) when b = Term.EXP b' ->
          let attr = Exp.get_attributes b' in
          let a' = Exp.set_attributes a attr in
          a'::nvs, LOOKUP (a', d, e, c, l1)
       | _ ->
          let x' =
          match x with
          | SKIP -> SKIP
          | FAIL -> FAIL
          | ASSIGN (a, b, _, l) -> ASSIGN (a, b, y, l)
          | ASSERT (a, _, l) -> ASSERT (a, y, l)
          | IF (a, b, c, _, l) -> IF (a, b, c, y, l)
          | WHILE (a, bs, b, c, _, l) -> WHILE (a, bs, b, c, y, l)
          | PROCCALL (a, b, i, _, l) -> PROCCALL (a, b, i, y, l)
          | CONS (a, b, _, l) -> CONS (a, b, y, l)
          | MUTATION (a, b, c, _, l) -> MUTATION (a, b, c, y, l)
          | LOOKUP (a, b, c, _, l) -> LOOKUP (a, b, c, y, l)
          | DISPOSE (a, _, l) -> DISPOSE (a, y, l)
          | MALLOC (a, c, _, l) -> MALLOC (a, c, y, l)
          | SARRAY (a, b, c, _, l) -> SARRAY (a, b, c, y, l)
          | PARALLEL (b, c, _, l) -> PARALLEL (b, c, y, l)
          | MAPS (a, b, _, l) -> MAPS (a, b, y, l)
          | BLOCK (b, _, l) -> BLOCK (b, y, l)
          | DECL (b, len, init_data, _, l) -> DECL (b, len, init_data, y, l)
          | RETURN (i, _, l) -> RETURN (i, y, l)
          | BREAK (_, l) -> BREAK (y, l)
          | CONTINUE (_, l) -> CONTINUE (y, l)
          | LABEL (l, el, _, ll) -> LABEL (l, el, y, ll)
          in
          nvs, x'

  let merge_lbl bs cs =
    (fun bs (c, cvs) ->
      let b1, b2 = List.partition (fun (b,_) -> b = c) bs in
      match b1 with
        [] -> (c, cvs)::b2
      | (b, bvs)::_ -> (b, bvs @@@ cvs)::b2
    ) |->> (bs, cs)
          
  let rec get_labels_in_exp = function
     Exp.BINOP (t1, _, t2) -> merge_lbl (get_labels_in_exp t1) (get_labels_in_exp t2)
   | Exp.ADDR t -> get_labels_in_exp t
   | Exp.REF t -> get_labels_in_exp t
   | Exp.NEG t -> get_labels_in_exp t
   | Exp.ARROW (t, _) -> get_labels_in_exp t
   | Exp.LBL (s, t) ->
      let fv = Exp.fv t in
      let s_fv = Exp.toStr |>>| fv in
      (s, s_fv)::get_labels_in_exp t
   | Exp.FCALL (_, tl) -> List.concat (get_labels_in_exp |>>| tl)
   | Exp.OFFSET (_, t) -> get_labels_in_exp t
   | _ -> []

  let get_labels_in_term = function
      Term.NULL -> []
    | Term.EXP exp -> get_labels_in_exp exp

  let rec get_labels_in_bexp = function
      BExp.LBL (s, t) ->
       let fv = BExp.fv t in
       let s_fv = Exp.toStr |>>| fv in
       (s, s_fv)::get_labels_in_bexp t
    | BExp.OP (b1, _, b2) -> merge_lbl (get_labels_in_bexp b1) (get_labels_in_bexp b2)
    | _ -> []
          
  let rec get_labels = function
    | SKIP -> []
    | FAIL -> []
    | ASSIGN (a, b, z, l) -> merge_lbl (get_labels_in_term b) (get_labels z)
    | ASSERT (a, z, l) -> get_labels z
    | IF (a, b, c, z, l) ->
       merge_lbl
         (merge_lbl (get_labels_in_bexp a) (get_labels b))
         (merge_lbl (get_labels c) (get_labels z))
    | WHILE (a, bs, b, c, z, l) ->
       merge_lbl (get_labels_in_bexp a) (merge_lbl (get_labels b) (get_labels z))
    | PROCCALL (a, b, i, z, l) ->
       merge_lbl (List.concat (get_labels_in_term |>>| b)) (get_labels z)
    | CONS (a, b, z, l) -> get_labels z
    | MUTATION (a, b, c, z, l) -> merge_lbl (get_labels_in_term a) (merge_lbl (get_labels_in_term c) (get_labels z))
    | LOOKUP (a, b, c, z, l) -> merge_lbl (get_labels_in_term b) (get_labels z)
    | DISPOSE (a, z, l) -> merge_lbl (get_labels_in_term a) (get_labels z)
    | MALLOC (a, tl, z, l) -> get_labels z
    | SARRAY (a, b, tl, z, l) -> get_labels z
    | PARALLEL (b, c, z, l) -> get_labels z
    | MAPS (a, b, z, l) -> get_labels z
    | BLOCK (a, z, l) -> merge_lbl (get_labels a) (get_labels z)
    | DECL (a, len, init_data, z, l) -> get_labels z
    | RETURN (i, z, l) -> merge_lbl (get_labels_in_term i) (get_labels z)
    | BREAK (z, l) -> get_labels z
    | CONTINUE (z, l) -> get_labels z
    | LABEL (l, el, z, ll) ->
       let lbls = get_labels z in
       (fun (lbl,_) -> lbl <> l ) |>- lbls

          
          
  let rec compose y  = function
    | SKIP -> y
    | FAIL -> FAIL
    | ASSIGN (a, b, z, l) -> ASSIGN (a, b, compose y z, l)
    | ASSERT (a, z, l) -> ASSERT (a, compose y z, l)
    | IF (a, b, c, z, l) -> IF (a, b, c, compose y z, l)
    | WHILE (a, bs, b, c, z, l) -> WHILE (a, bs, b, c, compose y z, l)
    | PROCCALL (a, b, i, z, l) -> PROCCALL (a, b, i, compose y z, l)
    | CONS (a, b, z, l) -> CONS (a, b, compose y z, l)
    | MUTATION (a, b, c, z, l) -> MUTATION (a, b, c, compose y z, l)
    | LOOKUP (a, b, c, z, l) -> LOOKUP (a, b, c, compose y z, l)
    | DISPOSE (a, z, l) -> DISPOSE (a, compose y z, l)
    | MALLOC (a, tl, z, l) -> MALLOC (a, tl, compose y z, l)
    | SARRAY (a, b, tl, z, l) -> SARRAY (a, b, tl, compose y z, l)
    | PARALLEL (b, c, z, l) -> PARALLEL (b, c, compose y z, l)
    | MAPS (a, b, z, l) -> MAPS (a, b, compose y z, l)
    | BLOCK (a, z, l) -> BLOCK (a, compose y z, l)
    | DECL (a, len, init_data, z, l) -> DECL (a, len, init_data, compose y z, l)
    | RETURN (i, z, l) -> RETURN (i, compose y z, l)
    | BREAK (z, l) -> BREAK (compose y z, l)
    | CONTINUE (z, l) -> CONTINUE (compose y z, l)
    | LABEL (l, el, z, ll) -> LABEL (l, el, compose y z, ll)

  let rec join_at_last last = function
    | SKIP -> last
    | FAIL -> FAIL
    | ASSIGN (a, b, y, l) -> ASSIGN (a, b, join_at_last last y, l)
    | ASSERT (a, y, l) -> ASSERT (a, join_at_last last y, l)
    | IF (a, b, c, y, l) -> IF (a, b, c, join_at_last last y, l)
    | WHILE (a, bs, b, c, y, l) -> WHILE (a, bs, b, c, join_at_last last y, l)
    | PROCCALL (a, b, i, y, l) -> PROCCALL (a, b, i, join_at_last last y, l)
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

  
                             
  let enblock map tr_statement body loc (fvs: Exp.attr list V.t) old_fvs  : t =
    let st_fvs = Exp.toStr |>>| fv tr_statement in
    let keys = fst |>>| map in
    let e_keys = Exp.toExp (packs fvs) |>>| keys in
    let v_keys = Exp.decode |>>| e_keys in
    let newvars = (fun v -> Exp.var_to_str v |<- st_fvs) |>- v_keys in

    let stmt : t = (fun stmt var ->
        let varv = Exp.be_typed fvs to_static loc (__E var) in
        if V.mem (fst var) old_fvs then
          stmt
        else 
          let decl, _ = build_dec true varv [] INIT_E stmt loc [] in
          decl
      ) |->> (tr_statement, newvars) in
    if List.length newvars > 0 then
      BLOCK (stmt, body, loc)
    else
      compose body tr_statement
    
    
  let get_type = function
    | C.Tvoid -> "void"
    | C.Tnamed str -> str
    | C.Tstruct (string1, o_l_field_group, l_attribute) -> string1
    | C.Tunion (string1, o_l_field_group, l_attribute) -> string1
    | C.Tenum (string1, o_l_field_group, l_attribute) -> "int"
    | C.Tchar -> "char"
    | C.Tbool -> "bool"
    | C.Tshort -> "short"
    | C.Tint -> "int"
    | C.Tlong -> "long"
    | C.Tint64 -> "int64"
    | C.Tfloat -> "float"
    | C.Tdouble -> "double"
    | C.Tsizet -> "sizet"
    | C.Tunsigned
    | _ -> ""                     (* GCC __typeof__ *)

  let get_spec = function
    | C.SpecType (typeSpecifier) ->
       let r = get_type typeSpecifier in
       r
    | _ -> ""

  let rec is_ptr = function
    | C.ARRAY (_, _, _) -> false
    | C.PTR (_, _) -> true
    | C.PROTO (decl_type, _, _) ->  false (* is_ptr decl_type *)
    | C.JUSTBASE -> false
    | C.PARENTYPE (_, decl_type, _) -> is_ptr decl_type
                                                     
  let rec is_proto dt = (* a proto and not a funcptr *)
    dbg "DT" "DT:" print_decl_type dt;
    let rec aux is_p = function
    | C.ARRAY (dt, _, _) -> aux is_p dt
    | C.PTR (_, dt) -> aux false dt
    | C.PROTO (dt, _, _) -> aux true dt
    | C.JUSTBASE -> is_p
    | C.PARENTYPE (_, dt, _) -> aux is_p dt
    in
    let rs = aux false dt in
    dbg "DT" "is_Proto:" pb rs;
    rs

    (*
  let rec is_fp_proto dt =
    let rec aux dt =
      match dt with
      | C.PROTO (dt, _, _) -> is_proto dt
      | C.PTR (_,dt) -> aux dt
      | C.ARRAY (dt,_,_) -> aux dt
      | C.PARENTYPE (_,dt,_) -> aux dt
      | C.JUSTBASE -> false
    in
    aux dt
     *)
    
        
  let rec is_funcptr dt = (* a funcptr and not a proto *)
    dbg "DT" "DT:" print_decl_type dt;
    let rec aux is_fp dt =
      match dt with
      | C.PARENTYPE (_,C.PTR (_, dt),_) -> aux true dt
      | C.PROTO (dt, _, _) -> aux false dt
      | C.PTR (_,dt) -> aux false dt
      | C.ARRAY (dt,_,_) -> aux is_fp dt
      | C.PARENTYPE (_,dt,_) -> aux is_fp dt
      | C.JUSTBASE -> is_fp
    in    
    let rs = aux false dt in
    dbg "DT" "is_FuncPtr:" pb rs;
    rs

  let rec is_anyway_funcptr dt = (* a funcptr and not a proto *)
    let rec aux  dt =
      match dt with
      | C.PARENTYPE (_,C.PTR (_, _),_) -> true
      | C.PROTO (dt, _, _) -> aux dt
      | C.PTR (_,dt) -> aux dt
      | C.ARRAY (dt,_,_) -> aux  dt
      | C.PARENTYPE (_,dt,_) -> aux dt
      | C.JUSTBASE -> false
    in    
    let rs = aux dt in
    dbg "DT" "is_FuncPtr:" pb rs;
    rs

  let rec is_second_order_funcptr dt = (* a funcptr and not a proto *)
    let rec aux c dt =
      match dt with
      | C.PROTO (C.PARENTYPE (_,C.PTR (_, dt),_), _, _) -> aux (c+1) dt
      | C.PROTO (dt, _, _) -> aux c dt
      | C.PTR (_,dt) -> aux 0 dt
      | C.ARRAY (dt,_,_) -> aux c dt
      | C.PARENTYPE (_,dt,_) -> aux c dt
      | C.JUSTBASE -> if c > 1 then true else false
    in    
    let rs = aux 0 dt in
    dbg "DT" "is_FuncPtr:" pb rs;
    rs


        (*
  let rec is_funcptr dt =
    let rec aux dt =
      match dt with
      | C.PROTO (C.PARENTYPE (_,C.PTR (_, _),_), _, _) -> true 
      | C.PROTO (dt, _, _) -> false
      | C.PTR (_, dt) -> aux dt
      | C.ARRAY (dt,_,_) -> aux dt
      | C.PARENTYPE (_,dt,_) -> aux dt
      | C.JUSTBASE -> false
    in
    aux dt
         *)
    
  let is_ptrptr = function
    | C.PTR (_, C.PTR(_, _)) -> true
    | _ -> false

  let get_name (str, decl_type, _, _) = str, is_ptr decl_type, is_ptrptr decl_type, is_funcptr decl_type

  let get_type_name_from_specifier specifier =
    let typename =
      let sps = ((<>) "") |>- (get_spec |>>| specifier) in
      
      if List.length sps = 0 then
        if List.exists (function C.SpecType C.Tunsigned -> true | _ -> false) specifier then
          "int"
        else
          ""
      else
        List.hd sps
    in

    (* TODO: Raise exception here to check potential errors *)
    if typename = "" then new_prog_var () else typename
  ;;

                                      
  let get_name_group (l_spec_elem, l_name) =
    let res1 = get_type_name_from_specifier l_spec_elem in

    let res2 = (get_name |>>| l_name) in
    (res1, res2)
  ;;
  

    
  let get_int e =
    match Exp.eval e with
      Exp.CONST i -> i
    | e ->
       (* pw "Note: Approximating "; Exp.pprint e; pn " to 1 in array declaration"; *)
       1

  let get_basic_type_info dt fvs  =
    let rec array_exp = function
      | C.ARRAY (C.JUSTBASE, _, exp) -> Some [exp]
      | C.ARRAY (dt, _, exp) ->
         begin
           match array_exp dt with
           | Some exps -> Some (exps @ [exp])
           | None -> Some [exp]
         end
      | C.PTR (_, dt) -> array_exp dt
      | C.PROTO (dt, _, _) -> array_exp dt
      | _ -> None
    in
    let ptr =
      if is_ptrptr dt then
        [Exp.PTRPTR]
      else
        if is_ptr dt then
          [Exp.PTR]
        else
          []
    in
     
    let arr =
      match array_exp dt with
        None -> ptr
      | Some l -> try
                  ptr @ [Exp.ARRAY (Exp.eval |>>| (Exp.toExp (packs fvs ) |>>| l))]
                with
                  e ->
                  pn "@@@@@@@@@@@@@@@";
                  raise e
    in
    
    if is_funcptr dt then
      begin
        if is_second_order_funcptr dt then
          let fp_attr1 = [Exp.FUNCPTR (arr,[])] in
          [Exp.FUNCPTR (fp_attr1,[])]
        else
          [Exp.FUNCPTR (arr,[])]
      end
        (*          arr @ [Exp.FUNCPTR ([], [])] *)
    else
      arr

  let rec size_of v =
    
    let struct_size v =
      let st_name = Exp.get_struct_name v in
       let (_, fields, _) = V.find st_name !structures in
       (fun acc (fld, _) ->
         let fld_sz = size_of fld in
         Exp.op fld_sz acc Op.ADD
       ) |->> (Exp.CONST 0, fields)
    in
    let array_size v =
      let lens : Exp.t list = Exp.get_array_length v in
      (fun acc ln ->
        Exp.op acc ln Op.MUL
      ) |->> (Exp.CONST 1, lens)
    in
    (* let rec get_aliased x =
      if Exp.is_simple_type x then
        x
      else
        if V.mem x !s_aliases then
          let y = V.find x !s_aliases in
          get_aliased y
        else
          raise (StError ("Get Aliased " ^ x))
    in *)
          
    
    match Exp.is_struct v && not (Exp.is_ptr v), Exp.is_array v with
      true, true ->
       let st_size = struct_size v in
       let ar_len = array_size v in
       Exp.op st_size ar_len Op.MUL
    | true, false ->
       let st_size = struct_size v in
       st_size
    | false, true ->
       let ar_len = array_size v in
       ar_len
    | false, false ->
       let n = Exp.size_of v in
       Exp.CONST n

  let rec cabs_size_of v =
    let struct_size v =
      let st_name = Exp.get_struct_name v in
       let (_, fields, _) = V.find st_name !structures in
       match fields with
         [] -> C.CONSTANT (C.CONST_INT "0")
       | (fld,_)::fields' ->
          let fld_sz = cabs_size_of fld in
          (fun acc (fld, _) ->
            let fld_sz = cabs_size_of fld in
            C.BINARY (C.ADD, fld_sz, acc)
          ) |->> (fld_sz, fields')
    in
    let array_size v =
      let lens : Exp.t list = Exp.get_array_length v in
      match lens with
        Exp.CONST n::lens' -> (** It should be extended to Exp.t to cabs.t *)
        (fun acc ln ->
          match ln with
            Exp.CONST n ->
             C.BINARY (C.MUL, acc, C.CONSTANT (C.CONST_INT (string_of_int n)))
          | _ -> raise (StError "array length should be constant (1) in size of")
        ) |->> (C.CONSTANT (C.CONST_INT (string_of_int n)), lens')
      | _ -> raise (StError "array length should be constant (2) in size of")
    in
    let smp_sz = if Exp.is_simple v then Exp.size_of v else 1 in

    let sz =
      match Exp.is_struct v && not (Exp.is_ptr v), Exp.is_array v with
        true, true ->
         let st_size = struct_size v in
         let ar_len = array_size v in
         C.BINARY (C.MUL, st_size, ar_len)
      | true, false ->
         let st_size = struct_size v in
         st_size
      | false, true ->
         let ar_len = array_size v in
         ar_len
      | false, false ->
         C.CONSTANT (C.CONST_INT "1")
    in
    C.BINARY (C.MUL, sz, C.CONSTANT (C.CONST_INT (string_of_int smp_sz)))
  ;;

  let get_struct_name name = ""

  let get_field_type st_name fld_name l t =
    let (s_n, s_f, _) =
      try
        V.find st_name !structures
      with
        Not_found ->
        
        V.iter (fun k v -> pn k) !structures;
        raise (StError ("Illegal Struct: " ^ fld_name ^ " of " ^ (Term.toStr t) ^ " @ " ^ st_name ^ "."  ^ (Locs.to_str l)))
    in
    
    let (fld,_) =
      try
        List.find (fun (n,_)-> Exp.toStr n = fld_name) s_f
      with
        _ ->
        (V.iter (fun _ st -> (fun (n,fs,_) -> p n; p "::"; iterS Exp.pprint ", " (fst |>>| fs); pn "") st)) !structures;
        raise (StError ("Illegal field " ^ fld_name ^ " of " ^ (Term.toStr t) ^ " @ " ^ st_name ^ "."  ^ (Locs.to_str l)))
    in
    let field_type = __A fld in
    field_type


    
  let build_lookup ?struct_name:(stn="") fvs (x:Exp.t) t' f pr l =
 
    
    let t = match t' with
        Term.EXP (Exp.ADDR e) -> Term.EXP e
      | t -> t
    in
    
    let th = try
        Term.head ("build_lookup * " ^ (Locs.to_str l)) t
      with
        w ->
        match t' with
          Term.NULL | Term.EXP Exp.NOTHING | Term.EXP (Exp.CONST 0) when stn <> "" ->
                       Exp.VAR (new_prog_var (), [])
        | _ ->
        pf_s "EXCEP" Exp.pprint x; p_s "EXCEP" "="; pf_s "EXCEP" Term.pprint t'; pn_s "EXCEP" "";
        print_string ("Cannot take head of " ); Term.pprint t'; print_endline "";
        pn (Locs.to_str l);
        raise w
    in

    let is_struct attr =
      List.exists (function Exp.STRUCT _ -> true | _ -> false) attr
    in
    let is_ptr attr =
      List.exists (function Exp.PTR -> true | _ -> false) attr
    in
    
    if f = "*" then
      
      let attr = Exp.get_attributes th in
      let attr'' = (function Exp.PTR | Exp.PTRPTR | Exp.GLOBAL | Exp.STATIC | Exp.ARRAY _ -> false | _ -> true) |>- attr in
      let is_attr_struct = is_struct attr'' in 
      let attr' = if is_attr_struct then Exp.PTR::attr'' else attr'' in
      if Exp.is_ptrptr th then
        ( let x' = Exp.set_attributes x (Exp.PTR::attr') in
          let lu = LOOKUP (x', t', f, pr, l) in
          [x'], lu)
      else(
        let x' = Exp.set_attributes x attr' in
        let lu = if is_attr_struct then
                   ASSIGN (x', t', pr, l)
                 else
                   LOOKUP (x', t', f, pr, l)
        in
        [x'], lu)
    else
      
      if Exp.is_struct th then
        begin
          (* Exp.pprint h; pn "$$$"; *)
          
          let _type = Exp.get_struct_name th in
          let _field_type = get_field_type _type f l t in
          let _field_type' = if List.exists (function Exp.ARRAY _ -> true | _ -> false) _field_type then
                               Exp.PTR::(((function Exp.ARRAY _ -> false | _ -> true)) |>- _field_type)
                             else
                               _field_type
          in
          let _field_type'' = if is_struct _field_type' && not (is_ptr _field_type') then
                                Exp.PTR::_field_type' (* (((function Exp.STRUCT _ -> false | _ -> true)) |>- _field_type') *)
                              else
                                _field_type'
          in
          let x' = Exp.set_attributes x _field_type'' in
          (* Exp.pprint x'; pn "@@"; *)
          [x'], LOOKUP (x', t, f, pr, l)
        end
      else
        begin
          Term.print t'; pn "";
          raise (StError ((Term.toStr t) ^ " is not a struct while accessing field " ^ f ^ " @ " ^ (Locs.to_str l)))
        end

  let get_array_exp fvs loc exp =
    let rec array_exp = function
      | C.ARRAY (C.JUSTBASE, _, C.NOTHING) ->
         Some [Exp.NOTHING]
      | C.ARRAY (dt, _, C.NOTHING) ->
         let res : Exp.t list option = array_exp dt in
         let sz' = match res with
             Some x -> Exp.NOTHING::x
           | _ -> [Exp.NOTHING]
         in
         Some sz'
      | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.VARIABLE e))
        | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.PAREN(C.VARIABLE e))) ->
         
         let v'= Exp.VAR (e, []) in
         let v = Exp.be_typed fvs to_static loc v' in
         let sz : Exp.t = Exp.eval (Exp.toExp (packs fvs ) @@ cabs_size_of v) in
         let res : Exp.t list option = array_exp dt in
         let sz' = match res with
             Some x -> sz::x
           | _ -> [sz]
         in
         Some sz'
      | C.ARRAY (C.JUSTBASE, _, exp) ->
         let exp' = Exp.eval @@ Exp.toExp (packs fvs ) exp in
         Some [exp']
      | C.ARRAY (dt, _, ((C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX  (C.PAREN C.VARIABLE vvv', _)))) as ee))
        | C.ARRAY (dt, _, ((C.PAREN (C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX (C.PAREN C.VARIABLE vvv', _))))) as ee)) when vvv=vvv' ->
         begin
           if V.mem vvv fvs then
             let v'= Exp.VAR (vvv, []) in
             let v = Exp.be_typed fvs to_static loc v' in
             if Exp.is_array v then
               Some (Exp.get_array_length v)
             else
               let sz = size_of v in
               Some [sz]
           else
             let e' = Exp.toExp (packs fvs ) ee in
             let e'' = Exp.eval e' in
             Some [e'']
         end
      | C.ARRAY (dt, _, exp) -> 
         begin
           let exp' = Exp.eval @@ Exp.toExp (packs fvs ) exp in
           match array_exp dt with
           | Some exps -> Some (exps @ [exp'])
           | None -> Some ([exp'])
         end
      | C.PTR (_, dc) -> array_exp dc
      | C.PROTO (dc, _,_) -> array_exp dc
      | C.JUSTBASE -> None
      | _ -> (* raise Error pn ("@@@@@@@@@@ 3"); *) None
    in
    array_exp exp

  let rec has_static =
    List.exists (function
    | C.SpecStorage (C.STATIC) -> true
    | _ -> false)
      
  let build_attributes_from_specs glob pointers l_spec =
    let type_name = get_type_name_from_specifier l_spec in
    
    let glob_attr =
      if glob then
        [Exp.GLOBAL]
      else
        []
    in
    (* V.iter (fun v k -> pn v) !array_aliases;
    pn "====="; *)
    let static_attr = if has_static l_spec then [Exp.STATIC] else [] in
    let attrs =
      if V.mem type_name !structures && not (type_name |<- !enums) then
        [Exp.STRUCT (get_original_type type_name)]
      else if (type_name, true) |<- pointers then
        [Exp.PTR]
      else if is_func_ptr_type type_name then
        let attr = get_func_ptr_attr type_name in
        attr (** TODO: Need attrs to extract from typedef *)
      else if V.mem type_name !array_aliases then
        [Exp.ARRAY (snd @@ V.find type_name !array_aliases)]
      else
        let type_name' = get_original_type type_name in
                 if Exp.is_simple_type type_name' then
          [Exp.SIMPLE (Exp.simple_size type_name')]
        else
          []
    in
    (* Exp.print (Exp.VAR ("", attrs)); *)
    glob_attr @@@ attrs @@@ static_attr


    
  let build_attributes_from_dt name fvs  loc dt =
    (** ARRAY *)
    let array_length_dt : Exp.t list option = get_array_exp fvs  loc dt in
    dbg "INIT" "Array Len:" (op_p (iterS Exp.pprint "-")) array_length_dt ; 
    let array_attr =
      match array_length_dt with
        None -> []
      | Some l_exp -> [Exp.ARRAY l_exp]
    in
    (** PTRPTR *)
    let ptrptr_attr =
      if is_ptrptr dt then
        [Exp.PTRPTR]
      else
        []
    in
    (** PTR *)
    let ptr_attr =
      if is_ptr dt then
        [Exp.PTR]
      else
        []
    in
    (** FUNC_PTR 
       int ( *fp)();
     *)
    let fp_attr, is_fp =
      if is_funcptr dt then
        begin
          func_ptrs := name::!func_ptrs;
          if is_second_order_funcptr dt then
            let fp_attr1 = [Exp.FUNCPTR (ptr_attr @ ptrptr_attr,[])] in
            [Exp.FUNCPTR (fp_attr1,[])], true
          else
            [Exp.FUNCPTR (ptr_attr @ ptrptr_attr, [])], true (** special change neede by km_fp3 *)
        end
      else
        [], false
    in
    let proto_attr, is_pr =
      
      if is_proto dt then(
        if is_anyway_funcptr dt then
            let fp_attr = [Exp.FUNCPTR (ptr_attr @ ptrptr_attr @ array_attr,[])] in
            [Exp.FUNC (fp_attr,[])], true
          else
            [Exp.FUNC (ptr_attr @ ptrptr_attr @ array_attr,[])], true
        
      )
      else
        [], false
    in
    let all =
      if is_fp then
        fp_attr @ array_attr
      else if is_pr then
        proto_attr
      else
        array_attr @ ptrptr_attr @ ptr_attr @ fp_attr @ proto_attr
    in
    (* Exp.print (Exp.VAR (name, all)); pn ""; (* ------ *) *)
    all

  let build_attributes ?is_param:(param=false) glob name pointers fvs  (loc : C.cabsloc) dt l_spec =
    
    let attrs1' = build_attributes_from_dt name fvs  loc dt in
    let attrs2' = build_attributes_from_specs glob pointers l_spec in
    
    (** FUNC + PTR -> PUNCPTR *)
    let ptr_attr, ptr'_attr = List.partition (function Exp.PTR -> true | _ -> false) attrs1' in
    let func_attr, func'_attr = List.partition (function Exp.FUNC _ -> true | _ -> false) attrs2' in
    let attrs1, attrs2 = 
      match func_attr, ptr_attr with
        Exp.FUNC (a,b)::_, Exp.PTR::_ ->
        Exp.FUNCPTR (a,b)::ptr'_attr, func'_attr
      | _ ->
         attrs1', attrs2'
    in

    (*
    (* --- *)
    Exp.print (Exp.VAR (name, attrs1)); pn " . dt"; 
    Exp.print (Exp.VAR (name, attrs2)); pn " . spec";
     *)
    
    (** ARRRAY xs + ARRAY ys -> ARRAY (ys @ xs) *)
    let arr_attr1, other_attrs1 = List.partition (function Exp.ARRAY _ -> true | _ -> false) attrs1 in
    let arr_attr2, other_attrs2 = List.partition (function Exp.ARRAY _ -> true | _ -> false) attrs2 in
    let arr_attr =
      match arr_attr1, arr_attr2 with
        Exp.ARRAY l_exp1::_, Exp.ARRAY l_exp2::_ -> [Exp.ARRAY (l_exp1@l_exp2)]
      | _, [] -> arr_attr1
      | _, _ ->  arr_attr2
    in

    (** STRUCT|SIMPLE|FUNCPTR + FUNC|FUNCPTR -> FUNC(STRUCT|SIMPLE|FUNCPTR)|FUNCPTR(STRUCT|SIMPLE|FUNCPTR) *)
    let st_attr, st'_attr = List.partition (function Exp.STRUCT _ -> true
                                                   | Exp.SIMPLE _ -> true
                                                   | Exp.FUNCPTR _ -> true
                                                   | _ -> false) other_attrs2 in
    let fp_attr, fp'_attr = List.partition (function Exp.FUNCPTR _ -> true | _ -> false) other_attrs1 in
    let pr_attr, pr'_attr = List.partition (function Exp.FUNC (ret_attr,_) -> true | _ -> false) fp'_attr in
    let param_attr = if param then [Exp.PARAM] else [] in
    match st_attr with
      st::_ ->
       (* let is_bp = List.length fp_attr > 0 in
       pw name; pb is_bp; pn ""; *)
       let attr =
         begin
           match fp_attr with
             Exp.FUNCPTR (ret_attr1,params1)::_ ->
              begin
                let fp2_attr, fp2'_attr = List.partition (function Exp.FUNCPTR _ -> true | _ -> false) ret_attr1 in
                   match fp2_attr with
                     Exp.FUNCPTR (ret_attr2,params2)::_ ->
                      [Exp.FUNCPTR ([Exp.FUNCPTR (st::ret_attr2,params2)]@fp2'_attr, params2)] 
                   | _ ->
                      [Exp.FUNCPTR (st::ret_attr1,params1)]
              end
           | _ ->
              match pr_attr with
                Exp.FUNC (ret_attr1, params1)::_ ->
                 begin
                   let fp2_attr, fp2'_attr = List.partition (function Exp.FUNCPTR _ -> true | _ -> false) ret_attr1 in
                   match fp2_attr with
                     Exp.FUNCPTR (ret_attr2,params2)::_ ->
                      [Exp.FUNC ([Exp.FUNCPTR (st::ret_attr2,params2)]@fp2'_attr, params2)] 
                   | _ ->
                      [Exp.FUNC (st::ret_attr1, params1)]
                 end
              | _ ->
                 st_attr
         end
       in
       
       let all = attr @ pr'_attr @ arr_attr @ st'_attr @ param_attr in
       
       (* Exp.print (Exp.VAR (name, all)); pn "  :: Final"; (* --- *) *)
       (* if name = "string_list_append" then
         raise Error; *)
       all
    | _ -> 
       let all = other_attrs1 @@@ other_attrs2 @ arr_attr @ param_attr in
       (* Exp.print (Exp.VAR ("", all)); pn "  :: Final"; *)
       all
       
  let get_array_dim_from_init_name is_global fvs  loc (init_name : C.init_name) specifier attrs type_name arrays_lens =
    let rec init_level = function
      | C.NO_INIT -> -1
      | C.SINGLE_INIT _ -> 0
      | C.COMPOUND_INIT el ->
         1 + (init_level @@ snd (List.hd el))
    in
    let rec get_array_length = function
      | C.NO_INIT -> []
      | C.SINGLE_INIT (C.CONSTANT (C.CONST_STRING s)) -> [String.length s + 1]
      | C.SINGLE_INIT e -> []
      | C.COMPOUND_INIT el ->
         let n = List.length el in
         let lens = (fun (_,e) ->
             get_array_length e) |>>| el in
         let max_len = (fun xs ys ->   
             match xs, ys with
               x1::_, y1::_ -> if x1>=y1 then xs else ys
             | _, _ -> []) |->> (List.hd lens, List.tl lens)
         in
         n::max_len
    in

    let aux ((name, dt, _, _), (exp : C.init_expression)) =
      let arr_attr, non_array_attr = List.partition (function Exp.ARRAY _ -> true | _ -> false) attrs in
      
      let array_length_dt =
        match arr_attr with
          Exp.ARRAY l_exp::_ -> Some l_exp
        | _ ->  None
      in
      let i_level = init_level exp in
      dbg "INIT" "i-level" pl i_level;
      
      let array_dim : Exp.t list =
        match array_length_dt with
          Some l_arr_len ->
           let l_arr_len1 = get_array_length exp in
           let rec aux xs ys =
             match xs, ys with
               Exp.NOTHING::xs', y::ys' ->
                Exp.CONST y::aux xs' ys'
             | x::xs', _::ys' ->
                x::aux xs' ys'
             | [], _ -> []
             | _, _ -> xs
           in
           (aux l_arr_len l_arr_len1)
        | None ->
           []
      in
      let is_pointer = List.exists (fun at -> at |<- [Exp.PTRPTR; Exp.PTR]) non_array_attr in
      let non_array_attr'' : Exp.attr list = if is_pointer then [Exp.PTR]@@@non_array_attr else non_array_attr in
      (* let non_array_attr'' =
        if is_func_ptr_type type_name then
          let attr = get_func_ptr_attr type_name in
          attr @@ non_array_attr'
        else
          non_array_attr'
      in (** TODO: Need to extract attrs from typedef *) *)
      let type_name' = get_original_type type_name in
      let non_array_attr''' =
        if Exp.is_simple_type type_name' && not (List.exists (function Exp.SIMPLE _ -> true | _ -> false) non_array_attr'') then
          Exp.SIMPLE (Exp.simple_size type_name')::non_array_attr''
        else
          non_array_attr''
      in
      
      let array_dim' =
        if V.mem type_name !array_aliases then
            let (_, lens) = V.find type_name !array_aliases in
            array_dim @ lens
          else
            array_dim
      in
      let len  =
          if List.length arrays_lens < List.length array_dim' then
            try
              take (List.length arrays_lens) array_dim'
            with
              _ -> []
          else
            array_dim'
      in
      
      (non_array_attr''', is_pointer, len)
    in
    aux init_name
;;
       
    
  let get_var_name_from_init_name is_global fvs  loc (l_init_name : C.init_name list) specifier =
    let rec init_level = function
      | C.NO_INIT -> -1
      | C.SINGLE_INIT _ -> 0
      | C.COMPOUND_INIT el ->
         1 + (init_level @@ snd (List.hd el))
    in
    let rec get_array_length = function
      | C.NO_INIT -> []
      | C.SINGLE_INIT (C.CONSTANT (C.CONST_STRING s)) -> [String.length s + 1]
      | C.SINGLE_INIT e -> []
      | C.COMPOUND_INIT el ->
         let n = List.length el in
         let lens = (fun (_,e) ->
             get_array_length e) |>>| el in
         let max_len = (fun xs ys ->   
             match xs, ys with
               x1::_, y1::_ -> if x1>=y1 then xs else ys
             | _, _ -> []) |->> (List.hd lens, List.tl lens)
         in
         n::max_len
    in

    let aux ((name, dt, _, _), (exp : C.init_expression)) =
      let attrs = build_attributes is_global name [] fvs  loc dt specifier in
      let arr_attr, other_attr = List.partition (function Exp.ARRAY _ -> true | _ -> false) attrs in
      let array_length_dt =
        match arr_attr with
          Exp.ARRAY l_exp::_ -> Some l_exp
        | _ ->  None
      in
      let i_level = init_level exp in
      dbg "INIT" "i-level" pl i_level;
      
      let array_length : Exp.t list option =
        match array_length_dt with
          Some l_arr_len ->
           let l_arr_len1 = get_array_length exp in
           let rec aux xs ys =
             match xs, ys with
               Exp.NOTHING::xs', y::ys' ->
                Exp.CONST y::aux xs' ys'
             | x::xs', _::ys' ->
                x::aux xs' ys'
             | [], _ -> []
             | _, _ -> xs
           in
           Some (aux l_arr_len l_arr_len1)
        | None ->
           None
      in
      let name' = __E (name, other_attr) in
      let ip = List.exists (fun at -> at |<- [Exp.PTRPTR; Exp.PTR]) other_attr in      
      (name', ip, exp, array_length)
    in
    aux |>>| l_init_name

    
  let tryit m x =
    
    let r = x in
    
    r
  ;;
    
  let rec get_var_from_exp pointers = function
    | C.VARIABLE ( str ) -> str (* if List.exists (fun (x,b) -> not b && x = str ) pointers then String.cat str "[*]" else str *)
    | C.UNARY (C.MEMOF, exp) -> (get_var_from_exp pointers exp) ^ "_(0)"
    | C.UNARY (op, exp) -> (t_u_op op) ^ (get_var_from_exp pointers exp)
    | C.INDEX (exp1, exp2) ->
       String.concat "_" [(get_var_from_exp pointers exp1); (get_var_from_exp pointers exp2)]
    | C.BINARY (op, exp1, exp2) ->
       get_var_from_exp pointers exp1
    (* String.concat "" [(get_var_from_exp pointers exp1);(t_b_op op);(get_var_from_exp pointers exp2)] *)
    | C.CONSTANT (C.CONST_INT (str)) -> str
    | C.CONSTANT (C.CONST_FLOAT (str)) -> str
    | C.CONSTANT (C.CONST_CHAR (str)) -> String.concat "" (Int64.to_string |>>| str)
    | C.CONSTANT (C.CONST_WCHAR (str)) -> String.concat "" (Int64.to_string |>>| str)
    | C.CONSTANT (C.CONST_STRING (str)) -> String.concat "" ["\""; str; "\""]
    | C.CONSTANT (C.CONST_WSTRING (str)) -> String.concat "" (Int64.to_string |>>| str)
    | C.PAREN (exp) -> (get_var_from_exp pointers exp)
    | C.MEMBEROF (exp, str) -> (get_var_from_exp pointers exp) ^ "." ^ str
    | C.MEMBEROFPTR (exp, str) -> (get_var_from_exp pointers exp) ^ "." ^ str
    | C.CAST (_, init_expression) -> "{cast}"
    | C.CALL _ -> "{call}"
    | _ -> "<UNKNOWN EXP>"


      
  let build_mutation ptr fld exp' body loc =
    let ptr' = Term.eval ptr in
    
    if fld <> "*" then
      let th = Term.head "build_mutation" ptr in
      let _type = Exp.get_struct_name th in    

      let exp =
        match exp' with
          Term.EXP ((Exp.VAR (vn, attr) as v)) ->
           if Exp.is_void v then
             let attrs' = get_field_type _type fld loc ptr in
             Term.EXP (Exp.VAR (vn, attrs'))
           else
             exp'
        | _ -> exp'
      in
      if _type |<- !unions then
        let (_, all_fields, _) = V.find _type !structures in
        (fun acc fld -> MUTATION (ptr', __N @@ fst fld, exp, acc, loc)) |->> (body, all_fields)
      else
        MUTATION (ptr', fld, exp, body, loc)
    else
      let exp =
        match exp' with
          Term.EXP ((Exp.VAR (vn, _) as v)) ->
           if Exp.is_void v && Term.with_head ptr then
             let ptr_v = Term.head "build mutation" ptr in
             let attr = Exp.get_attributes ptr_v in
             let attr' = ((<>) Exp.PTR) |>- attr in
             Term.EXP (Exp.VAR (vn, attr'))
           else
             exp'
        | _ -> exp'
      in
      MUTATION (ptr', fld, exp, body, loc)
  ;;
  
  let rec fc_get_statement pointers body loc fvs  = function
    | C.NOTHING -> body, []
    (** x++, ++x, x--, --x *)
    | C.UNARY (unary_operator, expression) ->
       let expression' = Exp.be_typed fvs to_static loc (Exp.toExp (packs fvs ) expression) in

       let res = begin
         let str = tryit "@@@5" @@ get_var_from_exp pointers expression in
         let x = Exp.be_typed fvs to_static loc (Exp.string_to_var str) in
         match unary_operator with
         | C.PREINCR | C.POSINCR ->
            ASSIGN (x, Term.EXP (Exp.BINOP (expression', Op.ADD, Exp.CONST 1)), body, loc)
         | C.PREDECR | C.POSDECR ->
            ASSIGN (x, Term.EXP (Exp.BINOP (expression', Op.SUB, Exp.CONST 1)), body, loc)
         | _ -> body
         end in
       res, []
    (** x op y *)
    | C.BINARY (binary_operator, expression1, expression2) ->
       begin
         let t_expression2 = Term.toTerm (packs fvs ) expression2 in
         let expression2' = Term.be_typed fvs to_static loc t_expression2 in
         (** ALERT: binary_operator is assumed to be only C.EQ *)
         (** Excluded cases : ( *(x) ) += y; x[i][j] += y; A.f += y; etc. *)
         (** MAY NEED TO REPAIR *)
         
         match expression1 with
         (** A.f ? ? *)
         | C.MEMBEROF (expression, str) ->
            let texp' = (Term.toTerm (packs fvs) expression) in
            let texp = Term.be_typed fvs to_static loc texp' in
            let v = Term.decode texp in
            if !Options.is_old || Exp.is_ptr (__E v) then
              try
                build_mutation texp str expression2' body loc, []
              with
                e ->
                
                pn "@@@@1";
                raise e
            else
              let st = (Term.toStr texp) ^ "_" ^ str in
              (* ASSIGN (__E(st, []), expression2', body, loc) *)
              let assn', new_fvs =
                try
                  build_assign (__E(st, [])) expression2' body loc
                with
                e ->
                pn "@@@@2";
                raise e
              in
              assn', new_fvs

         (** A->f ? ? *)
         | C.MEMBEROFPTR (expression, str) ->
            begin
              let texp = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) expression) in
              try
                build_mutation texp str expression2' body loc, []
             with
                e ->
                pn "@@@@3";
                raise e
            end
         (** **x ? ? Exp.PTRPTR?? *)
         (** *x ? ? *)
         | C.UNARY (C.MEMOF, exp) ->
            let exp' = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) exp) in
            let nv = (new_prog_var (), [Exp.EXQ;Exp.PTR]) in
            let tnv = Term.encode nv in
            begin
              match expression2 with
              (** *x ? *y : lookup followed by mutation *)
              | C.UNARY (C.MEMOF, exp2) ->
                 let p2 = MUTATION (exp', "*", tnv, body, loc) in
                 let exp2' = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) exp2) in
                 LOOKUP (__E nv, exp2', "*", p2, loc), []
              (** *x ? expression2 :  *)
              | _ ->
                 begin
                   try
                     build_mutation exp' "*" expression2' body loc, []
                   with
                     e ->
                     pn "@@@@4";
                     raise e
                 end
                                (* (** Possibly wrong *) LOOKUP (nv, Term.be_typed fvs loc (Term.toTerm exp), "*", body, loc) *)
            end
         (** x[i][j] ? ? *)
         | C.INDEX (expression11, expression12) ->
            begin
              (* a[i][j][k] = b
                 ==
                 t1 = a[i]
                 t2 = t1[j]
                 t2[k] = b
               *)
              
              
              let rec aux = function
                  C.INDEX (e1, e2) ->
                   
                   let (stmts, exp) = aux e1 in
                   let nv = new_prog_var () in
                   let nv_e = Exp.VAR (nv, [Exp.PTR]) in
                   let nv_c = C.VARIABLE nv in
                   let ptr = C.BINARY (C.ADD, exp, e2) in
                   let ptr_t = Term.be_typed fvs to_static loc @@ Term.toTerm (packs fvs ) ptr in
                   
                   let dc, _ = build_dec true nv_e [] INIT_E SKIP loc []  in 
                   let _, st =
                       build_lookup fvs nv_e ptr_t "*" SKIP loc
                     
                   in
                   (dc::st::stmts, nv_c)
                | e ->
                   ([], e)
              in
              
              
              let (stmts, e) = aux expression11 in
              
              let ptr = C.BINARY (C.ADD, e, expression12) in
              let ptr_t = Term.be_typed fvs to_static loc @@ Term.toTerm (packs fvs ) ptr in

              let exp2 = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) expression2) in
              let st =
                try
                  build_mutation ptr_t "*" exp2 SKIP loc
                with
                  e ->
                  pn "@@@@5";
                  raise e
              in
              BLOCK ((snd @@ join st stmts), body, loc), []
            end
         (** ? ? ? *)
         | _ ->
            
            let sexp = tryit "@@@4" @@ get_var_from_exp pointers expression1 in
            let lexp = List.filter (fun (_, _, vs) -> List.exists (fun v -> Exp.toStr v == sexp) vs) !ptr_structs in
            let exp =
              match lexp with
                [] -> Exp.string_to_var sexp
              | (_, _, exp')::_ ->
                 try
                    List.hd exp'
                 with
                   _ -> raise (StError ("fc_get_statement"))
            in
            let exp = Exp.be_typed fvs to_static loc exp in
            
            let f x =
              let assn, new_fvs = build_assign exp (Term.be_typed fvs to_static loc (Term.EXP (Exp.BINOP (Exp.toExp (packs fvs ) expression1, x, Exp.toExp (packs fvs ) expression2)))) body loc in
              assn, new_fvs
                      in

          
            match binary_operator with
            (** ? = ? *)
            | C.ASSIGN ->
               begin
                 
                 match expression2 with
                 (** ? = &x *)
                 (** Missing: A.f = &x *)
                 | C.UNARY (C.ADDROF, C.MEMBEROFPTR (_, _)) ->
                    ASSIGN (exp, expression2', body, loc), []
                 | C.UNARY (C.ADDROF, exp2) ->
                    
                    let exp2 = Exp.be_typed fvs to_static loc (Exp.string_to_var (tryit "@@@3" @@ get_var_from_exp pointers exp2)) in
                    let exp' = if Exp.is_void exp then
                                 __E (__N exp, __A exp)
                               else
                                 __E (__N exp, [Exp.SIMPLE 4])
                    in

                    (* MUTATION (expression1, "*", expression2, body, loc) *)
                    (* LOOKUP (exp, Term.toTerm exp2, "&", body, loc) *)
                    
                    ASSIGN (exp', Term.EXP (Exp.ADDR exp2), body, loc), []
                 (* MAPS (exp, exp2, body, loc) *)
                 (** ? = *x *)
                 | C.UNARY (C.MEMOF, exp2) ->
                    LOOKUP (exp, Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) exp2), "*", body, loc), []
                 (* TO REVIEW *)
                 (* MAPS (exp, exp2, body, loc) *)
                 (* MUTATION (exp, __V "points", Term.toTerm exp2, body ) *)
                 (** ? = ? *)
                 | _ ->
                    begin
                      let rhs = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) expression2) in
                      let assn, new_fvs = build_assign exp rhs body loc in
                      assn, new_fvs
                    end
                      (* ASSIGN (exp, rhs), body, loc) *)
               end
            | C.ADD_ASSIGN -> f Op.ADD
            | C.SUB_ASSIGN -> f Op.SUB
            | C.MUL_ASSIGN -> f Op.MUL
            | C.DIV_ASSIGN -> f Op.DIV
            | C.MOD_ASSIGN -> f Op.MOD
            | C.SHL_ASSIGN -> f Op.SHL
            | C.SHR_ASSIGN -> f Op.SHR
            | C.BAND_ASSIGN -> f Op.BAND
            | C.BOR_ASSIGN -> f Op.BOR
            | C.XOR_ASSIGN -> ASSIGN (exp, Term.encode (new_prog_var (), [Exp.EXQ]), body, loc) (* CAUTION *), []
            | _ -> body, []
            
       end
    | C.PAREN (expression) -> fc_get_statement pointers body loc fvs  expression
    | C.COMMA (l_expression) ->
       begin
         match l_expression with
         | [] -> body, []
         | (x::xs) ->
            let body_xs, fvs' = fc_get_statement pointers body loc fvs  (C.COMMA xs) in
            let myp, fvs'' = fc_get_statement pointers body_xs loc fvs  x in
            myp, fvs'@fvs''
       end
    | C.CALL (expression1, expression2) ->
       begin
         let expression1' = Exp.be_typed fvs to_static loc (Exp.toExp (packs fvs ) expression1) in
         let proc_name = Term.EXP expression1' in
         let params = (fun x -> Term.be_typed fvs  to_static loc (Term.toTerm (packs fvs ) x)) |>>| expression2 in
         build_call proc_name params SKIP  loc, []
                    (* PROCCALL (proc_name, params , new_func (), SKIP, loc) *)
       end
    | _ -> body, []

  let rec get_expression fvs  = function
    | C.NO_INIT -> Term.NULL
    | C.SINGLE_INIT expression -> Term.toTerm (packs fvs ) expression
    | C.COMPOUND_INIT [] -> Term.NULL
    | C.COMPOUND_INIT ((_, init_expression)::_) ->
       get_expression fvs  init_expression

  let rec is_return_exists = function
    | C.BLOCK (block, _) ->  List.exists is_return_exists block.C.bstmts
    | C.SEQUENCE (statement1, statement2, _) -> (is_return_exists statement1) || (is_return_exists statement2)
    | C.IF (expression, statement1, statement2, _) -> (is_return_exists statement1) || (is_return_exists statement2)
    | C.WHILE (formula, expression, statement, _) -> is_return_exists statement
    | C.DOWHILE (formula, expression, statement, _) -> is_return_exists statement
    | C.FOR (formula, for_clause, expression1, expression2, statement, _) -> is_return_exists statement
    | C.RETURN (expression, _) -> true
    | C.SWITCH (expression, statement, _) -> is_return_exists statement
    | C.CASE (expression, statement, _) -> is_return_exists statement
    | _ -> false

  (*
  let rec does_rbc_exists = function
    | SKIP -> (SKIP, (false, false, false))
    | ASSERT (a, p, l) ->
       let (p', res) = does_rbc_exists p in
       (ASSERT (a, p', l), res)
    | ASSIGN (x, t, p, l) ->
       let (p', res) = does_rbc_exists p in
       (ASSIGN (x, t, p', l), res)
    | IF (b, p1, p2, p, l) ->
       let (p1', (r11,r12,r13)) = does_rbc_exists p1 in
       let (p2', (r21,r22,r23)) = does_rbc_exists p2 in
       let (p',  (r31,r32,r33)) = does_rbc_exists p in
       (IF (b, p1', p2', p', l), (r11||r21||r31, r12||r22||r32, r13||r23||r33))
    | WHILE (b, p1, a, p, l) ->
       let (p1', res) = does_rbc_exists p1 in
       let (p', res) = does_rbc_exists p in
       (WHILE (b, p1', a, p', l), res)
    | PROCCALL (fn, ps, p, l) ->
       let (p', res) = does_rbc_exists p in
       (PROCCALL (fn, ps, p', l), res)
    | CONS (e, vs, p, l) ->
       let (p', res) = does_rbc_exists p in
       (CONS (e, vs, p', l), res)
    | MUTATION (t1, fn, t2, p, l) ->
       let (p', res) = does_rbc_exists p in
       (MUTATION (t1, fn, t2, p', l), res)
    | LOOKUP (v, t1, t2, p, l) ->
       let (p', res) = does_rbc_exists p in
       (LOOKUP (v, t1, t2, p', l), res)
    | DISPOSE (t, p, l) ->
       let (p', res) = does_rbc_exists p in
       (DISPOSE (t, p, l), res)
    | MAPS (v1, v2, p, l) ->
       let (p', res) = does_rbc_exists p in
       (MAPS (v1, v2, p', l), res)
    | PARALLEL (p1, p2, p, l) ->
       let (p', res) = does_rbc_exists p in
       (PARALLEL (p1, p2, p', l), res)
    | SARRAY (v, t, tl, p, l) ->
       let (p', res) = does_rbc_exists p in
       (SARRAY (v, t, tl, p', l), res)
    | ARRAY (v, t, tl, p, l) ->
       let (p', res) = does_rbc_exists p in
       (ARRAY (v, t, tl, p', l), res)
    | BLOCK (t, p, l) ->
       let (p', res) = does_rbc_exists p in
       (BLOCK (t, p', l), res)
    | DECL (v, len, p, l) ->
       let (p', res) = does_rbc_exists p in
       (DECL (v, len, p', l), res)
    | RETURN (i, p, l) ->
       (* let (p', (r_ret, r_brk, r_cont)) = does_rbc_exists p in *)
       (SKIP, (true, false, false))
(*    | BREAK (b, p, l) ->
       (SKIP, (false, true, false)) *)
    | CONTINUE (p, l) ->
       (SKIP, (false, false, true))
    | LABEL (l, p) ->
       ()
   *)


         
  let rec is_break_exists = function
    | C.BLOCK (block, _) ->  List.exists is_break_exists block.C.bstmts
    | C.SEQUENCE (statement1, statement2, _) -> (is_break_exists statement1) || (is_break_exists statement2)
    | C.IF (expression, statement1, statement2, _) -> (is_break_exists statement1) || (is_break_exists statement2)
    | C.WHILE (formula, expression, statement, _) -> false (* is_break_exists statement *)
    | C.DOWHILE (formula, expression, statement, _) -> false (* is_break_exists statement *)
    | C.FOR (formula, for_clause, expression1, expression2, statement, _) -> false (* is_break_exists statement *)
    | C.BREAK (_) -> true
    | C.SWITCH (expression, statement, _) -> is_break_exists statement
    | C.CASE (expression, statement, _) -> is_break_exists statement
    | _ -> false

  let rec is_continue_exists = function
    | C.BLOCK (block, _) ->  List.exists is_continue_exists block.C.bstmts
    | C.SEQUENCE (statement1, statement2, _) -> (is_continue_exists statement1) || (is_continue_exists statement2)
    | C.IF (expression, statement1, statement2, _) -> (is_continue_exists statement1) || (is_continue_exists statement2)
    | C.WHILE (formula, expression, statement, _) -> false (* is_continue_exists statement *)
    | C.DOWHILE (formula, expression, statement, _) -> false (* is_continue_exists statement *)
    | C.FOR (formula, for_clause, expression1, expression2, statement, _) -> false (* is_continue_exists statement *)
    | C.CONTINUE (_) -> true
    | C.SWITCH (expression, statement, _) -> is_continue_exists statement
    | C.CASE (expression, statement, _) -> is_continue_exists statement
    | _ -> false
         
  (** It will extract array cell, function call, memory, inner increament, decrement *)

  let print_map map =
    if List.length map > 0 then
      List.iter (fun (v,t) -> Cprint.print_expression (C.BINARY (C.ASSIGN, v, t)); print_string ",") map
    else
      print_string "Empty Map";
    print_string "\n"

  let update_fvs fvs1 fvs2 =
    V.fold (fun k v a -> V.add k v a) fvs1 fvs2

    
  let add_fv ?op:(optional=false) fvs v =
    let (x, attrs) = __V v in
    let fvs' =
      if optional && V.mem x fvs && List.length (V.find x fvs) > 0 then(
        fvs
      )
      else(    
        V.add x attrs (V.remove x fvs)
      )
    in
    fvs'

  let add_fvs ?op:(optional=false) fvs nvs =
    let fvs' = (add_fv ~op:optional) |->> (fvs, nvs) in
    fvs'    
    
  let build_if l r =
    match r with
      None -> []
    | Some r1 ->
       List.concat ((fun (exp, v) ->
           match v with
             C.VARIABLE sv ->
              [C.DEFINITION (C.DECDEF (([C.SpecType C.Tint], [((sv, C.JUSTBASE, [], l), C.NO_INIT)]), l));
               C.IF (exp,
                     C.COMPUTATION (C.BINARY (C.ASSIGN, v, C.CONSTANT (C.CONST_INT "1")), l),
                     C.COMPUTATION (C.BINARY (C.ASSIGN, v, C.CONSTANT (C.CONST_INT "0")), l),
                     l
              )]
           | _ -> []
         ) |>>| r1)     
      
  let rec get_assignments ?struct_name:(stn="") is_if l fvs  = function
    | [] -> (fvs, [])
    | (k', var)::xs ->
       let loc = Locs.to_loc l in
       let (fvs, xs') : ((Exp.attr list) V.t * t list) = get_assignments is_if l fvs  xs in

       let rec to_assignment ?is_cast:(cast=false) k' var fvs =
         let v' = Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) var) in
         dbg "GETASSIGN" "Translate " (fun (k', var) -> Cprint.print_expression (C.BINARY (C.ASSIGN, k', var))) (k', var);
         
         match k' with
         | C.UNARY (C.MEMOF, pointer) ->
            let pointer' = Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) pointer) in
            let res =
              try
                build_mutation pointer' "*" v' SKIP loc
              with
                e ->
                pn "@@@@6";
                raise e
            in
            [], [res], xs'
            
         | C.MEMBEROF (pointer, field)   ->
            let pointer' =
              Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) pointer)
            in
            let res =
              try
                build_mutation pointer' field v' SKIP loc
              with
                e ->
                pn "@@@@7";
                raise e
            in
            [], [res], xs'
            
         | C.INDEX (left_part, index)    ->
            let res, fvs' = fc_get_statement [] SKIP loc fvs  (C.BINARY (C.ASSIGN, k', var)) in
            [], [res], xs'
            
         | C.VARIABLE (kk) ->
            begin
              let k = Exp.be_typed fvs to_static l (Exp.string_to_var kk) in

              let is_it_struct specs =
                let org_tp = get_original_type (get_type_name_from_specifier specs) in
                
                let is_simple = Exp.is_simple_type org_tp || org_tp = "void" in
                
                
                let is_struct = VCabs.is_struct (!enums) specs in
                
                not is_simple && is_struct
              in
              match var with
              | C.CAST (([C.SpecType C.Tsizet], C.JUSTBASE), C.SINGLE_INIT (C.UNARY (C.ADDROF, e))) ->


                 let rec get_dtype = function
                   | C.PTR([], C.JUSTBASE) -> C.JUSTBASE
                   | C.PARENTYPE (attrs1, x, attrs2) ->
                      C.PARENTYPE (attrs1, get_dtype x, attrs2)
                   | C.ARRAY (dtype, attrs, expr) ->
                      C.ARRAY (get_dtype dtype, attrs, expr)
                   | C.PTR (attrs, dtype) ->
                      C.PTR (attrs, get_dtype dtype)
                   | C.PROTO (dtype, names, variadic) ->
                      C.PROTO (get_dtype dtype, names, variadic)
                   | C.JUSTBASE -> C.JUSTBASE
                 in

                 let rec retrieve_base = function
                     C.MEMBEROFPTR (C.CAST ((speclist, dtype'), C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0"))), field) ->
                      let dtype = get_dtype dtype' in
                      (dtype, speclist, C.VARIABLE field)
                   | C.MEMBEROF (base', field) ->
                      let (a,b,base) = retrieve_base base' in
                      (a,b,C.MEMBEROF(base, field))
                   | C.MEMBEROFPTR (base', field) ->
                      let (a,b,base) = retrieve_base base' in
                      (a,b,C.MEMBEROFPTR(base, field))
                   | C.INDEX (base', index) ->
                      let (a,b,base) = retrieve_base base' in
                      (a,b,C.INDEX(base, index))
                   | C.PAREN (exp) ->
                      let (a,b, exp') = retrieve_base exp in
                      (a,b,C.PAREN(exp'))
                   | e ->
                      pn "== "; Cprint.print_expression e; pn " ==";
                      raise (StError "Unmatched Offset Pattern")
                 in

                 let (dtype, speclist, member) = retrieve_base e in

                 let rec member_to_expr = function
                   | C.VARIABLE _ as v ->
                      let exp = Exp.toExp (packs fvs ) v in
	                    exp
                   | C.MEMBEROF (base, field) ->
                      let exp = member_to_expr base in
                      Exp.ARROW (exp, field)
                   | C.INDEX (base, index) ->
                      let exp = member_to_expr base in
	                    Exp.INDICES ([exp;Exp.toExp (packs fvs ) index])
                   | _ ->
                      raise (StError "Member to Expr")
                 in

                 let type_name = get_type_name_from_specifier speclist in
                 let tt = Exp.OFFSET (type_name, member_to_expr member) in
                 [k], [ASSIGN (k, Term.EXP tt, SKIP, loc)], xs'
                 
              | C.CAST ((specs, dt), C.SINGLE_INIT exp ) when
                    is_it_struct specs  ->

                 let s = get_type_name_from_specifier specs in
                 let ptr_attr = match dt with C.PTR _ -> [Exp.PTR] | _ -> [] in
                 let st_attr =
                   if is_func_ptr_type s then
                     let attr = get_func_ptr_attr s in
                     Exp.FUNCPTR (attr, [])
                   else if s = "void" then
                     Exp.SIMPLE 8
                   else
                     Exp.STRUCT s
                 in
                 let (vn, attr) = Exp.var k in
                 let k'' = Exp.VAR (vn, st_attr::attr @ ptr_attr) in
                 let t_exp = Term.be_typed fvs to_static l @@ Term.toTerm (packs fvs ) exp in   
                 [k''], [ASSIGN (k'', t_exp, SKIP, loc)], xs'
                 
              | C.CALL ((C.VARIABLE fname) as exp, cl_exp)
                | C.CALL (C.PAREN ((C.VARIABLE fname) as exp), cl_exp) ->
                 
                 let l_exp = (Term.be_typed fvs to_static l) |>>| (Term.toTerm (packs fvs ) |>>| cl_exp) in
                 let t_exp = Term.be_typed fvs to_static l @@ Term.toTerm (packs fvs ) exp in
                 
                 let fhd = Term.head "#_1=f()" t_exp in

                 let v_k' =
                   if cast then
                     k
                   else
                     let attrs' = Exp.get_attributes fhd in (** attributes of the function *)
                     let attrs1 = (fun at' -> not (at' |<- [Exp.GLOBAL;Exp.STATIC] )) |>- attrs' in
                     let attrs2 = if Exp.is_func fhd then (** This is a carefull design choice *)
                                    Exp.get_func_ret fhd
                                  else if Exp.is_funcptr fhd then
                                    Exp.get_funcptr_ret fhd
                                  else 
                                    attrs1
                     in
                     let attrs = if Exp.is_struct k && not (Exp.is_struct fhd) then
                                   attrs2 @ [Exp.STRUCT (Exp.get_struct_name k)]
                                 else
                                   attrs2
                     in
                     __E (kk, attrs)
                 in
                 
                 let pr, fvs', xs''' =
                   let selected, xs''' = List.partition (function LOOKUP (x, Term.EXP Exp.VAR _, "*", _, _) when Term.EXP x = t_exp -> true | _ -> false) xs' in
                   match selected with
                     LOOKUP (_, x, _, _, _)::_ ->
                      let pr1 = ASSIGN (v_k', Term.EXP (__E ("$ret", [Exp.GLOBAL])), SKIP, loc) in
                      let pr2 = build_call x l_exp SKIP  loc in
                      [pr2;pr1], [v_k'], xs'''
                   | _ ->
                      match Term.toStr t_exp with
                        "malloc" ->
                         begin
                           [MALLOC (v_k', Term.toExp @@ List.hd l_exp, SKIP, loc)], [v_k'], xs'
                         end
                      | "free" -> [DISPOSE (Term.EXP v_k', SKIP, loc)],[v_k'],xs'
                      | fstr ->
                         ([build_call t_exp l_exp SKIP  loc;
                        ASSIGN (v_k', Term.EXP (__E ("$ret", [Exp.GLOBAL])), SKIP, loc)], [v_k'], xs')
                 in
                 fvs', pr, xs'''
                 
              | C.MEMBEROF (expression, str)
                | C.MEMBEROFPTR (expression, str) ->
                 let exp = Term.toTerm (packs fvs ) expression in
                 let exp' = Term.be_typed fvs to_static l exp in

                 if !Options.is_old || not (str = "*") then
                   let (nvs, stmt) =
                     try
                         build_lookup ~struct_name:stn fvs k exp' str SKIP loc                       
                     with
                       e ->
                       dbg "EXCEP" "DEBUG DATA" Cprint.print_expression expression;
                       dbg "EXCEP" "Field Name:" p str;
                       dbg "EXCEP" "FVs:" (V.iter (fun k v -> Exp.var_print (k,v); p ", ")) fvs;
                       pn_s "EXCEP" "";
                       
                       dbg "EXCEP" "Structures:\n" (V.iter (fun _ st -> (fun (n,fs,_) -> p n; p "::"; iterS Exp.pprint ", " (fst |>>| fs); pn "") st)) !structures;
                       dbg "EXCEP" "Previous Expressions:" (iterS (fun (e1,e2) -> Cprint.print_expression e1; p "-->"; Cprint.print_expression e2) "\n") xs;
                       raise e
                   in
                   nvs, [stmt], xs'
                 else
                   let st = (Term.toStr exp') ^ "_" ^ str in
                   [], [ASSIGN (k, Term.encode (st, []), SKIP, loc)], xs'
                   
              | C.QUESTION (exp1, exp2, exp3) ->
                 let exp1' = BExp.be_typed fvs to_static l (BExp.toBExp (packs fvs ) loc exp1) in
                 let exp2' = Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) exp2) in
                 let exp3' = Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) exp3) in
                 [], [IF (exp1', BLOCK (ASSIGN (k, exp2', SKIP, loc), SKIP, loc), BLOCK (ASSIGN (k, exp3', SKIP, loc), SKIP, loc),SKIP,loc)], xs'
                 
              | C.INDEX (_, _) as x
                | C.UNARY (C.MEMOF, x) ->
                 begin
                   match x with
                     C.PAREN (C.CONSTANT (C.CONST_STRING str))
                   | C.CONSTANT (C.CONST_STRING str) ->
                      let stmt = ASSIGN (k, Term.EXP (Exp.STRING str), SKIP, loc) in
                      [k], [stmt], xs'
                   | _ ->
                      
                      let exp = Term.be_typed fvs to_static l (Term.toTerm (packs fvs ) x) in
                      let (nvs, stmt) = build_lookup fvs k exp "*" SKIP loc in
                      
                      nvs, [stmt], xs'
                 end
                
              | _ ->
                 
                 let attr =
                   try
                     match v' with
                       Term.EXP (Exp.ADDR (Exp.ARROW (t, f))) ->
                        
                        let t_head = Exp.head "Making of Arrow" t in
                        let _type =  Exp.get_struct_name t_head in
                        let _field_type= get_field_type _type f loc (Term.EXP t) in
                        if Exp.PTR |<- _field_type then
                          Exp.PTRPTR::_field_type
                        else
                          Exp.PTR::_field_type
                     | Term.EXP (Exp.ADDR (t)) ->
                        
                        let t_head = Exp.head "Making of Arrow" t in
                        
                        let _field_type = (function Exp.GLOBAL -> false| Exp.STATIC -> false | Exp.FUNC _ | Exp.PARAM -> false | _ -> true) |>- (Exp.get_attributes t_head) in
                        
                        if Exp.PTR |<- _field_type then
                          Exp.PTRPTR::_field_type
                        else(
                          if Exp.is_func t_head then
                            let attr = Exp.get_func_ret t_head in
                            Exp.FUNCPTR (attr, [])::_field_type
                          else
                            Exp.PTR::_field_type
                        )
                     | _ ->
                        let t_head = Term.head "Making of Assignment" v' in
                        let _field_type = (function Exp.GLOBAL -> false| Exp.STATIC -> false | Exp.FUNC _ | Exp.PARAM -> false | _ -> true) |>- (Exp.get_attributes t_head) in
                        if Exp.is_func t_head then
                          let attr = Exp.get_func_ret t_head in
                          Exp.FUNCPTR (attr, [])::_field_type
                        else
                          _field_type
                   with
                     _ ->
                     []
                 in
                 let k'' = Exp.set_attributes k attr in
                 
                 [k''], [(ASSIGN (k'', v', SKIP, loc))], xs'
            end
         | C.CAST ((specifier, decl_type), init_expression) ->
            (* pn "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
            Cprint.print_init_expression init_expression; pn "";
            pn "~===========================~"; *)
            let rec f i_e specifier =
              match i_e with
              | Cabs.SINGLE_INIT ((C.VARIABLE (vname) as expression)) ->
                 
                 let s_type = get_type_name_from_specifier specifier in
                 let o_type = get_original_type s_type in
                 let nv =
                   if Exp.is_simple_type o_type then
                     __E (vname, [Exp.SIMPLE (Exp.simple_size o_type)])
                   else if o_type = "void" then
                     __E (vname, [])
                   else
                     __E (vname, [Exp.STRUCT o_type; Exp.PTR])
                 in
                 let fvs' = add_fv ~op:true fvs nv in
                 let (newvars, stmt, xs'') = to_assignment ~is_cast:true expression var fvs' in
                 (nv::newvars, stmt, xs'')
              | C.SINGLE_INIT (C.CAST ((sp, _), ie)) ->
                 f ie sp
              | C.SINGLE_INIT (expression) ->
                 dbg "EXCEP" "Problemetic expression:" Cprint.print_expression expression;
                 raise (StError ("NON-VARIABLE INIT @ " ^ (Locs.to_str loc)))                
              | C.NO_INIT -> ([], [], [])
              | C.COMPOUND_INIT ls ->
                 (fun (a,b,c) (d, e) -> let (a',b',c') = f e specifier in (a@a', b@b', c@c')) |->> (([],[],[]), ls)
            in
            
            f init_expression specifier
         | _ -> raise (StError "Get Assignment")
       in
       let (newvars', stmt, xs'') = to_assignment k' var fvs in
       
       let newvars = (function Exp.VAR _ as newvars -> newvars | _ -> raise (StError "Not a variable")) |>>| newvars' in
       (add_fvs ~op:true fvs newvars, xs'' @ stmt)
        
  let contains str sub =
    let slen = String.length str in
    let blen = String.length sub in
    let mlen = slen - blen in
    let rec rcont i =
      if i <= mlen then
        begin
          if String.length str >= i+blen && (String.sub str i blen) = sub then
            true
          else
            rcont (i+1)
        end
      else
        false
    in
    rcont 0

  let rec init_to_expression = function
    | C.SINGLE_INIT (e1) -> [e1]
    | C.NO_INIT -> []
    | C.COMPOUND_INIT es ->
       try
         List.concat ((fun (_, iexp) -> init_to_expression iexp) |>>| es)
       with
         _ -> raise (StError ("init_to_expression"))

  (*
  let to_assignments assns loc =
    (fun (x, e) -> C.COMPUTATION (C.BINARY (C.ASSIGN, C.VARIABLE (fst x), List.hd @@ init_to_expression e), loc)) |>>| assns
   *)

  let to_assignments' fvs  assns loc =
    List.concat ((fun (x, e) ->
        let e' = (init_to_expression e) in
        if List.length e' = 1 then
          try
            [ASSIGN (x, Term.toTerm (packs fvs ) (List.hd e'), SKIP, Locs.to_loc loc)]
          with
            _ -> raise (StError "List.hd problem at To Assignments")
        else
          fst ((
              fun (acc, n) e ->
              let x' = Term.EXP (Exp.BINOP (x, Op.ADD, Exp.CONST n)) in
              let res =
                try
                  build_mutation x' "*" (Term.toTerm (packs fvs ) e) SKIP (Locs.to_loc loc)
                with
                  e ->
                  pn "@@@@8";
                  raise e
              in
              (acc@[res], n+1)
            ) |->> (([], 0), e'))
      ) |>>| assns)

  let rec no_lbl = function
      C.CALL (C.VARIABLE s, _) when String.length s > 1 && String.sub s 0 1 = "@" -> false
    | C.BINARY (C.AND, exp1, exp2) -> no_lbl exp1 && no_lbl exp2
    | _ -> true

  (* let return = RETURN (Term.zero, SKIP, Locs.dummy) *)

  let rec does_contain_ptr c_exp =
    match c_exp with
    | C.INDEX (exp1, exp2) -> does_contain_ptr exp1 || does_contain_ptr exp2
    | C.BINARY (_, exp1, exp2) -> does_contain_ptr exp1 || does_contain_ptr exp2
    | C.UNARY (_, exp) -> does_contain_ptr exp
    | C.QUESTION (exp1, exp2, exp3) -> does_contain_ptr exp1 || does_contain_ptr exp2 || does_contain_ptr exp3
    | C.CALL (_, l_exp) -> List.exists does_contain_ptr l_exp
    | C.COMMA (l_exp) -> List.exists does_contain_ptr l_exp
    | C.PAREN (exp) -> does_contain_ptr exp
    | C.VARIABLE (_) -> false
    | C.MEMBEROF (exp, _) -> does_contain_ptr exp
    | C.MEMBEROFPTR (_, _) -> true
    | _ -> false

  let rec get_pattern fvs  loc = function
    (** *x != Null *)
    | (C.BINARY (C.NE, C.UNARY(C.MEMOF,_), exp0)) as exp  when Term.toTerm (packs fvs ) exp0 = Term.NULL -> [BExp.toBExp (packs fvs ) loc exp]
    | (C.BINARY (C.EQ, C.UNARY(C.MEMOF,_), exp0)) as exp  when Term.toTerm (packs fvs ) exp0 = Term.NULL -> [BExp.toBExp (packs fvs ) loc exp]
    | (C.BINARY (_, C.UNARY(C.MEMOF,_), C.CONSTANT (C.CONST_CHAR _))) as exp -> [BExp.toBExp (packs fvs ) loc exp]
    (** *x != Null or *y != Null *)
    | C.BINARY (C.AND, exp1, exp2) -> get_pattern fvs  loc exp1 @ get_pattern fvs  loc exp2
    | C.BINARY (C.OR, exp1, exp2) -> get_pattern fvs  loc exp1 @ get_pattern fvs  loc exp2
    | C.UNARY (C.NOT, exp) -> get_pattern fvs  loc exp
    | exp ->
       dbg "PATTERN" "No Pattern" Cprint.print_expression exp;
       []

  let rec cabs_beval e =
    (* let rec shl x y =
      if y <= 0 then
        x
      else
        shl (x*2) (y-1)
    in
    
    let rec shr x y =
      if y <= 0 then
        x
      else
        shl (x/2) (y-1)
    in
    let rec band c1 c2 =
      let b1 = c1 mod 2 in
      let b2 = c2 mod 2 in
      let c1' = c1 / 2 in
      let c2' = c2 / 2 in
      let b = b1*b2 in
      if c1'=0 || c2'=0 then
        b
      else
        b + 2 * band c1' c2'
    in
    let rec bor c1 c2 =
      let b1 = c1 mod 2 in
      let b2 = c2 mod 2 in
      let c1' = c1 / 2 in
      let c2' = c2 / 2 in
      let b = if b1+b2 > 0 then 1 else 0 in
      if c1'=0 || c2'=0 then
        b + 2 * (c1'+c2')
      else
        b + 2 * bor c1' c2'
    in
    let rec bxor c1 c2 =
      let b1 = c1 mod 2 in
      let b2 = c2 mod 2 in
      let c1' = c1 / 2 in
      let c2' = c2 / 2 in
      let b = if b1=b2 then 0 else 1 in
      if c1'=0 && c2'=0 then
        b
      else
        b + 2 * bxor c1' c2'
    in *)
    match e with
      C.VARIABLE _ -> (None, None)
    | C.CONSTANT (C.CONST_INT i) -> (None, Some (int_of_string i))
    | C.UNARY (C.NOT, e1) ->
       begin
         let e1' = cabs_beval e1 in
         match e1' with
           (Some b, c) -> (Some (not b), c)
         | (None, Some i) -> (Some (i <> 0), None)
         | _ -> e1'
       end
    | C.UNARY (C.MINUS, e1) ->
       begin
         let e1' = cabs_beval e1 in
         match e1' with
           (b, Some i) -> (b, Some (0-i))
         | _ -> e1'
       end
    | C.UNARY (C.BNOT, e1) ->
       begin
         let e1' = cabs_beval e1 in
         match e1' with
           (b, Some i) -> (b, Some (0-i-1))
         | _ -> e1'
       end
    | C.BINARY (op, e1, e2) ->
       begin
         let e1' = cabs_beval e1 in
         let e2' = cabs_beval e2 in
         match e1', e2' with
           (Some b1, _), (Some b2, _) ->
           begin
             let e1'' =
              match op with
                C.AND -> Some (b1 && b2)
              | C.OR -> Some (b1 || b2)
              | C.EQ -> Some (b1 = b2)
              | C.NE -> Some (b1 <> b2)
              | _ -> None
             in
             e1'', None
           end
         | (_, Some c1), (_, Some c2) ->
            begin
                match op with
                  C.ADD -> (None, Some (c1+c2))
                | C.SUB -> (None, Some (c1-c2))
                | C.MUL -> (None, Some (c1*c2))
                | C.DIV -> (None, Some (c1/c2))
                | C.MOD -> (None, Some (c1 mod c2))
                | C.EQ -> (Some (c1=c2), None)
                | C.NE -> (Some (c1<>c2), None)
                | C.LT -> (Some (c1<c2), None)
                | C.GT -> (Some (c1>c2), None)
                | C.LE -> (Some (c1<=c2), None)
                | C.GE -> (Some (c1>=c2), None)
                | C.SHL -> (None, Some (c1 lsl c2))
                | C.SHR -> (None, Some (c1 lsr c2))
                | C.BAND -> (None, Some (c1 land c2))
                | C.BOR -> (None, Some (c1 lor c2))
                | C.XOR -> (None, Some (c1 lxor c2))
                | _ -> (None, None)
            end
         | _ -> (None, None)
       end
    | _ -> (None, None)

  

  let get_declaration ?is_global:(glob=false) ?is_param:(param=false) (specifier, l_init_name) pointers cabsloc fvs  =
    let loc = Locs.to_loc cabsloc in
    let get_fields_from_stored_struct str =
      if V.mem str !structures then
          let (_, fld_dt, _) =
            V.find str !structures in
          fst |>>| fld_dt
      else
        (dbg "WARNING" "Exceptional Struct" p str;
         [])
    in
    let rec init i f = if i = 0 then [] else f i :: init (i-1) f in
    let filter_attr attr =
      if List.exists (function Exp.PTRPTR -> true | _ -> false) attr then
        List.filter ((<>) Exp.PTRPTR) attr
      else if List.exists (function Exp.PTR -> true | _ -> false) attr then
        List.filter ((<>) Exp.PTR) attr
      else
        attr
    in
    let rec _typeof_e fvs = function
        [] -> None
      | C.SpecType C.TtypeofE exp:: _ ->
         begin
           match exp with
             C.UNARY (_, C.VARIABLE v) ->
              if V.mem v fvs then
                let attrs = V.find v fvs in                
                Some (filter_attr attrs)
              else
                None
           | _ ->
              None
         end
      | _ ->
         None
    in
    let rec get_tags fvs cabsloc dt =
      let rec aux = function
        | C.JUSTBASE -> []
        | C.ARRAY (dt, _, C.NOTHING) ->
           Some (Exp.NOTHING):: aux dt
        | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.VARIABLE e))
          | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.PAREN(C.VARIABLE e))) ->
           let v'= Exp.VAR (e, []) in
           let v = Exp.be_typed fvs to_static loc v' in
           let sz = size_of v in
           Some sz::aux dt
        | C.ARRAY (dt, _, ((C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX  (C.PAREN C.VARIABLE vvv', _)))) as ee))
          | C.ARRAY (dt, _, ((C.PAREN (C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX (C.PAREN C.VARIABLE vvv', _))))) as ee)) when vvv=vvv' ->
           begin
             let res = aux dt in
             if V.mem vvv fvs then
               let v'= Exp.VAR (vvv, []) in
               let v = Exp.be_typed fvs to_static loc v' in
               if Exp.is_array v then
                   Some (List.hd (Exp.get_array_length v)) :: res
               else
                 let sz = size_of v in
                 Some sz :: res
             else
               let e' = Exp.toExp (packs fvs ) ee in
               let e'' = Exp.eval e' in
               Some e'' :: res
           end
        | C.ARRAY (dt, _, e)  ->
           Some (Exp.eval @@ Exp.toExp (packs fvs ) e) :: aux dt
        | C.PARENTYPE (_, dt, _) -> aux dt
        | C.PTR (_, dt) -> aux dt
        | C.PROTO (dt, _, _) -> aux dt
      in
      (List.rev @@ aux dt)
    in
    let rec convert_field fvs (specifier, (name, dt, _, cabsloc)) =
      let loc = Locs.to_loc cabsloc in
      let (str, is_struct, fields, lens) = is_it_struct fvs loc specifier in
      let tags = get_tags fvs cabsloc dt in
      let (is_array, len) = (fun (is_array, len) tag ->
          match tag with
            None -> (is_array || false, len@lens)
          | Some n -> true, len @ [n]
        ) |->> ((false, []), tags) in
      let attr0 = if not is_struct && Exp.is_simple_type str then [Exp.SIMPLE (Exp.simple_size str)] else [] in
      let attr1 = if is_struct then [Exp.STRUCT (get_original_type str)] else [] in
      let attr2 = if is_array then [Exp.ARRAY len] else [] in
      __E (name, attr0@attr1@attr2)

    and convert_fields fvs = function
        [] -> []
      | (specifier, ne_l)::flds ->
         let current_flds = (fun (name, _) -> (specifier, name)) |>>| ne_l in
         let current_flds' = convert_field fvs |>>| current_flds in
         current_flds' @ convert_fields fvs flds
    
    and is_it_struct (fvs : Exp.attr list V.t) loc = function
        [] -> ("", false, [], [])
      | C.SpecType (C.Tnamed str')::_ ->
         begin
           dbg "INIT" "str':" p str';
           
           let str = get_original_type str' in
           dbg "INIT" "str:" p str;
           if Exp.is_simple_type str then
             if V.mem str' !array_aliases then
               let (_, lens) = V.find str' !array_aliases in
               (str, false, [], lens)
             else
               (str, false, [], [])
           
           else
             try
             let fields = get_fields_from_stored_struct str in
             
             if V.mem str' !array_aliases then(
               pn_s "INIT" (str' ^ " is array");
               let (str'', lens) = V.find str' !array_aliases in
               
               try
                 let fields = get_fields_from_stored_struct str'' in
                  (str'', true, fields, lens)
               with
                  Not_found ->
                  (str', true, fields, lens)
             )
             else
               (str', true, fields, [])
                  
           with
             Not_found ->
              if V.mem str !array_aliases then
                let (str', lens) = V.find str !array_aliases in
                let lens' = lens in
                try
                  let fields = get_fields_from_stored_struct str' in
                  (str', true, fields, lens')
                with
                  Not_found ->
                  ("", false, [], lens')
              else
                ("", false, [], [])
         end
      | C.SpecType (C.Tstruct (str, None, _))::_ ->
         let fields = get_fields_from_stored_struct str in
         (str, true, fields, [])
      | C.SpecType (C.Tstruct (str, Some fields_l, _))::_ ->
         let str : string = if str = "" then new_prog_var () else str in
         let fields : Exp.t list = convert_fields fvs fields_l in
         let fields' = (fun fld -> (fld, Term.zero)) |>>| fields in
         structures := V.add str (str, fields', None) !structures;
         (str, true, fields, [])
      | _::xs -> is_it_struct fvs loc xs
    in
    let to_int_list s =
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) []
    in
    let rec break_string_in_init = function
        C.NO_INIT -> C.NO_INIT
      | C.SINGLE_INIT (C.CONSTANT C.CONST_STRING s) ->
         let lt = to_int_list s in
         let lt' = (fun t -> (C.NEXT_INIT, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT (string_of_int (Char.code t)))))) |>>| lt in
         C.COMPOUND_INIT lt'
      | C.SINGLE_INIT t -> C.SINGLE_INIT t
      | C.COMPOUND_INIT lt ->
         let lt' = (fun (a,b) -> a, break_string_in_init b) |>>| lt in
         C.COMPOUND_INIT lt'
    in
    
    let rec init_justbase fvs structs data =
      match data with
        C.NO_INIT -> INIT_E
      | C.SINGLE_INIT exp ->
         (* V.iter (fun v atr -> Exp.print (Exp.VAR (v, atr))) fvs; *)
         let exp' = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) exp) in
         (* Term.print exp';
          *)
         INIT_S exp'
      | C.COMPOUND_INIT iw_ie_l ->
         match structs with
           None ->
           begin
             match iw_ie_l with
               (_, C.SINGLE_INIT exp)::[] ->
                INIT_S (Term.toTerm (packs fvs ) exp)
             | _ ->
               pn "ERROR";
                print_endline (Locs.print loc);
                Cprint.print_init_expression data;
                raise (StError "Init Justbase Error")
              end
         | Some fields ->
           init_struct fvs fields data
                                          
    and init_struct fvs (fields : Exp.t list) data =
      dbg "INIT" (String.concat "," (Exp.get_printable_string |>>| fields) ^ " =") Cprint.print_init_expression data;
      
      match data with
      (* | C.COMPOUND_INIT ((_, C.SINGLE_INIT exp)::[]) when List.length fields > 1 (* when Term.toTerm exp = Term.NULL *) ->
        
         INIT_S (Term.toTerm exp) *)
      | C.COMPOUND_INIT iw_ie_l ->
         
         let fld_dt' = (fun f -> (f, INIT_S Term.zero)) |>>| fields in
         let fld_dt = Array.of_list fld_dt' in
            
         let rec get_index key i = function
           | Exp.VAR x::xs -> if fst x=key then i else get_index key (i+1) xs
           | _ ->
              print_endline (Locs.print loc);
              raise (StError "Get Index Error")
         in

         let (fld_dt', _) =
           (fun (fld_dt, i) (iw,ie) ->
             match iw with
               C.NEXT_INIT ->
                begin
                  try
                    let (fld, _) = Array.get fld_dt i in
                    let r = init_field fvs fld ie in
                    Array.set fld_dt i (fld,r)
                  with
                    _e ->
                    pn "Index error A";
                    V.iter (fun k (a, el) ->
                        if List.length el > 0 then (
                        pw k; p ":"; pw a; iterS Exp.pprint "-" el; pn "")) !array_aliases; pn "-----------";
                    print_endline (Locs.print loc);
                    (* raise (StError "Index error A") *)
                    raise _e
                end;
                (fld_dt, i+1)
             | C.INFIELD_INIT (fld, iw) ->
                let i = get_index fld 0 fields in 
                let (fld, _) = Array.get fld_dt i in
                let r = init_field fvs fld ie in
                begin
                  try
                    Array.set fld_dt i (fld,r)
                  with
                    _e ->
                    print_endline (Locs.print loc);
                    pn "Index error B";
                    raise _e
                end;
                (fld_dt, i+1)
             | _ ->
                print_endline (Locs.print loc);
                raise (StError "init struct error")
           ) |->> ((fld_dt, 0), iw_ie_l)
         in
         
         let fld_dt = Array.to_list fld_dt' in
         
         let res = INIT_M (snd |>>| fld_dt) in
         
         res
                 
      | _ ->
         print_endline (Locs.print loc);
         raise (StError "init struct error B")

    and init_field fvs (field : Exp.t) data =
      dbg "INIT" ("INIT_FIELD " ^ Exp.get_printable_string field ^ " =") Cprint.print_init_expression data;
      dbg "INIT" "field:" Exp.print field;
      
      if Exp.is_array field then
        begin
          pn_s "INIT" "Array";
          let ln = Exp.get_array_length field in
          
          let ln' = ((fun l -> Some l) |>>| ln) in
          
          if Exp.is_struct field then
            let st = Exp.get_struct_name field in
            let (_, fields', _) = V.find st !structures in
            let fields = fst |>>| fields' in
            init_array fvs ln' (Some fields) data
          else
            init_array fvs ln' None data
        end
      else
        begin
          if Exp.is_struct field && not (Exp.is_ptr field) then
            begin
              pn_s "INIT" "Struct|Array_Aliases";
              let st = Exp.get_struct_name field in
              if V.mem st !structures then
                let (_, fields', _) = V.find st !structures in
              
                if V.mem st !array_aliases then
                  begin
                    pn_s "INIT" "Typedef Array with Struct";
                    let (_, ln) = V.find st !array_aliases in
                    let ln' = ((fun l -> Some l) |>>| ln) in
                  
                    let fields = fst |>>| fields' in
                    init_array fvs ln' (Some fields) data
                  end
                else
                  begin
                    pn_s "INIT" "Struct";
                    let fields = fst |>>| fields' in
                    let res = init_justbase fvs (Some fields) data in
                  
                    res
                  end
              else
                if V.mem st !array_aliases then
                  begin
                    pn_s "INIT" "Typedef Array";
                    let (_, ln) = V.find st !array_aliases in
                    let ln' = ((fun l -> Some l) |>>| ln) in
                  
                    init_array fvs ln' None data
                  end
                else
                  begin
                    match data with
                      C.SINGLE_INIT s ->
                       INIT_S (Term.toTerm (packs fvs ) s)
                    | _ ->
                       raise (StError ("Funny init data " ^ st))
                    
                  end
            end
          else
            begin
              pn_s "INIT" "Simple";
              let res = init_justbase fvs None data in
              
              res
            end
        end
      
    and init_array fvs tags (structs : Exp.t list option) data : init =
      pn_s "INIT" "In Init Array";
      match tags with
        []
      | None::[] -> (** No Array *)
         begin
           dbg "INIT" "None =" Cprint.print_init_expression data;
           match data with
             C.SINGLE_INIT x ->
              let z = init_justbase fvs structs data in
              z
           | C.NO_INIT -> INIT_E
           | C.COMPOUND_INIT ed ->
              begin
                match structs with
                | None ->
                   op_p (iterS Exp.pprint ",") structs;
                   print_endline (Locs.print loc);
                   raise (StError "init array error B")
                | Some fields ->
                   init_struct fvs fields data
              end
         end
      | Some n::tags' ->
         begin
           dbg "INIT" ("Some " ^ Exp.get_printable_string n ^ " =") Cprint.print_init_expression data;
           match data with
             C.COMPOUND_INIT ((_,x)::xs) ->
              let n =
                if n = Exp.NOTHING then
                  let ln = List.length xs + 1 in
                  Exp.CONST ln
                else
                  n
              in
              let xs =
                match n with
                  Exp.CONST n' ->
                   let ln = List.length xs + 1 in 
                   if n' = ln then
                     xs
                   else
                     let diff = n' - ln in
                     let cabs0 = C.CONSTANT (C.CONST_INT "0") in
                     let xs' = init diff (fun _ -> (C.NEXT_INIT, C.SINGLE_INIT cabs0)) in
                     xs @ xs'
                | _ -> xs
              in
              let zs : init list =
                let n_1 = Exp.op n (Exp.CONST 1) Op.SUB in
                
                match init_array fvs (Some n_1::tags') structs (C.COMPOUND_INIT xs) with
                  INIT_M data' -> data'
                | _ ->
                   print_endline (Locs.print loc);
                   raise (StError "init array error C")
                
              in
              INIT_M ((init_array fvs tags' structs x)::zs)
           | C.NO_INIT ->
              INIT_E
           | C.COMPOUND_INIT [] ->
              
              INIT_M []
           | C.SINGLE_INIT (C.CONSTANT (C.CONST_STRING _)) ->
              let data' = break_string_in_init data in
              init_array fvs tags structs data'
           | C.SINGLE_INIT (C.VARIABLE _ as v) ->
              INIT_S (Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) v))
           | C.SINGLE_INIT (C.CAST (_, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0")))) 
           | C.SINGLE_INIT (C.PAREN (C.CAST (_, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0"))))) ->  
              INIT_S Term.NULL
           | _ ->
              print_endline (Locs.print loc);              
              print_init_exp data;
              raise (StError "init array error D")
         end
      | _ ->
         print_endline (Locs.print loc);
         raise (StError "init array error E")
    in
    let initialize_one fvs (specifier, ((vname, dt, _, cabsloc), init_expression)) =
      (* Cprint.print_init_expression init_expression; pn ""; kimura *)
      
      let loc = Locs.to_loc cabsloc in
      let arrays = get_tags fvs cabsloc dt in
      dbg "INIT" "lens:" (iterS print_spec_elem "-") specifier;
      let (struct_name, is_type_struct, (fields: Exp.t list), lens) = is_it_struct fvs loc specifier in
      dbg "INIT" "lens:" (iterS Exp.pprint "-") lens;
      let arrays' = arrays @ ((fun i -> Some i) |>>| lens) @ [None] in
      
      let structs : Exp.t list option = if is_type_struct then Some fields else None in
      
      (* let init_expression' = break_string_in_init init_expression in *)

      let init_data = try
          dbg "INIT" "------------------\n" p vname;
          dbg "INIT" " = " Cprint.print_init_expression init_expression;
          dbg "INIT" "dt" print_decl_type dt;
          dbg "INIT" "Arrays" (iterS (op_p Exp.pprint) ",") arrays'; 
          init_array fvs arrays' structs init_expression
        with
          _e ->
          pn vname;
          pn struct_name;
          pb is_type_struct; pn "";
          iterS (op_p Exp.pprint) "-" arrays'; pn "";
          raise _e
      in
      
      (vname, arrays', init_data)
    in
    let type_name = get_type_name_from_specifier specifier in
    let rec simplify_init_expression fvs vname exp : Exp.t list * t list * C.statement list * C.init_expression =
      match exp with
      | C.SINGLE_INIT e ->
         begin
           pn_s "INIT" "Single Init";
           let rec aux e =
             match e with
             | C.CONSTANT (C.CONST_STRING s) ->
                
                let nv = new_prog_var () in
                let lt = (to_int_list s) @ [Char.chr 0] in
                let lt' = (fun t ->
                    INIT_S (Term.EXP (Exp.CONST (Char.code t)))
                  (* (C.NEXT_INIT, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT (string_of_int (Char.code t))))) *)
                  ) |>>| lt in
                let lens = [Exp.CONST (List.length lt')] in
                let simple_type = Exp.SIMPLE (Exp.simple_size "long") in
                let nv_e = __E (nv, [Exp.ARRAY lens; simple_type]) in
                let newv = C.VARIABLE nv in
                (* let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in *)
                
                let init_data = INIT_M lt' in
                let newdecl', _ = build_dec true nv_e lens init_data SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [], C.SINGLE_INIT newv)
             | C.CONSTANT _
             | C.VARIABLE _ ->
                if V.mem type_name !structures && type_name |<- !unions then
                  let (_, all_fields, _) = V.find type_name !structures in
                  let es' = (fun _ -> (C.NEXT_INIT, C.SINGLE_INIT e)) |>>| all_fields in
                  ([], [], [], C.COMPOUND_INIT es')
                    (* (nvs, es', [], newdecl) *)
                else
                  ([], [], [], C.SINGLE_INIT e)
             (* (nvs, [e], [], newdecl) *)
             | C.CAST (((C.SpecType C.Tvoid)::_, C.PTR (_,C.JUSTBASE)), C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0"))) ->
                ([], [], [], exp)
             | C.CAST (((C.SpecType C.Tnamed nm)::_, _), C.SINGLE_INIT exp) when is_func_ptr_type nm ->
                let nv = new_prog_var () in
                let attr = get_func_ptr_attr nm in
                let nv_e = __E (nv, attr) in (* TODO: *)
                let newv = C.VARIABLE nv in
                
                let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in
                let newdecl', _ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
                
             | C.PAREN (exp) ->
                
                aux exp
             | C.QUESTION (exp1,exp2,exp3) ->
                begin
                  match cabs_beval exp1 with
                    (Some b, _) ->
                     let exp = if b then exp1 else exp2 in
                     aux exp
                  | _ ->
                     let nv = new_prog_var () in
                     let newv = C.VARIABLE nv in
                     let st = C.IF (exp1,
                                    C.COMPUTATION (C.BINARY (C.ASSIGN, newv, exp2), cabsloc),
                                    C.COMPUTATION (C.BINARY (C.ASSIGN, newv, exp3), cabsloc), cabsloc)
                     in
                     let nv_e = __E (nv, V.find vname fvs) in
                     
                     let newdecl',_ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                     ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
                end
             | _ ->
                let nv = new_prog_var () in
                let nv_e = __E (nv, V.find vname fvs) in
                let newv = C.VARIABLE nv in
                
                let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in
                
                let newdecl',_ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
           in
           aux e
         end
      | C.NO_INIT ->
         pn_s "INIT" "No Init";
         ([], [], [], C.NO_INIT)
      | C.COMPOUND_INIT es ->
         pn_s "INIT" "Compound Init";
         (* if List.length es = 1 then
           simplify_init_expression (snd (List.hd es))
         else *)
           let (nvs', decls', sts', es') =
             (fun (nvs, decls, sts, acc) (iw, ie) ->
               let (nvs', decls', sts', ie') = simplify_init_expression fvs vname ie in
               match iw, ie' with
               | _, C.COMPOUND_INIT ie'' when V.mem type_name !structures && type_name |<- !unions ->
                  pn_s "INIT" "Union Mode";
                  (nvs@nvs', decls@decls', sts@sts', acc@ie'')
               | C.NEXT_INIT, _ ->
                  pn_s "INIT" "Normal Mode";
                 (nvs@nvs', decls@decls', sts@sts', acc@[(iw,ie')])
               | _, _ ->
                  pn_s "INIT" "Else Mode";
                  (nvs@nvs', decls@decls', sts@sts', acc@[(iw,ie')])
             ) |->> (([],[],[],[]), es)
           in
           pf_s "INIT" Cprint.print_init_expression (C.COMPOUND_INIT es');
           (nvs', decls', sts', C.COMPOUND_INIT es')
    in

    let simplify_init_data fvs ((vname, dt, a, b), init_expression) =
      dbg "INIT" "=========\nBefore simplification:" Cprint.print_init_expression init_expression;
      let (nvs, decls, sts, es) = simplify_init_expression fvs vname init_expression in
      dbg "INIT" "After simplification:" Cprint.print_init_expression es;
      (nvs, decls, sts, ((vname, dt, a, b), es))
    in

    (* let is_extern =
      List.exists (fun spec_elem ->
          match spec_elem with
          | C.SpecStorage (C.EXTERN) -> true
          | _ -> false) specifier in *)

    
    let is_pointer_type = (type_name, true) |<- pointers in
    (* let is_struct = V.mem type_name !structures && not (type_name |<- !enums) in *)

    (* let extra_attr = List.fold_left (fun a (b,c) ->
                   if b then
                     a@[c]
                   else
                     a
                 ) [] [(is_struct, Exp.STRUCT (get_original_type type_name)); (is_extern, Exp.EXTERN)] in *)
    
    let init_name_to_declare (new_pointers, decl_1, fvs, new_statements, new_declares) ((vname, dt, a, cabsloc), init_expression) =
      let attrs = build_attributes ~is_param:param glob vname pointers fvs cabsloc dt specifier in
      let fvs' = add_fv fvs (Exp.VAR (vname, attrs)) in
      let _nvs, _decls, _statements, init_name' = simplify_init_data fvs' ((vname, dt, a, cabsloc), init_expression) in
      let fvs'' = add_fvs fvs' _nvs in
      let (_, arrays_lens, init_data) = initialize_one fvs'' (specifier, init_name') in
      let arrays_lens' = (fun acc o -> match o with Some i -> acc@[i] | None -> acc) |->> ([], arrays_lens) in
      
      let (non_array_attr''', is_pointer, len) = get_array_dim_from_init_name glob fvs'' cabsloc init_name' specifier attrs type_name arrays_lens' in
      (* let non_array_n_other_attr = non_array_attr @@ extra_attr in *)

      let v' = Exp.VAR (vname, non_array_attr''') in
      if Exp.is_func v' && Exp.is_funcptr v' then(
        pn "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR";
        Exp.print (Exp.VAR (vname, attrs)); pn "";
        print_decl_type dt; pn "";
        iterS print_spec_elem "," specifier; pn "";
        Exp.print v'; 
        raise Error);
      
      let decl, v =
          match _typeof_e fvs'' specifier with
            None ->
            build_dec false v' len init_data SKIP loc specifier
          | Some attr ->
             let v'' = Exp.VAR (vname, attr) in
             let len' = if Exp.is_array v'' then
                          Exp.get_array_length v''
                        else
                          len
             in
             build_dec false v'' len' init_data SKIP loc specifier
      in

      let vname' = Exp.var_decode v in
      

      (* let single_init_data_with_types =
        match init_data with
          INIT_S t ->
           if Term.with_head t then
             let d = Term.toExp t in
             let t_attr = Exp.get_attributes d in
             if List.length t_attr = 0 then
               let attr = Exp.get_attributes v in 
               [Exp.set_attributes d attr]
             else
               [d]
           else
             []
        | _ -> []
      in *)
      (** NOTE: Type adjustment for init data. May be not necessary but need to check *)
      (* let decl_1' = (fun p ->
          match p with
            DECL (v, s, d, p', l) ->
             begin
               match d with
                 INIT_S t -> 
                  begin
                    let vd = Term.toExp t in
                    try
                      let d' =
                        if Exp.is_funcptr v && not (__N vd |<- !func_ptrs) then
                          vd
                        else
                          List.find (fun v -> Exp.toStr v = Exp.toStr vd) single_init_data_with_types
                      in
                      
                      let dc = DECL (v, s, INIT_S (Term.EXP d'), p', l) in  
                      dc
                    with
                      _ ->
                      let dc = DECL (v, s, d, p', l) in
                      dc
                  end
               | _ ->
                  let dc = DECL (v, s, d, p', l) in
                  dc
             end
          | p' -> p'
        ) decl in *)
      let decl_1' = decl in
      
      let new_fvs, newdecls = join SKIP _decls in
      
      (** NOTE: Type adjustment for init data of additional declarations. May be not necessary but need to check *)
      (* let newdecl_1 = (fun p ->
          match p with
            DECL (v, s, d, p', l) ->
             begin
               match d with
                 INIT_S t ->
                  begin
                    let vd = Term.decode t in
                    try
                      let d' = List.find (function Exp.VAR (s1,_) -> s1 = fst vd | _ -> false) single_init_data_with_types in
                      DECL (v, s, INIT_S (Term.EXP d'), p', l)
                    with
                      _ -> DECL (v, s, d, p', l)
                  end
               | _ ->
                  DECL (v, s, d, p', l)
             end
          | p' -> p'
        ) newdecls in *)
      let newdecl_1 = newdecls in
      
      (** NOTE: Completely new variables excluding newly declared one. *)
      (* let nvs_1 = (fun v ->
          try
            List.find (function Exp.VAR (s1,_) -> s1 = __N v | _ -> false) single_init_data_with_types
          with
            _ -> v
        ) |>>| (_nvs) in *)

      
      let ns_pointer = if is_pointer then [v] else [] in
      let new_pointer = (type_name, "", ns_pointer) in
      ptr_structs.contents <- (new_pointer::!ptr_structs);
      
      let new_pointers' =
        if is_pointer_type then
          [(vname', false)]
        else
          (fun n -> (Exp.var_decode n, false)) |>>| ns_pointer
      in

      (* let all_newv' = nvs_1 @@ [v] in *)
      
      (new_pointers@@@new_pointers', decl_1@@@[decl_1'], fvs'', (* all_newv@@@all_newv',*) new_statements@_statements, new_declares@[newdecl_1])
    in
    
    
    let (new_pointers, decl_1, fvs, new_statements, new_declares) = init_name_to_declare |->> (([],[],fvs,[],[]), l_init_name) in
    (new_pointers,  decl_1, fvs, List.rev new_statements, snd @@ join SKIP new_declares)
    
  ;;

         
         (*
  let get_declaration ?is_global:(glob=false) (specifier, l_init_name) pointers cabsloc fvs  =
    let loc = Locs.to_loc cabsloc in
    (*
    pn "";
    iterS print_spec_elem "~" specifier;
    pn "";
    iterS print_init_name "~" l_init_name;
    pn ".";
     *)
    
    let get_fields_from_stored_struct str =
      if V.mem str !structures then
        (
          let (_, fld_dt, _) =
            V.find str !structures in
          fst |>>| fld_dt
        )
      else
        (
          dbg "WARNING" "Exceptional Struct" p str;
          []
        )
    in

    let rec init i f =
      if i = 0 then [] else f i :: init (i-1) f
    in

    let filter_attr attr =
      if List.exists (function Exp.PTRPTR -> true | _ -> false) attr then
        List.filter ((<>) Exp.PTRPTR) attr
      else if List.exists (function Exp.PTR -> true | _ -> false) attr then
        List.filter ((<>) Exp.PTR) attr
      else
        attr
    in
    
    let rec _typeof_e = function
        [] -> None
      | C.SpecType C.TtypeofE exp:: _ ->
         begin
           match exp with
             C.UNARY (_, C.VARIABLE v) ->
              if V.mem v fvs then
                let attrs = V.find v fvs in                
                Some (filter_attr attrs)
              else
                None
           | _ ->
              None
         end
      | _ ->
         None
    in
    (*
    let get_int_from_cabs = function
        C.CONSTANT (C.CONST_INT s) -> int_of_string s
      | C.NOTHING -> -1
      | _ ->
         print_endline (Locs.print (Locs.to_loc cabsloc));
         raise (StError "Unexpected Length of Array")
    in
     *)
    (** Get the type name. It can be simple type name or a structure name *)
    let type_name = get_type_name_from_specifier specifier in
    dbg "TYPE" "TypeName:" p type_name;
   
    
    let rec get_tags cabsloc dt =
      let rec aux = function
        | C.JUSTBASE -> []
        | C.ARRAY (dt, _, C.NOTHING) ->
           Some (Exp.NOTHING):: aux dt
        | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.VARIABLE e))
          | C.ARRAY (dt, _, C.EXPR_SIZEOF (C.PAREN(C.VARIABLE e))) ->
           let v'= Exp.VAR (e, []) in
           let v = Exp.be_typed fvs to_static loc v' in
           let sz = size_of v in
           Some sz::aux dt
        | C.ARRAY (dt, _, ((C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX  (C.PAREN C.VARIABLE vvv', _)))) as ee))
          | C.ARRAY (dt, _, ((C.PAREN (C.BINARY (C.DIV, C.EXPR_SIZEOF C.PAREN (C.VARIABLE vvv), C.EXPR_SIZEOF C.PAREN(C.INDEX (C.PAREN C.VARIABLE vvv', _))))) as ee)) when vvv=vvv' ->
           begin
             let res = aux dt in
             if V.mem vvv fvs then
               let v'= Exp.VAR (vvv, []) in
               let v = Exp.be_typed fvs to_static loc v' in
               if Exp.is_array v then
                   Some (List.hd (Exp.get_array_length v)) :: res
               else
                 let sz = size_of v in
                 Some sz :: res
             else
               let e' = Exp.toExp (packs fvs ) ee in
               let e'' = Exp.eval e' in
               Some e'' :: res
           end
        | C.ARRAY (dt, _, e)  ->
           Some (Exp.eval @@ Exp.toExp (packs fvs ) e) :: aux dt
        | C.PARENTYPE (_, dt, _) -> aux dt
        | C.PTR (_, dt) -> aux dt
        | C.PROTO (dt, _, _) -> aux dt
      in
      (List.rev @@ aux dt)
    in

    let rec convert_field (specifier, (name, dt, _, cabsloc)) =
      let loc = Locs.to_loc cabsloc in
      let (str, is_struct, fields, lens) = is_it_struct loc specifier in
      let tags = get_tags cabsloc dt in
      let (is_array, len) = (fun (is_array, len) tag ->
          match tag with
            None -> (is_array || false, len@lens)
          | Some n -> true, len @ [n]
        ) |->> ((false, []), tags) in
      let attr0 = if not is_struct && Exp.is_simple_type str then [Exp.SIMPLE (Exp.simple_size str)] else [] in
      let attr1 = if is_struct then [Exp.STRUCT (get_original_type str)] else [] in
      let attr2 = if is_array then [Exp.ARRAY len] else [] in
      __E (name, attr0@attr1@attr2)
    
    and convert_fields = function
        [] -> []
      | (specifier, ne_l)::flds ->
         let current_flds = (fun (name, _) -> (specifier, name)) |>>| ne_l in
         let current_flds' = convert_field |>>| current_flds in
         current_flds' @ convert_fields flds
    
    and is_it_struct loc = function
        [] -> ("", false, [], [])
      | C.SpecType (C.Tnamed str')::_ ->
         begin
           dbg "INIT" "str':" p str';
           
           let str = get_original_type str' in
           dbg "INIT" "str:" p str;
           if Exp.is_simple_type str then
             if V.mem str' !array_aliases then
               let (_, lens) = V.find str' !array_aliases in
               (str, false, [], lens)
             else
               (str, false, [], [])
           
           else
             try
             let fields = get_fields_from_stored_struct str in
             
             if V.mem str' !array_aliases then(
               pn_s "INIT" (str' ^ " is array");
               let (str'', lens) = V.find str' !array_aliases in
               
               try
                 let fields = get_fields_from_stored_struct str'' in
                  (str'', true, fields, lens)
               with
                  Not_found ->
                  (str', true, fields, lens)
             )
             else
               (str', true, fields, [])
                  
           with
             Not_found ->
              if V.mem str !array_aliases then
                let (str', lens) = V.find str !array_aliases in
                let lens' = lens in
                try
                  let fields = get_fields_from_stored_struct str' in
                  (str', true, fields, lens')
                with
                  Not_found ->
                  ("", false, [], lens')
              else
                ("", false, [], [])
         end
      | C.SpecType (C.Tstruct (str, None, _))::_ ->
         let fields = get_fields_from_stored_struct str in
         (str, true, fields, [])
      | C.SpecType (C.Tstruct (str, Some fields_l, _))::_ ->
         let str : string = if str = "" then new_prog_var () else str in
         let fields : Exp.t list = convert_fields fields_l in
         let fields' = (fun fld -> (fld, Term.zero)) |>>| fields in
         structures := V.add str (str, fields', None) !structures;
         (str, true, fields, [])
      | _::xs -> is_it_struct loc xs
    in
    (*
    let rec pr_tags = function
        [] -> pw "[]"
      | None::_ -> pw "None"
      | Some n::xs -> pl n; pw ""; pr_tags xs
    in *)

    let to_int_list s =
      let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
      exp (String.length s - 1) []
    in
    
    let rec break_string_in_init = function
        C.NO_INIT -> C.NO_INIT
      | C.SINGLE_INIT (C.CONSTANT C.CONST_STRING s) ->
         let lt = to_int_list s in
         let lt' = (fun t -> (C.NEXT_INIT, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT (string_of_int (Char.code t)))))) |>>| lt in
         C.COMPOUND_INIT lt'
      | C.SINGLE_INIT t -> C.SINGLE_INIT t
      | C.COMPOUND_INIT lt ->
         let lt' = (fun (a,b) -> a, break_string_in_init b) |>>| lt in
         C.COMPOUND_INIT lt'
    in
    
    let rec init_justbase fvs structs data =
      match data with
        C.NO_INIT -> INIT_E
      | C.SINGLE_INIT exp ->
         (* V.iter (fun v atr -> Exp.print (Exp.VAR (v, atr))) fvs; *)
         Cprint.print_expression exp; pn "  @@";
         print_fvs fvs; pn " ===================";
         let exp' = Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) exp) in
         (* Term.print exp';
          *)
         INIT_S exp'
      | C.COMPOUND_INIT iw_ie_l ->
         match structs with
           None ->
           begin
             match iw_ie_l with
               (_, C.SINGLE_INIT exp)::[] ->
                INIT_S (Term.toTerm (packs fvs ) exp)
             | _ ->
               pn "ERROR";
                print_endline (Locs.print loc);
                Cprint.print_init_expression data;
                raise (StError "Init Justbase Error")
              end
         | Some fields ->
           init_struct fvs fields data
                                          
    and init_struct fvs (fields : Exp.t list) data =
      dbg "INIT" (String.concat "," (Exp.get_printable_string |>>| fields) ^ " =") Cprint.print_init_expression data;
      
      match data with
      (* | C.COMPOUND_INIT ((_, C.SINGLE_INIT exp)::[]) when List.length fields > 1 (* when Term.toTerm exp = Term.NULL *) ->
        
         INIT_S (Term.toTerm exp) *)
      | C.COMPOUND_INIT iw_ie_l ->
         
         let fld_dt' = (fun f -> (f, INIT_S Term.zero)) |>>| fields in
         let fld_dt = Array.of_list fld_dt' in
            
         let rec get_index key i = function
           | Exp.VAR x::xs -> if fst x=key then i else get_index key (i+1) xs
           | _ ->
              print_endline (Locs.print loc);
              raise (StError "Get Index Error")
         in

         let (fld_dt', _) =
           (fun (fld_dt, i) (iw,ie) ->
             match iw with
               C.NEXT_INIT ->
                begin
                  try
                    let (fld, _) = Array.get fld_dt i in
                    let r = init_field fvs fld ie in
                    Array.set fld_dt i (fld,r)
                  with
                    _e ->
                    pn "Index error A";
                    V.iter (fun k (a, el) ->
                        if List.length el > 0 then (
                        pw k; p ":"; pw a; iterS Exp.pprint "-" el; pn "")) !array_aliases; pn "-----------";
                    print_endline (Locs.print loc);
                    (* raise (StError "Index error A") *)
                    raise _e
                end;
                (fld_dt, i+1)
             | C.INFIELD_INIT (fld, iw) ->
                let i = get_index fld 0 fields in 
                let (fld, _) = Array.get fld_dt i in
                let r = init_field fvs fld ie in
                begin
                  try
                    Array.set fld_dt i (fld,r)
                  with
                    _e ->
                    print_endline (Locs.print loc);
                    pn "Index error B";
                    raise _e
                end;
                (fld_dt, i+1)
             | _ ->
                print_endline (Locs.print loc);
                raise (StError "init struct error")
           ) |->> ((fld_dt, 0), iw_ie_l)
         in
         
         let fld_dt = Array.to_list fld_dt' in
         
         let res = INIT_M (snd |>>| fld_dt) in
         
         res
                 
      | _ ->
         print_endline (Locs.print loc);
         raise (StError "init struct error B")

    and init_field fvs (field : Exp.t) data =
      dbg "INIT" ("INIT_FIELD " ^ Exp.get_printable_string field ^ " =") Cprint.print_init_expression data;
      dbg "INIT" "field:" Exp.print field;
      
      if Exp.is_array field then
        begin
          pn_s "INIT" "Array";
          let ln = Exp.get_array_length field in
          
          let ln' = ((fun l -> Some l) |>>| ln) in
          
          if Exp.is_struct field then
            let st = Exp.get_struct_name field in
            let (_, fields', _) = V.find st !structures in
            let fields = fst |>>| fields' in
            init_array fvs ln' (Some fields) data
          else
            init_array fvs ln' None data
        end
      else
        begin
          if Exp.is_struct field && not (Exp.is_ptr field) then
            begin
              pn_s "INIT" "Struct|Array_Aliases";
              let st = Exp.get_struct_name field in
              if V.mem st !structures then
                let (_, fields', _) = V.find st !structures in
              
                if V.mem st !array_aliases then
                  begin
                    pn_s "INIT" "Typedef Array with Struct";
                    let (_, ln) = V.find st !array_aliases in
                    let ln' = ((fun l -> Some l) |>>| ln) in
                  
                    let fields = fst |>>| fields' in
                    init_array fvs ln' (Some fields) data
                  end
                else
                  begin
                    pn_s "INIT" "Struct";
                    let fields = fst |>>| fields' in
                    let res = init_justbase fvs (Some fields) data in
                  
                    res
                  end
              else
                if V.mem st !array_aliases then
                  begin
                    pn_s "INIT" "Typedef Array";
                    let (_, ln) = V.find st !array_aliases in
                    let ln' = ((fun l -> Some l) |>>| ln) in
                  
                    init_array fvs ln' None data
                  end
                else
                  begin
                    match data with
                      C.SINGLE_INIT s ->
                       INIT_S (Term.toTerm (packs fvs ) s)
                    | _ ->
                       raise (StError ("Funny init data " ^ st))
                    
                  end
            end
          else
            begin
              pn_s "INIT" "Simple";
              let res = init_justbase fvs None data in
              
              res
            end
        end
      
    and init_array fvs tags (structs : Exp.t list option) data : init =
      pn_s "INIT" "In Init Array";
      match tags with
        []
      | None::[] -> (** No Array *)
         begin
           dbg "INIT" "None =" Cprint.print_init_expression data;
           match data with
             C.SINGLE_INIT x ->
              let z = init_justbase fvs structs data in
              z
           | C.NO_INIT -> INIT_E
           | C.COMPOUND_INIT ed ->
              begin
                match structs with
                | None ->
                   op_p (iterS Exp.pprint ",") structs;
                   print_endline (Locs.print loc);
                   raise (StError "init array error B")
                | Some fields ->
                   init_struct fvs fields data
              end
         end
      | Some n::tags' ->
         begin
           dbg "INIT" ("Some " ^ Exp.get_printable_string n ^ " =") Cprint.print_init_expression data;
           match data with
             C.COMPOUND_INIT ((_,x)::xs) ->
              let n =
                if n = Exp.NOTHING then
                  let ln = List.length xs + 1 in
                  Exp.CONST ln
                else
                  n
              in
              let xs =
                match n with
                  Exp.CONST n' ->
                   let ln = List.length xs + 1 in 
                   if n' = ln then
                     xs
                   else
                     let diff = n' - ln in
                     let cabs0 = C.CONSTANT (C.CONST_INT "0") in
                     let xs' = init diff (fun _ -> (C.NEXT_INIT, C.SINGLE_INIT cabs0)) in
                     xs @ xs'
                | _ -> xs
              in
              let zs : init list =
                let n_1 = Exp.op n (Exp.CONST 1) Op.SUB in
                
                match init_array fvs (Some n_1::tags') structs (C.COMPOUND_INIT xs) with
                  INIT_M data' -> data'
                | _ ->
                   print_endline (Locs.print loc);
                   raise (StError "init array error C")
                
              in
              INIT_M ((init_array fvs tags' structs x)::zs)
           | C.NO_INIT ->
              INIT_E
           | C.COMPOUND_INIT [] ->
              
              INIT_M []
           | C.SINGLE_INIT (C.CONSTANT (C.CONST_STRING _)) ->
              let data' = break_string_in_init data in
              init_array fvs tags structs data'
           | C.SINGLE_INIT (C.VARIABLE _ as v) ->
              INIT_S (Term.be_typed fvs to_static loc (Term.toTerm (packs fvs ) v))
           | C.SINGLE_INIT (C.CAST (_, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0")))) 
           | C.SINGLE_INIT (C.PAREN (C.CAST (_, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0"))))) ->  
              INIT_S Term.NULL
           | _ ->
              print_endline (Locs.print loc);              
              print_init_exp data;
              raise (StError "init array error D")
         end
      | _ ->
         print_endline (Locs.print loc);
         raise (StError "init array error E")
    in

    let initialize_one fvs (specifier, ((vname, dt, _, cabsloc), init_expression)) =
      (* Cprint.print_init_expression init_expression; pn ""; kimura *)
      
      let loc = Locs.to_loc cabsloc in
      let arrays = get_tags cabsloc dt in
      dbg "INIT" "lens:" (iterS print_spec_elem "-") specifier;
      let (struct_name, is_type_struct, (fields: Exp.t list), lens) = is_it_struct loc specifier in
      dbg "INIT" "lens:" (iterS Exp.pprint "-") lens;
      let arrays' = arrays @ ((fun i -> Some i) |>>| lens) @ [None] in
      
      let structs : Exp.t list option = if is_type_struct then Some fields else None in
      
      (* let init_expression' = break_string_in_init init_expression in *)

      let init_data = try
          dbg "INIT" "------------------\n" p vname;
          dbg "INIT" " = " Cprint.print_init_expression init_expression;
          dbg "INIT" "dt" print_decl_type dt;
          dbg "INIT" "Arrays" (iterS (op_p Exp.pprint) ",") arrays'; 
          init_array fvs arrays' structs init_expression
        with
          _e ->
          pn vname;
          pn struct_name;
          pb is_type_struct; pn "";
          iterS (op_p Exp.pprint) "-" arrays'; pn "";
          raise _e
      in
      
      (vname, arrays', init_data)
    in


    let rec simplify_init_expression exp : Exp.t list * t list * C.statement list * C.init_expression =
      match exp with
      | C.SINGLE_INIT e ->
         begin
           pn_s "INIT" "Single Init";
           let rec aux e =
             match e with
             | C.CONSTANT (C.CONST_STRING s) ->
                
                let nv = new_prog_var () in
                let lt = (to_int_list s) @ [Char.chr 0] in
                let lt' = (fun t ->
                    INIT_S (Term.EXP (Exp.CONST (Char.code t)))
                  (* (C.NEXT_INIT, C.SINGLE_INIT (C.CONSTANT (C.CONST_INT (string_of_int (Char.code t))))) *)
                  ) |>>| lt in
                let lens = [Exp.CONST (List.length lt')] in
                let simple_type = Exp.SIMPLE (Exp.simple_size "long") in
                let nv_e = __E (nv, [Exp.ARRAY lens; simple_type]) in
                let newv = C.VARIABLE nv in
                (* let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in *)
                
                let init_data = INIT_M lt' in
                let newdecl', _ = build_dec true nv_e lens init_data SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [], C.SINGLE_INIT newv)
             | C.CONSTANT _
             | C.VARIABLE _ ->
                if V.mem type_name !structures && type_name |<- !unions then
                  let (_, all_fields, _) = V.find type_name !structures in
                  let es' = (fun _ -> (C.NEXT_INIT, C.SINGLE_INIT e)) |>>| all_fields in
                  ([], [], [], C.COMPOUND_INIT es')
                    (* (nvs, es', [], newdecl) *)
                else
                  ([], [], [], C.SINGLE_INIT e)
             (* (nvs, [e], [], newdecl) *)
             | C.CAST (((C.SpecType C.Tvoid)::_, C.PTR (_,C.JUSTBASE)), C.SINGLE_INIT (C.CONSTANT (C.CONST_INT "0"))) ->
                ([], [], [], exp)
             | C.CAST (((C.SpecType C.Tnamed nm)::_, _), C.SINGLE_INIT exp) when is_func_ptr_type nm ->
                let nv = new_prog_var () in
                let attr = get_func_ptr_attr nm in
                let nv_e = __E (nv, attr) in (* TODO: *)
                let newv = C.VARIABLE nv in
                
                let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in
                let newdecl', _ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
                
(*             | C.CAST (a, C.SINGLE_INIT exp) ->
                begin
                  let (a,b,c,d) = aux exp in
                  let d' =
                    match d with
                      C.NO_INIT -> (a,b,c,d)
                    | INIT_S x -> 
                end 
             | C.CAST (a, exp) ->
                raise (StError "Multiple Cast not Supported")
                (*
                let (nvs, decls, sts, e') = simplify_init_expression exp in
                
                let nv = new_prog_var () in
                let newv = C.VARIABLE nv in
                let nv_e = __E (nv, []) in
                let newdecl',_ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) specifier in
                (* let aassn = ASSIGN (nv_e, Term.toTerm e', SKIP, loc) in *)
                (nvs, decls@[newdecl'], sts, C.SINGLE_INIT newv) *) *)
             | C.PAREN (exp) ->
                
                aux exp
             | C.QUESTION (exp1,exp2,exp3) ->
                begin
                  match cabs_beval exp1 with
                    (Some b, _) ->
                     let exp = if b then exp1 else exp2 in
                     aux exp
                  | _ ->
                     let nv = new_prog_var () in
                     let newv = C.VARIABLE nv in
                     let st = C.IF (exp1,
                                    C.COMPUTATION (C.BINARY (C.ASSIGN, newv, exp2), cabsloc),
                                    C.COMPUTATION (C.BINARY (C.ASSIGN, newv, exp3), cabsloc), cabsloc)
                     in
                     (* let simple_type = Exp.SIMPLE (Exp.simple_size "int") in *)
                     let nv_e = __E (nv, []) in
                     
                     let newdecl',_ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                     ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
                end
             | _ ->
                let nv = new_prog_var () in
                (* let simple_type = Exp.SIMPLE (Exp.simple_size "long") in *)
                let nv_e = __E (nv, []) in
                let newv = C.VARIABLE nv in
                
                let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in
                
                let newdecl',_ = build_dec true nv_e [] INIT_E SKIP (Locs.to_loc cabsloc) []  in
                ([nv_e], [newdecl'], [st], C.SINGLE_INIT newv)
           in
           aux e
         end
      | C.NO_INIT ->
         pn_s "INIT" "No Init";
         ([], [], [], C.NO_INIT)
      | C.COMPOUND_INIT es ->
         pn_s "INIT" "Compound Init";
         (* if List.length es = 1 then
           simplify_init_expression (snd (List.hd es))
         else *)
           let (nvs', decls', sts', es') =
             (fun (nvs, decls, sts, acc) (iw, ie) ->
               let (nvs', decls', sts', ie') = simplify_init_expression ie in
               match iw, ie' with
               | _, C.COMPOUND_INIT ie'' when V.mem type_name !structures && type_name |<- !unions ->
                  pn_s "INIT" "Union Mode";
                  (nvs@nvs', decls@decls', sts@sts', acc@ie'')
               | C.NEXT_INIT, _ ->
                  pn_s "INIT" "Normal Mode";
                 (nvs@nvs', decls@decls', sts@sts', acc@[(iw,ie')])
               | _, _ ->
                  pn_s "INIT" "Else Mode";
                  (nvs@nvs', decls@decls', sts@sts', acc@[(iw,ie')])
             ) |->> (([],[],[],[]), es)
           in
           pf_s "INIT" Cprint.print_init_expression (C.COMPOUND_INIT es');
           (nvs', decls', sts', C.COMPOUND_INIT es')
    in
    
    let simplify_init_data (nvs', decls', sts', acc) ((vname, dt, a, b), init_expression) =
      dbg "INIT" "=========\nBefore simplification:" Cprint.print_init_expression init_expression;
      let (nvs, decls, sts, es) = simplify_init_expression init_expression in
      dbg "INIT" "After simplification:" Cprint.print_init_expression es;
      (nvs@nvs', decls@decls', sts@sts', acc@[((vname, dt, a, b), es)])
    in
    let _nvs, _decls, _sts, l_init_name' = simplify_init_data |->> (([],[],[],[]), l_init_name) in
    let fvs' = (fun fvs x ->
        match x with
          Exp.VAR (v, attr) -> V.add v attr fvs
        | _ -> fvs
      ) |->> (fvs, _nvs) in
    
    let initialize (specifier, l_init_name) =
      let fvs'' = (fun fvs ((name, dt, _, loc), _) ->
          let attrs = build_attributes glob name pointers fvs' loc dt specifier in
          add_fv fvs (Exp.VAR (name, attrs)) (** TODO: Can be unified *)
        ) |->> (fvs', l_init_name) in
      
      
      let res = (fun x ->
        initialize_one fvs'' (specifier,x)
        ) |>>| l_init_name in
      
      res, fvs''
    in

    let init_data, fvs'' = initialize (specifier, l_init_name') in
    
    let is_extern = List.exists (fun spec_elem ->
                        match spec_elem with
                        | C.SpecStorage (C.EXTERN) -> true
                        | _ -> false) specifier in
    
    let is_pointer_type = (type_name, true) |<- pointers in (** If the type is a pointer type *)

    (** Get list of variable/pointer names, along with if it is a pointer and its initial expression and its length if an array *)
    let ns = get_var_name_from_init_name glob fvs'' cabsloc l_init_name' specifier in
    dbg "TYPE" "Number of declarations:" pl (List.length ns);
    (** TODO: Need to check if prototype *)
    let is_struct = V.mem type_name !structures && not (type_name |<- !enums) in
    let attr = List.fold_left (fun a (b,c) ->
                   if b then
                     a@[c]
                   else
                     a
                 ) [] [(is_struct, Exp.STRUCT (get_original_type type_name));(glob, Exp.GLOBAL); (is_extern, Exp.EXTERN)] in
    
    (* let nvs, ns, (assns:C.statement list), newdecl = *)
    let _, ns, _, _ =
      (fun (nvs, ns, acc, newdecl) (v, p, e, l) ->
        let rec aux nvs init_data sts newdecl = function
          | C.SINGLE_INIT e ->
             begin
               match e with
                 C.CONSTANT _
               | C.VARIABLE _ ->
                  if V.mem type_name !structures && type_name |<- !unions then
                    let (_, all_fields, _) = V.find type_name !structures in
                    let es' = (fun _ -> e) |>>| all_fields in
                    (nvs, es', [], newdecl)
                  else
                    (nvs, [e], [], newdecl)
               | C.CAST (_, exp) ->
                  aux nvs init_data sts newdecl exp
               | C.PAREN (exp) ->
                  aux nvs init_data sts newdecl (C.SINGLE_INIT exp)
               | C.QUESTION (exp1,exp2,exp3) ->
                  begin
                    match cabs_beval exp1 with
                      (Some b, _) ->
                      let exp = if b then exp1 else exp2 in
                      aux nvs init_data sts newdecl (C.SINGLE_INIT exp)
                    | _ ->
                       let nv = new_prog_var () in
                       let simple_type = Exp.SIMPLE (Exp.simple_size "long") in
                       
                       let nv_e = __E (nv, [simple_type]) in
                       let newv = C.VARIABLE nv in
                       let st = C.IF (exp1,
                                      C.COMPUTATION (C.BINARY(C.ASSIGN, newv, exp2), cabsloc),
                                      C.COMPUTATION (C.BINARY (C.ASSIGN, newv, exp3), cabsloc), cabsloc)
                       in
                       
                       let newdecl', _ = build_dec true nv_e [] INIT_E newdecl (Locs.to_loc cabsloc) [] in
                       (nvs@[nv_e], [newv], sts@[st], newdecl')
                  end
               | _ ->
                  
                  let nv = new_prog_var () in
                  let simple_type = Exp.SIMPLE (Exp.simple_size "long") in
                                  
                  let nv_e = __E (nv, [simple_type]) in
                  let newv = C.VARIABLE nv in
                  
                  let st = C.COMPUTATION (C.BINARY (C.ASSIGN, newv, e), cabsloc) in
                  let newdecl',_ = build_dec true nv_e [] INIT_E newdecl (Locs.to_loc cabsloc) [] in
                  (nvs@[nv_e], [newv], sts@[st], newdecl')
             end
          | C.NO_INIT ->
             (nvs, [], [], newdecl)
          | C.COMPOUND_INIT es ->
             let get_init_data_from_es () =
               (fun (nvs, init_data, sts, newdecl) (iw, es') ->
                 match iw with
                 | C.ATINDEX_INIT _ ->
                    print_endline (Locs.print (Locs.to_loc cabsloc));
                    raise (StError "ATINDEX_INIT")
                 | C.ATINDEXRANGE_INIT _ ->
                    print_endline (Locs.print (Locs.to_loc cabsloc));
                    raise (StError "ATINDEXRANGE_INIT")
                 | _ ->
                 let (nvs', init_data', sts', newdecl') = aux nvs init_data sts newdecl es' in
                 (nvs', init_data @ init_data', sts', newdecl')     
               ) |->> ((nvs,[],[],SKIP), es)
             in
             
             get_init_data_from_es ()
        in

        let (nvs', init_data, sts, newdecl') = aux [] [] [] SKIP e in
        
        (nvs@nvs', ns@[(v, p, init_data, l)], acc@sts, compose newdecl newdecl')
      ) |->> (([],[],[],SKIP), ns)
    in
    
    (** Assign type to the variable/pointer names *)
    let ns = (fun (n, b, q, l) ->
        let n = Exp.set_attributes n (attr @@@ Exp.get_attributes n) in
        (n, b, q, l)
      ) |>>| ns
    in

    (* (** TODO: remove this function possibly *)
    let rec init_be_typed count attr fvs = function
        INIT_E -> INIT_E
      | INIT_S (Term.EXP (Exp.VAR (n, []))) when count = 0 ->
         let t' = Term.EXP (Exp.VAR (n, attr)) in
         INIT_S t'  
      | INIT_S t ->
         (* V.iter (fun k v -> Exp.print (Exp.VAR (k,v))) fvs; pn ""; *)
         let t' =
           try 
             Term.be_typed fvs to_static loc t with _ -> t in
         
         (* Term.print t'; pn ""; *)
         INIT_S t'
      | INIT_M ids -> INIT_M ((init_be_typed (count+1) attr fvs) |>>| ids)
    in *)

    let type_name' = get_original_type type_name in
   
    let declx = (fun (x, b, e', (l:Exp.t list option)) ->
        
        let v =
          let v' = (fun x a -> Exp.var_add a x) |->> (x, attr) in
          let v'' = if b (*|| is_proto_type *) then Exp.var_add Exp.PTR v' else v' in
          let v''' = if is_func_ptr_type type_name then
                       let attr = get_func_ptr_attr type_name in
                       Exp.vars_add attr v''
                     else
                       v''
          in (** TODO: Need to extract attrs from typedef *)
          if Exp.is_simple_type type_name' then
            Exp.var_add (Exp.SIMPLE (Exp.simple_size type_name')) v'''
          else
            v'''
        in
        
        let len' = match l with
            Some lens -> lens
          | None -> []
        in

        let len =
          if V.mem type_name !array_aliases then
            let (_, lens) = V.find type_name !array_aliases in
            len' @ lens
          else
            len'
        in
        
        dbg "TYPE" "Var name:" Exp.pprint v;
        dbg "TYPE" "Array Length:" (iterS Exp.pprint ",") len;
        let (_, dims, data') = List.find (fun (v',_,_) -> v'= __N v) init_data in

        let dims' = (fun acc o -> match o with Some i -> acc@[i] | None -> acc) |->> ([], dims) in
        dbg "TYPE" "Init data dimension:" (iterS Exp.pprint ",") dims';
        let data = (* init_be_typed 0 (snd @@ Exp.var v) fvs'' *) data' in
        
        let len  =
          if List.length dims' < List.length len then
            try
              take (List.length dims) len
            with
              _ -> []
          else
            len
        in
        
        let dc,v =
          match _typeof_e specifier with
            None ->
             
            build_dec false v len data SKIP loc specifier
          | Some attr ->
             let sv' = Exp.toStr v in
             let v' = Exp.VAR (sv', attr) in
             let len' = if Exp.is_array v' then
                          Exp.get_array_length v'
                        else
                          len
             in
             build_dec false v' len' data SKIP loc specifier
        in
        
        
        (((v,b,e',l), dc), (v, data))
      ) |>>| ns in

    let (declx, v_datas) = List.split declx in
    let (ns, decl) = List.split declx in
    
    let newv_with_types =
      (fun acc (v, data) ->
        match data with
          INIT_S t ->
           let t_attr = Exp.get_attributes (Term.toExp t) in
           if List.length t_attr = 0 then
             let attr = Exp.get_attributes v in
             let d' =
               
               if Term.with_head t then
                 let d = Term.toExp t in
                 [Exp.set_attributes d attr]
               else
                 []
             in
             acc @ d'
           else
             acc @ [Term.toExp t]
        | _ ->
           acc
      ) |->> ([], v_datas) in

    let decl_1 = (fun p ->
        match p with
          DECL (v, s, d, p', l) ->
           begin
             match d with
               INIT_S t -> 
                begin
                  let vd = Term.toExp t in
                  try
                    let d' =
                      if Exp.is_funcptr v && not (__N vd |<- !func_ptrs) then
                        vd
                      else
                        List.find (fun v -> Exp.toStr v = Exp.toStr vd) newv_with_types
                    in

                    let dc = DECL (v, s, INIT_S (Term.EXP d'), p', l) in  
                    dc
                  with
                    _ ->
                     let dc = DECL (v, s, d, p', l) in
                     dc
                end
             | _ ->
                let dc = DECL (v, s, d, p', l) in
             (*        if List.length s > 1 then
                        (pprint 4 dc;
                         raise (StError "Multidimensional Arrays"))
                else
              *)        dc
           end
        | p' -> p'
      ) |>>| decl in

    let new_fvs, newdecls = join SKIP _decls in
    (* (fun a d ->
        join  
      ) |->> (SKIP, _decls) in *)
    
    let newdecl_1 = (fun p ->
        match p with
          DECL (v, s, d, p', l) ->
           begin
             match d with
               INIT_S t ->
                begin
                  let vd = Term.decode t in
                  try
                    let d' = List.find (function Exp.VAR (s1,_) -> s1 = fst vd | _ -> false) newv_with_types in
                    DECL (v, s, INIT_S (Term.EXP d'), p', l)
                  with
                    _ -> DECL (v, s, d, p', l)
                end
             | _ ->
                DECL (v, s, d, p', l)
           end
        | p' -> p'
      ) newdecls in

    let nvs_1 = (fun v ->
        try
          List.find (function Exp.VAR (s1,_) -> s1 = __N v | _ -> false) newv_with_types
        with
          _ -> v
      ) |>>| _nvs in

    let fvs' = (fun (n, _, _, _) -> n) |>>| ns in

    (** Split between pointers and variables *)
    let ns_pointer, _ = List.partition (fun (_, b, _, _) -> b) ns in

    (* if (ns_pointer = []) then pn "EMP" else pn "NON-EMP"; *)
    (* let ns_nonpointer, ns_array = List.partition (fun (_, _, _, len) -> len = None) rest in *)
    (** Generate list of new pointers for ptr_structs and append them *)

    let new_pointers = (fun (n, _, _, _) -> (type_name, "", [n])) |>>| ns_pointer in
    ptr_structs.contents <- (!ptr_structs @ new_pointers);

    let new_pointers =
      if is_pointer_type then
        (fun (n, _, _, _) -> (Exp.var_decode n, false)) |>>| ns
      else
        (fun (n, _, _, _) -> (Exp.var_decode n, false)) |>>| ns_pointer
    in

    let all_newv = nvs_1 @ fvs' in
    
    
    
    (new_pointers,
     decl_1,
     all_newv,
     List.rev _sts,
     newdecl_1)

    *)
  (*
    if type_name |<- (fst |>>| (!structures)) then
    (** Type is a strcuture *)
      begin
        (** New pointers_structures are updated below *)
        let applicable_pointers = (fun (_, n, _) -> n = type_name) |>- (!ptr_structs) in
        let new_pointers = List.concat ((fun (n, _, _, _) -> (fun (t, _, c) -> (t, "", n::c)) |>>| applicable_pointers) |>>| ns_nonpointer) in
        ptr_structs.contents <- (!ptr_structs @ new_pointers);
        (** Memory allocation is taken place by structures *)
        (* let (_, fields) = try List.find (fun (n, _) -> n = type_name) (!structures) with _ -> raise (StError "TR-1") in
   *)
        (*
        let decs = (fun x ->
            (* let x = Exp.add (Exp.STRUCT (type_name)) x in
               let x = if is_pointer_type then Exp.add Exp.PTR x else x in *)
            if !Options.is_old then
              begin
                let q = SARRAY (x, Term.EXP (Exp.CONST 1), [], SKIP, loc) (* CONS (__V x, (fun (ns, e) -> (ns, e)) |>>| fields, SKIP, loc) *) in
                q
              end
            else
              begin
                ASSIGN (x, Term.NULL, SKIP, loc)
              end
          ) |>>| ((fun (a, _, _, _) -> a) |>>| ns_nonpointer) in
   *)
        (** Structure pointer is treated same as Simple pointer *)
        let assns = (fun (a,_,e,_) -> (a, e)) |>>| ns_pointer in

        (** Structure array should be treated as simple array *)
        (* let arrays = (fun (n, _, _, l) ->
            match l with
              Some (e) ->
              SARRAY (n, Term.toIndices e, [], SKIP, loc)
            | _ -> raise (StError "get_declaration B")
          ) |>>| ns_array in *)
        let type' = (fun (n, _, _, _) -> (Exp.decode n, false)) |>>| ns_pointer in

        (type',
         decl,
         fvs',
         assns)
      end
    else if is_pointer_type then
      (** Type is a pointer  *)
      begin
        dbg "TYPE" "Number of Pointers:" pl (List.length ns);
        let type' = (fun (n, _, _, _) -> (Exp.decode n, false)) |>>| ns in
        let decs = List.concat
                     ((fun (n, _, e, l) ->
                       pn "@";
                       match l with
                       | Some exps ->
                          let e' = init_to_expression e in
                          let data = fst ((
                                       fun (acc, c) e ->
                                       let x' = Exp.CONST c in
                                       (acc@[(x', Term.toTerm e)], c+1)
                                     ) |->> (([], 0), e')) in
                          (* let n = Exp.add Exp.ARRAY n in
                             let n = Exp.add Exp.PTR n in *)         
                          [MALLOC (n, Term.toIndices exps, data, SKIP, loc)]
                       | None -> []
                     ) |>>| ns)
        in
        dbg "TYPE" "Number of Decs:" pl (List.length decs);
        let assns = List.concat
                      ((fun (n, _, e, l) ->
                        match l with
                        | Some exps -> [] (** array with length*)
                        | None ->
                           [(n, e)]
                      ) |>>| ns)
        in
        dbg "TYPE" "Number of Assns:" pl (List.length assns);
        (type',
         decl @ decs,
         fvs', [])
      end
    else
      (** Type is a simple type *)
      begin
        (** ns' is the list of pointers *)
        let ns' = ((fun (n, b, _, _) -> b) |>- ns) in
        let new_pointers = (fun (n, b, _, _) -> (Exp.decode n, false)) |>>| ns' in
        (* let statements =
           (fun (n, _, e, l) ->
            match l with
            | Some exp ->
               (** Array Initialization *)
               let exps = init_to_exp e in
              (** A Mutation for each cell *)
              let rec iexp_to_cons i = function
                  [] -> []
                | texp::xsexp ->
                  let eexp = Term.EXP (Exp.BINOP(__V n, Op.ADD, Exp.CONST i)) in
                  (MUTATION (eexp, "*", texp, SKIP, loc))::iexp_to_cons (i+1) xsexp
              in
              let init_exp_cons = if exps = [Term.NULL] then [] else (iexp_to_cons 0 exps) in
              SARRAY (n, Term.toIndices exp, [], SKIP, loc)::init_exp_cons
            | None ->
              []
           ) |>>| ns in          (* USE ns' for filtered ASSIGN *) *)
        (* let statements = List.concat statements in *)
        let decs = List.concat
                     ((fun (n, _, e, l) ->
                       match l with
                       | Some exps ->
                          let e' = init_to_expression e in
                          let data = fst ((
                                       fun (acc, c) e ->
                                       let x' = Exp.CONST c in
                                       (acc@[(x', Term.toTerm e)], c+1)
                                     ) |->> (([], 0), e')) in
                          (* let n = Exp.add Exp.ARRAY n in
                             let n = Exp.add Exp.PTR n in *)         
                          [MALLOC (n, Term.toIndices exps, data, SKIP, loc)]
                       | None -> []
                     ) |>>| ns)
        in
        dbg "TYPE" "Number of Decs:" pl (List.length decs);
        
        (*
        let assns = 
          List.concat (
              (fun (n, b, e, l) ->
                match l with
                | Some exp -> []
                | None ->
                   if b = false && e = C.NO_INIT then
                     []
                   else
                     [(n,e)]
              ) |>>| ns)
        in
   *)

        (new_pointers, decl (* @ decs @ statements *), fvs', [])
      end
   *)

    

  let rec translate (pointers : (string * bool) list) (fvs : (Exp.attr list) V.t) = function
    | [] -> (*begin pn ""; pn "all pointers: "; iterS (fun (x,_) -> pw x) "," pointers; *)
       (SKIP, [])
    | x::xs ->
       (** Initially the labels and their bodies are retrieved. *)
       
       let ((body, labels), loc) =
         match x with
         | C.LABEL (str, statement, loc) when String.length str > 1 && String.sub str 0 1 = "@" ->
            (* let body' = trans_body [] pointers SKIP fvs statement in *)
            (* let (statement', labels1) = translate pointers fvs [statement] in *)

            let (body, labels2) = translate pointers fvs  xs in
            (* let bd = join body [body'] in *)

            let ell = get_labels body in
            let el = try
                snd @@ List.find (fun (lbl, el) -> lbl = str) ell
              with
                Not_found -> []
            in
            let lbl = LABEL (str, el, body, Locs.to_loc loc) in
            
            (** Aggregate the results *)
            ((lbl, labels2), loc)
         (** All the declarations *)
         | C.DEFINITION (definition) ->
            (** Analysis for pointer types to feed the later parts *)
            
            begin
              match definition with
              (** Single or multiple declarations *)
              | C.DECDEF ((specifier, l_init_name), loc) ->
                 (* Cprint.print_def definition; *)
                 begin
                   (** pointers, arrays and/or assignment statements are extracted *)
                   let (new_pointers, (statements : t list), all_fvs, assns, newdecl) =
                     try
                       get_declaration (specifier, l_init_name) pointers loc fvs
                     with
                       e ->
                       pf_s "EXCEP" Cprint.print_def definition; pn_s "EXCEP" "  ::";
                       raise e
                   in
                   
                  
                   (* let all_fvs = add_fvs ~op:false fvs fvs' in *)
                   
                   let (aux_body, _) = translate (pointers @ new_pointers) all_fvs  assns in
                   let (body, labels) = translate (pointers @ new_pointers) all_fvs  xs in
                   (** Aggregate the results *)

                   let deal_voids dcl =
                     let res =
                     match dcl with
                       DECL (a,b,c,d,l) ->
                        
                        if Exp.is_void a then
                          let fvs = fv body in
                          let nm = __N a in
                          try
                            let v' = List.find (function Exp.VAR (v, attr) -> v = nm && List.length attr > 0 | _ -> false) fvs in
                            
                            DECL (v',b,c,d,l), [(a, v')]
                          with
                            _ -> dcl, []
                        else
                          dcl, []
                     | _ -> 
                        dcl, []
                     in
                     
                     res
                   in
                   let res = deal_voids |>>| statements in
                   let statements', new_fvs = List.split res in
                   (* let all_fvs' = add_fvs ~op:true fvs (List.concat new_fvs) in *)
                   let body =
                     (fun body (x, v') ->
                       substitute (Term.EXP x) (Term.EXP v') body
                     ) |->> (body, (List.concat new_fvs))
                               
                   in
                   
                   ((compose (compose (snd @@ join body statements') aux_body) newdecl, labels), loc)
                 end
              (** typedef - rare cases *)
              | C.TYPEDEF (name_group, loc) -> 
                 let (spec, names) = get_name_group name_group in
                 let fptrs = (fun acc (n,_,_,is_fptr) -> if is_fptr then acc@[n] else acc) |->> ([], names) in
                 func_ptrs.contents <- !func_ptrs @ fptrs;
                 if (spec, true) |<- pointers then
                   let p = (fun (n,_,_,_) -> (n, true)) |>>| names in
                   translate (pointers @ p) fvs  xs, loc
                 else
                   let p = (fun (n,_,_,_) -> (n, true)) |>>| ((fun (_,b,c,_) -> b || c) |>- names) in
                   translate (pointers @ p) fvs  xs, loc
              (** Skip the only typedef *)
              | C.ONLYTYPEDEF (specifier, loc) -> begin translate (pointers) fvs  xs, loc end
              (** In other cases, return *)
              | _ -> begin (SKIP, []), dummy_loc end
            end
         | C.COMPUTATION (C.BINARY (C.ASSIGN, left_exp, right_exp), loc) ->
            
            let left = tryit "@@@2" @@ get_var_from_exp pointers left_exp in
            let is_lh_pointer = List.exists (fun (_, _, c) -> (String.concat "." (Exp.var_decode |>>| c)) = left) !ptr_structs in
            if is_lh_pointer then
              let (typ, _, ns) = try List.find (fun (_, _, c) -> (String.concat "." (Exp.var_decode |>>| c)) = left) !ptr_structs  with _ -> raise (StError "TR-2") in
              let left = Exp.var_encode Exp.PTR left in

              if V.mem typ !structures then
                let all_pointer_fields_of_typ = List.filter (fun (_, t, _) -> t = typ) !ptr_structs in
                let new_pointers = (fun (t, _, c) -> (t, "", left::c)) |>>| all_pointer_fields_of_typ in
                ptr_structs.contents <- (!ptr_structs @ new_pointers);
                translate pointers fvs  xs, loc
              else
                translate pointers fvs  xs, loc
            else
              translate pointers fvs  xs, loc
         | _ -> translate pointers fvs  xs, dummy_loc
       in
       (* let is_ret = is_return_exists x in *)
       let body' =
         (* if is_ret && body <> SKIP then
          IF (BExp.UNIT (Term.encode_str "$return", Op.EQ, zero), body, SKIP, SKIP, Locs.dummy )
        else *)
         body
       in
       let body' =
         if body' <> SKIP && is_continue_exists x then
           IF (BExp.UNIT (Term.encode_str "$continue", Op.EQ, zero), body', SKIP, SKIP, Locs.to_loc loc)
         else
           body'
       in
       (* let body' =
        if body' <> SKIP && is_break_exists x then
          IF (BExp.UNIT (Term.encode_str "$break", Op.EQ, zero), body', SKIP, SKIP, Locs.dummy)
        else
          body'
      in *)
       match x with
       | C.DEFINITION (definition) -> (body', labels)
       | C.LABEL (str, statement, _) when String.length str > 1 && String.sub str 0 1 <> "@" ->
          let body' = try
              trans_body labels pointers body' fvs  statement
            with
              e -> pn_s "EXCEP" "|||"; pf_s "EXCEP" Cprint.print_statement statement; pn_s "EXCEP" "|||"; raise e
          in
          (* let body'' = compose return body' in *)
          (body', (str, body')::labels)
       | _ -> begin try
                  (trans_body labels pointers body' fvs  x, labels)
                with
                  e -> pn_s "EXCEP" "|||";  pf_s "EXCEP" Cprint.print_statement x; pn_s "EXCEP" "|||"; raise e
              end

  and trans_body labels pointers (body : t) (fvs:(Exp.attr list) V.t) = function
    (** No operation. No clear. *)
    | C.NOP (_) -> body
    (** Procedure Call *)
    | C.BREAK (loc) -> BREAK (body, Locs.to_loc loc)
    | C.COMPUTATION (C.CAST (_, C.SINGLE_INIT (C.CALL (expression, expressions))), l)
      | C.COMPUTATION (C.CALL (expression, expressions), l) ->
       begin
         let loc = Locs.to_loc l in
         
         let (pre_progs, expressions') =
           (fun (ifs,acc) expression ->
             let (r, expression') = BExp.extract_if_from_exp [] l expression in
             match r with
               Some ifs' -> 
                (ifs@ifs', acc@[expression'])
             | None ->
                (ifs, acc@[expression])
           ) |->> (([],[]), expressions) in

         if List.length pre_progs > 0 then
           (
             
             let all_prog = pre_progs @ [C.COMPUTATION (C.CALL(expression, expressions'), l)] in
             fst @@ translate pointers fvs  all_prog
           )
         else
           try
             
             let (exp, map0) = extract_ref_call ~level:false [] expression in
            
             (* let map =
             (** For function pointer analysis, now we need to provide artificial temporary variable *)
             if List.length map = 0 then
               let nv = C.VARIABLE (new_prog_var ()) in
               [(nv, C.UNARY(C.MEMOF, exp))]
             else
               map
               in *)
             let name = tryit "@@@1" @@ get_var_from_exp pointers exp in
             
             let rec extract = function
               | [] -> ([], map0)
               | [exp] ->
                  
                  let (exp', map) = extract_ref_call ~level:false ~is_call:true map0 exp in
                  
                  ([exp'], map)
               | exp :: exps ->
                  let (exps', map) = extract exps in
                  let (exp', map') = extract_ref_call ~level:false ~is_call:true map exp in
                  (exp' :: exps', map')
             in
             let (expressions', map) = extract expressions in
             (*             List.iter (fun (a,b) -> Cprint.print_expression a; p "==>"; Cprint.print_expression b; pn "") map;*)
             let (fvs', assigns') = get_assignments true l fvs map in
             let t_expressions = (Term.be_typed fvs' to_static l) |>>| (Term.toTerm (packs fvs' ) |>>| expressions') in
             
                        
             
             let prog, assigns =
               try
                 if name = "free" && List.length (t_expressions) > 0 then
                   DISPOSE (List.hd t_expressions, SKIP, Locs.to_loc l), assigns'
                 else (* if List.exists (fun fn -> Exp.to_str fn = name) !functions then *)
                   begin
                     let e_expression = Exp.toExp (packs fvs  ) exp in
                     let expression' = Exp.be_typed fvs' to_static l e_expression in
                     let fhead = Exp.head "" expression' in
                    
                     
                     if Exp.is_funcptr fhead then
                       let selected, assigns = List.partition (function LOOKUP (x, Term.EXP Exp.VAR _, "*",_,_) when x = expression' -> true | _ -> false) assigns' in
                       match selected with
                         LOOKUP (_, x, _, _, l)::_ ->
                          let call = build_call x t_expressions SKIP  loc in
                          call, assigns
                       | _ ->
                          let call = build_call (Term.EXP expression') t_expressions SKIP  loc in
                          (* PROCCALL (Term.EXP expression', t_expressions, new_func (), body, loc) in *)
                          call, assigns
                     else
                       build_call (Term.EXP expression') t_expressions SKIP  loc
                       (* PROCCALL (Term.EXP expression', t_expressions, new_func (), body, loc) *), assigns'
                   end
               with
                 e -> pn "0001"; raise e
             (* else
           body *)
             in
             
             
             let bd = snd @@ join prog assigns in
             
             enblock map bd body loc fvs' fvs
             (* let bd1 = (fun bd (fv, _)->
                      match fv with
                        C.VARIABLE vn
                      | C.CAST (_, C.SINGLE_INIT (C.VARIABLE vn)) ->
                         
                        let fv' = Exp.VAR (vn, V.find vn fvs') in
                        DECL (fv', [], INIT_E, bd, loc)
                      | e ->
                         Cprint.print_expression e; pn "";
                         print_string (Locs.print (Locs.to_loc l));
                         raise (StError "program expression not supported")
               ) |->> (bd, map) in
             pn "======------=";
             BLOCK (bd1, body, loc) *)
             
           with err -> pf_s "EXCEP" pw l.C.filename; pf_s "EXCEP" pi l.C.lineno; raise err
       end
    (** Other Computation (non-procedure call) *)
    | (C.COMPUTATION (C.QUESTION (cond, ifpart, elsepart), l)) ->
       (* let rec to_stmt (exp : C.expression ) : C.statement list =
         match exp with
           C.COMMA el ->
            begin
            let rec aux = function
                [] -> []
              | [e] ->
                 to_stmt e
              | e::es ->
                 to_stmt e @ aux es
            in
            aux el
            end
         | C.QUESTION (exp1, exp2, exp3) ->
            let exp2' = C.BLOCK ({C.blabels = []; C.battrs = []; C.bstmts = to_stmt exp2}, l) in
            let exp3' = C.BLOCK ({C.blabels = []; C.battrs = []; C.bstmts = to_stmt exp3}, l) in

            [C.IF (exp1, exp2', exp3', l)]
         | C.CALL (_, _) as e -> [C.COMPUTATION (e, l)]
         | C.BINARY (op, exp, C.QUESTION (exp1, exp2, exp3))
              when op = C.ASSIGN ||
                     op = C.ADD_ASSIGN ||
                       op = C.SUB_ASSIGN ||
                         op = C.MUL_ASSIGN ||
                           op = C.DIV_ASSIGN ||
                             op = C.MOD_ASSIGN ||
                               op = C.BAND_ASSIGN ||
                                 op = C.BOR_ASSIGN ||
                                   op = C.XOR_ASSIGN ||
                                     op = C.SHL_ASSIGN ||
                                       op = C.SHR_ASSIGN ->
            let exp2' = C.COMPUTATION (C.BINARY (op, exp, exp2), l) in
            let exp3' = C.COMPUTATION (C.BINARY (op, exp, exp3), l) in
            [C.IF (exp1, exp2', exp3', l)]
         | C.BINARY (op, _, _) as e
              when op = C.ASSIGN ||
                     op = C.ADD_ASSIGN ||
                       op = C.SUB_ASSIGN ||
                         op = C.MUL_ASSIGN ||
                           op = C.DIV_ASSIGN ||
                             op = C.MOD_ASSIGN ||
                               op = C.BAND_ASSIGN ||
                                 op = C.BOR_ASSIGN ||
                                   op = C.XOR_ASSIGN ||
                                     op = C.SHL_ASSIGN ||
                                       op = C.SHR_ASSIGN -> [C.COMPUTATION (e, l)]
         
         | C.PAREN (s) ->
            to_stmt s
         | e ->
            let cv = C.VARIABLE (new_prog_var ()) in
            [C.COMPUTATION (C.BINARY (C.ASSIGN, cv, e), l)]
       in
       let ifpart' = C.BLOCK ({C.blabels = []; C.battrs = []; C.bstmts = to_stmt ifpart}, l) in
       let elsepart' = C.BLOCK ({C.blabels = []; C.battrs = []; C.bstmts = to_stmt elsepart}, l) in
       let stmt = C.IF (cond, ifpart', elsepart', l) in
       dbg "WARNING-S" "Original Statement:\n" Cprint.print_statement ss;
       dbg "WARNING-S" "COnverted Statement:\n" Cprint.print_statement stmt; *)
       
       
       let nv = new_prog_var () in
       let cv = C.VARIABLE nv in
       let ev = Exp.VAR (nv, []) in
       let loc = Locs.to_loc l in
       let stmt = C.COMPUTATION (C.BINARY (C.ASSIGN, cv, C.QUESTION (cond, ifpart, elsepart)), l) in
       let body' = trans_body labels pointers body fvs stmt in
       fst @@ build_dec true ev [] INIT_E body' loc []
             
    | (C.COMPUTATION (C.PAREN (e), l))
      | (C.COMPUTATION (C.CAST (_, C.SINGLE_INIT (e)), l)) ->
       trans_body labels pointers body fvs (C.COMPUTATION (e, l))

    | C.COMPUTATION (C.BINARY (C.ASSIGN, l_exp, C.CALL (C.VARIABLE "malloc", sz::_)), l)
      | C.COMPUTATION (C.BINARY (C.ASSIGN, l_exp, C.CAST (_, C.SINGLE_INIT(C.CALL (C.VARIABLE "malloc", sz::_)))), l) ->
       begin
         let (pre_progs, sz') =
           let (r, sz') = BExp.extract_if_from_exp [] l sz in
           begin
             match r with
               Some r' ->
                (r', sz')
             | None -> ([], sz)
           end
         in
         if List.length pre_progs > 0 then
           begin
             let st = C.COMPUTATION (C.BINARY (C.ASSIGN, l_exp, C.CALL (C.VARIABLE "malloc", [sz'])), l) in
             let p' = fst @@ translate pointers fvs (pre_progs @ [st]) in
             compose body p'
           end
         else
           begin
             
             let loc = Locs.to_loc l in
             let rec get_sn_t = function
                 C.TYPE_SIZEOF (C.SpecType tp::_, _) ->
                  (get_original_type (get_type tp), Exp.CONST 1, [])
               | C.BINARY (C.MUL, exp1, exp2) ->
                  let (s, e1, map) = get_sn_t exp1 in
                  let e = Exp.eval @@ Exp.op e1 (Exp.toExp (packs fvs ) exp2) Op.MUL in
                  (s, e, map)
               | C.EXPR_SIZEOF (C.PAREN (C.UNARY (C.MEMOF, _))) ->
                  ("long", Exp.CONST 1, [])
               | C.VARIABLE _ as e ->
                  ("char", Exp.toExp (packs fvs ) e, [])
               | e ->
                  let (e', map) = extract_ref_call [] e in
                  ("char", Exp.toExp (packs fvs ) e', map)
             (* Cprint.print_expression e; pn "";
              raise (StError "Unsupported Malloc -2") *)
             in
             
             let (s, ln, map) = get_sn_t sz in
             let ln' = Exp.be_typed fvs to_static loc ln in

             match l_exp with
               C.VARIABLE _ ->
               let e_lexp' = Exp.toExp (packs fvs) l_exp in
               let e_lexp = Exp.be_typed fvs to_static loc e_lexp' in
               let mlc = MALLOC (e_lexp, ln', body, loc) in
               mlc
             | _ ->
                let (fvs', assigns) =
                  try get_assignments true l fvs map  with _e ->
                    raise _e
                in             
                let nv = new_prog_var () in  (* EXPERIMENTAL REMOVAL *)
                let c_nv = C.VARIABLE nv in
                let attr = if Exp.is_simple_type s then
                             [Exp.SIMPLE (Exp.simple_size s); Exp.PTR]
                           else
                             [Exp.STRUCT (get_original_type s); Exp.PTR] in
                let e_nv = Exp.VAR (nv, attr) in
                
                let c_rest = C.COMPUTATION (C.BINARY (C.ASSIGN, l_exp, c_nv), l) in
                let rest = trans_body labels pointers SKIP (V.add nv attr fvs) c_rest in
                let mlc = MALLOC (e_nv, ln', rest, loc) in
                let (dclr, _) = build_dec true e_nv [] INIT_E mlc loc [] in
                let pr' = snd @@ join dclr assigns in
                enblock map pr' body loc fvs' fvs
           end
       end
    | C.COMPUTATION (expression'', l) ->
       begin
         let (pre_progs, expression') =
           let (r, exp2') = BExp.extract_if_from_exp [] l expression'' in
           begin
             match r with
               Some r' ->
                (r', exp2')
             | None -> ([], expression'')
           end
         in

         
         if List.length pre_progs > 0 then
           begin
             let p' = fst @@ translate pointers fvs (pre_progs @ [C.COMPUTATION (expression', l)]) in
             compose body p'
           end
         else
           
           try
             let loc = Locs.to_loc l in
             let (expression, map) = extract_ref_call [] expression' in
             
             (* Cprint.print_expression expression; pn "";
             iterS (fun (a,b) -> Cprint.print_expression a; p "="; Cprint.print_expression b) "--" map; pn ""; *)
             
             (** the next two computation is independent *)
             
             let (fvs', assigns) =
               try get_assignments true l fvs map  with _e ->
                 Cprint.print_statement (C.COMPUTATION (expression'', l));
                 raise _e in
             let comp, new_fvs1 = fc_get_statement pointers SKIP loc fvs' expression in
             let f_is_free = function
               | [] -> false
               | PROCCALL (name, _, _, _, _)::_::[] -> (* contains *) (Term.toStr name) = "free"
               | _ -> false
             in
             let f_is_alloc = function
               | [] -> false, None
               | PROCCALL (name', args, _, _, _)::_::[] ->
                  begin
                    let name = Term.toStr name' in
                    try
                      name = "malloc" || name = "alloc", Some (List.hd args) 
                    with _ -> false, None
                  end
               | _ -> false, None
             in
             let new_fvs2, tr_statement =
               if f_is_free assigns then
                 begin
                   try
                     match List.hd assigns with
                     | PROCCALL (_, exp, _, _, loc) ->
                        [], DISPOSE (List.hd exp, SKIP, loc)
                     | _ -> join comp assigns
                   with
                     _ -> raise (StError "0003")
                 end
               else
                 begin
                   (** IT IS UNCOMMENTED HERE *)
                   let is_malloc, len = f_is_alloc assigns in
                   let build_array v len body fields loc =
                     (* let elen = Exp.eval len in
                     let fields' = (fun (a,b) -> (a,b)) |>>| fields in *)
                     raise (StError "Unsupported Malloc - 3")
                     (* MALLOC (v, "", Term.EXP elen, body, loc) *)
                   in
                   if is_malloc then
                     match comp with
                     | ASSIGN (v,_,_,_) ->
                        let fields =
                          if List.exists (fun (a,b,c) -> (String.concat "." (__N |>>| c)) = Exp.toStr v) !ptr_structs then
                            let (typ,_,_) = try List.find (fun (a,b,c) -> (String.concat "." (__N |>>| c)) = Exp.toStr v) !ptr_structs with _ -> raise (StError "TR-3") in
                            if V.mem typ !structures then
                              let (s_n, s_f, _) = try V.find typ !structures with Not_found -> raise (StError "TR-4") in
                              let fields = (fun (n,e)-> (n, e)) |>>| s_f in
                              fields
                            else
                              [(__E ("*",[Exp.PTR]), Term.EXP (Exp.NOTHING))]
                          else
                            [(__E ("*",[Exp.PTR]), Term.EXP (Exp.NOTHING))]
                        in
                        begin
                          match len with
                            Some (Term.EXP (Exp.BINOP (Exp.NOTHING, Op.MUL, r)))
                          | Some (Term.EXP (Exp.BINOP (Exp.CONST 1, Op.MUL, r))) ->
                             [], build_array v r SKIP fields loc
                          | Some (Term.EXP (Exp.BINOP (l, Op.MUL, Exp.NOTHING)))
                            | Some (Term.EXP (Exp.BINOP (l, Op.MUL, Exp.CONST 1))) ->
                             [], build_array v l SKIP fields loc
                          | Some (Term.EXP Exp.NOTHING) | Some (Term.NULL) | None ->
                             [], CONS (v, fields, body, loc)
                          | Some (Term.EXP l) ->
                             [], build_array v l SKIP fields loc
                        end
                     | _ ->
                        join comp assigns
                   else (
                     join comp assigns
                   )
                 end
             in

             let fvs'' = (fun fvs v ->
                 let n = __N v in
                 let attr = Exp.get_attributes v in
                 V.add n attr fvs
               ) |->> (fvs', (new_fvs1@new_fvs2)) in (* =============== *)
             enblock map tr_statement body loc fvs'' fvs 
             
           with err ->
             pf_s "EXCEP" pw "COMP";
             pf_s "EXCEP" pw l.C.filename;
             pf_s "EXCEP" pi l.C.lineno; raise err
       end
    | C.BLOCK (block, l) ->
       begin
         (* pn "############";
         Cprint.print_statement (C.BLOCK (block, l)); pn "&&&&&&&&&&"; *)
         let (b, _) = translate pointers fvs block.C.bstmts in
         (* pn "????????????"; *)
         join_at_last body (BLOCK (b, SKIP, Locs.to_loc l))
       end
    | C.SEQUENCE (statement1, statement2, _) ->
       begin pw "<seq>"; let (b, _) = translate [] fvs [statement1; statement2] in b end
    | C.IF (C.PAREN (exp), st1, st2, l) ->
       let if_full = C.IF (exp, st1, st2, l) in
       trans_body labels pointers body fvs if_full
    | C.IF (C.BINARY (C.AND, (C.PAREN (C.BINARY (C.BAND, _, _) as expression1)), expression2), statement1, statement2, l)
      | C.IF (C.BINARY (C.AND, (C.BINARY (C.BAND, _, _) as expression1), expression2), statement1, statement2, l) ->
       let if_full = C.IF (expression1, C.IF(expression2, statement1, statement2, l), statement2, l) in
       trans_body labels pointers body fvs if_full (** @Sep1 *)
    | C.IF (C.BINARY (C.AND, (C.BINARY (C.NE, _, exp) as expression1), expression2), statement1, statement2, l)
      | C.IF (C.BINARY (C.AND, C.PAREN (C.BINARY (C.NE, _, exp) as expression1), expression2), statement1, statement2, l)
         when no_lbl expression1 && no_lbl expression2 && Term.toTerm (packs fvs ) exp = Term.NULL && does_contain_ptr expression2 ->
       (* Cprint.print_expression (C.BINARY (C.AND, (expression1), expression2)); print_string "\n"; *)
       let if_full = C.IF (expression1, C.IF(expression2, statement1, statement2, l), statement2, l) in
       trans_body labels pointers body fvs if_full (** @Sep1 *)
    | C.IF (C.BINARY (C.OR, (C.BINARY (C.EQ, _, exp) as expression1), expression2), statement1, statement2, l)
    | C.IF (C.BINARY (C.OR, C.PAREN (C.BINARY (C.EQ, _, exp) as expression1), expression2), statement1, statement2, l)
         when no_lbl expression1 && no_lbl expression2 && Term.toTerm (packs fvs ) exp = Term.NULL && does_contain_ptr expression2 ->
       (* Cprint.print_expression (C.BINARY (C.AND, (expression1), expression2)); print_string "\n"; *)
       let if_full = C.IF (C.UNARY (C.NOT, expression1), C.IF(expression2, statement1, statement2, l), statement1, l) in
       trans_body labels pointers body fvs if_full (** @Sep1 *)
     
    (* | C.IF (C.BINARY (C.OR, expression1, expression2), statement1, statement2, l) ->
       let if_full = C.IF (expression1, statement1, C.IF(expression2, statement1, statement2, l), l) in
       trans_body labels pointers body fvs if_full
    | C.IF (C.BINARY (C.AND, expression1, expression2), statement1, statement2, l) when no_lbl expression1 && no_lbl expression2 ->
       let if_full = C.IF (expression1, C.IF(expression2, statement1, statement2, l), statement2, l) in
       trans_body labels pointers body fvs if_full *) (** @Aug21 *)
    | C.IF (expression, statement1, statement2, l) ->
       begin
         let loc = Locs.to_loc l in

         pn_s "EXT" "BEGIN";
         dbg "EXT" "If(...):" Cprint.print_expression expression;
         
         let (r, expression') = BExp.extract_if_from_bexp [] l expression in
         
         match r with
           Some pre_progs ->
            let all_prog = pre_progs @ [C.IF (expression', statement1, statement2, l)] in
            dbg "EXT" "If(...) If(###):" Cprint.print_expression expression';
            (* pn "BEGIN*************";
           Cprint.print_expression expression; pn "";
           pn "--->";
           iterS Cprint.print_statement "\n" all_prog; pn "";
        
           pn "END******"; *)
            (* raise Error *)
            let p' = fst @@ translate pointers fvs all_prog in
            compose body p'
         | None ->      
            begin
              try
                dbg "EXT" "If(###):" Cprint.print_expression expression';

                (*   iterS Exp.pprint "-" fvs; pn "***********\n";
                 *)
                
                if_mode.contents <- true;

                let (expression, map) = extract_ref_call ~level:false [] expression' in
                if_mode.contents <- false;

                dbg "EXT" "#=x;  If(###):" Cprint.print_expression expression';

                
                let (fvs', assigns) = try get_assignments false l fvs map with e ->
                                        
                                        dbg "EXCEP" "Problematic Map:" VcpExtract.print_map map; raise e in
                
                begin
                  (*  if !nopointermode then
             IF (expression, trans_body labels pointers SKIP statement1, trans_body labels pointers SKIP statement2, body)
             else *)
                  
                  let exp' = exp_ref pointers expression in
                  
                  let exp =
                    match exp' with
                    | C.VARIABLE (x) ->
                       if (x, false) |<- pointers then
                         C.BINARY (C.NE, exp', C.NOTHING)
                       else
                         C.BINARY (C.NE, exp', C.CONSTANT (C.CONST_INT "0"))
                    | _ -> exp'
                  in
                  
                  let if_full =
                    (* match exp with
                    | C.BINARY (C.OR, exp1, exp2) when !ormode ->
                    let if2 = C.IF (exp2, statement1, statement2, l) in
                    let if1 = C.IF (exp1, statement1, if2, l) in
                    trans_body labels pointers body fvs if1
                    | _ -> *) (** @Aug21 *)
                    let body1 = trans_body labels pointers SKIP fvs' statement1 in
                    
                    let body2 = trans_body labels pointers SKIP fvs' statement2 in
                    if (body1 = SKIP && body2 = SKIP) then
                      SKIP
                    else
                      begin
                        (* iterS Exp.pprint ", " fvs; pn "..."; *)
                        let exp' =
                          try
                            BExp.toBExp (packs fvs ) loc exp
                          with
                            e ->
                            Cprint.print_expression expression; pn "";
                            dbg "EXCEP" "exp':" Cprint.print_expression exp'; pn "";
                            dbg "EXCEP" "Exception in conversion of boolean expression:"  Cprint.print_expression exp;
                            raise e
                        in
                        let exp'' = BExp.be_typed fvs' to_static loc exp' in
                        let body1 = match body1 with
                          | BLOCK _ -> body1
                          | _ -> BLOCK (body1, SKIP, loc)
                        in
                        let body2 = match body2 with
                          | BLOCK _ -> body2
                          | _ -> BLOCK (body2, SKIP, loc)
                        in
                        IF (exp'', body1, body2, SKIP, loc)
                      end
                  in
                  let bd = snd @@ join if_full assigns in
                  (* pprint 5 bd; *)
                  enblock map bd body loc fvs' fvs
                end
              with err ->
                pn_s "EXCEP" "IF ";
                pf_s "EXCEP" pw l.C.filename;
                pf_s "EXCEP" pi l.C.lineno;
                dbg "EXCEP" "Exception at IF:" Cprint.print_statement (C.IF (expression, statement1, statement2, l)); pn "";
                
                raise err
            end
       end
    | C.WHILE (formula, expression, statement, l) ->
       begin

         let loc = Locs.to_loc l in
         pn_s "EXT" "BEGIN";
         dbg "EXT" "While(...):" Cprint.print_expression expression;
         
         let (r, expression') = BExp.extract_if_from_bexp [] l expression in
         dbg "EXT" "While(###):" Cprint.print_expression expression';
         
         match r with
           Some pre_progs ->
            
            (* Cprint.print_expression expression; pn "";
           Cprint.print_expression expression'; pn "";
           pi (List.length pre_progs);
           pn ""; *)
            (* raise Error *)
            let p' = fst @@ translate pointers fvs (pre_progs @ [C.WHILE (formula, expression', statement, l)]) in
            compose body p'
         | None ->
            try
             
              if_mode.contents <- true;
              let patterns = get_pattern fvs loc expression' in
              dbg "EXT" "While(###):" Cprint.print_expression expression';
              
              let (expression, map) = extract_ref_call ~level:false [] expression' in
              dbg "EXT" "While(###):" Cprint.print_expression expression;
              
              
              if_mode.contents <- false;
              let expression = if expression = C.NOTHING then true_g else expression in
              let (fvs', assigns) = try get_assignments false l fvs map with e -> pf_s "EXCEP" Cprint.print_expression expression; raise e in
              
              begin
                let whilefull =
                  match expression with
                  | C.BINARY (C.OR, _, _) when !ormode ->
                     let inner = C.IF (expression, statement, C.BREAK (l), l) in
                     let while_full = C.WHILE (formula, true_g, inner, l) in
                     trans_body labels pointers SKIP fvs' while_full
                  (*        WHILE (true_f, , formula, body) *)
                  | _ ->
                     (* let is_break = is_break_exists statement in
                     let is_cont = is_continue_exists statement in
                     let is_ret = is_return_exists statement in *)
                     (* if is_break then
                       let break_s =
                         if is_ret then
                           C.BINARY (C.AND,
                                     C.BINARY (C.EQ, C.VARIABLE ("$return"), zero_g),
                                     C.BINARY (C.EQ, C.VARIABLE ("$break"), zero_g))
                         else
                           C.BINARY (C.EQ, C.VARIABLE ("$break"), zero_g)
                       in
                       
                       let exp = exp_ref pointers expression in
                       
                       let exp' = if exp = true_g then break_s else C.BINARY (C.AND, break_s, exp) in
                       let inner = trans_body labels pointers SKIP fvs' statement in
                       let inner = join_at_last (snd @@ join SKIP assigns) inner in
                       let inner' =
                         if is_cont then
                           begin
                             ASSIGN (Exp.string_to_var "$continue", zero, inner, loc)
                           end
                         else
                           inner
                       in
                       if !noformulamode && formula = ([],[],true,[],[]) then
                         ASSIGN (Exp.string_to_var "$break", zero, IF (BExp.toBExp (packs fvs ) loc exp', inner', SKIP, SKIP, loc), loc)
                       else
                         ASSIGN (Exp.string_to_var "$break", zero, WHILE (BExp.toBExp (packs fvs ) loc exp', patterns, inner', Formula.translate (packs fvs ) [formula], SKIP, loc), loc)
                     else *) (
                       let inner = trans_body labels pointers SKIP fvs' statement in
                       let inner = match inner with
                           BLOCK (prog, pr2, lc2) -> BLOCK (join_at_last (snd @@ join SKIP assigns) prog, pr2, lc2)
                         | _ -> BLOCK (join_at_last (snd @@ join SKIP assigns) inner, SKIP, loc)
                       in
                       let inner' =
                         (* if is_cont then
                           ASSIGN (Exp.string_to_var "$continue", zero, inner, loc)
                         else *)
                           inner
                       in
                       let exp' = exp_ref pointers expression in
                       if !noformulamode && formula = ([],[],true,[],[]) then
                         IF (BExp.toBExp (packs fvs ) loc exp', inner', SKIP, SKIP, loc)
                       else(
                         dbg "EXT" "exp':" Cprint.print_expression  exp';
                         let exp' = BExp.toBExp (packs fvs ) loc exp' in
                         let exp'' = BExp.be_typed fvs' to_static l exp' in
                         let inner' = match inner' with
                           | BLOCK _ -> inner'
                           | _ -> BLOCK (inner', SKIP, loc)
                         in
                         WHILE (exp'', patterns, inner', Formula.translate (packs fvs ) [formula], SKIP, loc)
                       )
                     )
                in
                let new_fvs, finalwhile = join whilefull assigns in
                pn_s "EXT" "While DONE";
                enblock map finalwhile body loc fvs' fvs
              end
            with err ->
              pf_s "EXCEP" pw "WHILE";
              pf_s "EXCEP" pw l.C.filename;
              pf_s "EXCEP" pi l.C.lineno;
              raise err
       end
    | C.DOWHILE (formula, expression, statement, l) ->
       begin
         try
           if_mode.contents <- true;
           let (expression, map) = extract_ref_call [] expression in
           if_mode.contents <- false;
           let (fvs', assigns) = get_assignments false l fvs map in
           let whilebody = trans_body labels pointers body fvs' (C.WHILE (formula, expression, statement, l)) in
           begin
             let whilefull =
               match whilebody with
               | WHILE (_, _, body, _, _, _) -> join_at_last whilebody body
               | ASSIGN (_, _, WHILE _, _) -> join_at_last whilebody body
               | IF _ -> whilebody
               | _ -> whilebody
             in
             snd @@ join whilefull assigns
           end
         with err -> raise err
       end
    | C.FOR (formula, for_clause, expression_cond0, expression2, statement, l) ->
       begin
         try
           let ((fc_body : t), map_init, new_fvs) =
             match for_clause with
             | C.FC_EXP (expression) ->
                begin
                  if_mode := true;
                  let (expression, map) = extract_ref_call [] expression in
                  if_mode := false;
                  let assn, new_fvs = fc_get_statement pointers SKIP (Locs.to_loc l) fvs expression in
                  (assn, map, new_fvs)
                end
             | C.FC_DECL (definition) ->
                begin
                  match definition with
                  | C.DECDEF ((specifier, l_init_name), _) ->
                     let (l_init_name, map) = extract_ref_call_l_init_name [] l_init_name in
                     begin
                       let type_name = get_type_name_from_specifier specifier in
                       let ns = get_var_name_from_init_name false fvs l l_init_name specifier in
                       (** pointer and assignment statements are extracted *)
                       let (p, s) =
                         if (type_name, true) |<- pointers then
                           ((fun (n,_,_,_) -> (n, false)) |>>| ns,
                            (fun (n,_,e,_) -> ASSIGN (n, get_expression fvs e, SKIP, Locs.to_loc l)) |>>| ns)
                         else
                           let ns' = ((fun (n,b,_,_) -> b) |>- ns) in
                           if !nopointermode then
                             ((fun (n,b,_,_) -> (n, false)) |>>| ns',
                              (fun (n,_,e,_) -> ASSIGN (n, get_expression fvs e, SKIP, Locs.to_loc l)) |>>| ns) (* use ns' for filter *)
                           else
                             ((fun (n,b,_,_) -> (n, false)) |>>| ns',
                              (fun (n,_,e,_) -> ASSIGN (n, get_expression fvs e, SKIP, Locs.to_loc l)) |>>| ns') (* use ns' for filter *)
                       in
                       (snd @@ join SKIP s, map, [])
                     end
                  | _ -> pw "<EXCEPTION DECDEF>"; (SKIP, [], [])
                end
           in
           let (expression_cond, map_cond) = extract_ref_call [] expression_cond0 in
           let (expression_inc, map_inc) = extract_ref_call [] expression2 in
           let (fvs1, (assigns_init:t list)) = get_assignments false l fvs map_init in
           let (fvs2, (assigns_cond:t list)) = get_assignments false l fvs1 map_cond in
           let (fvs', (assigns_inc:t list)) = get_assignments false l fvs2 map_inc in
           let while_statement = C.WHILE(formula, expression_cond, statement, l) in
           let stmt_inc, new_fvs1 = fc_get_statement pointers SKIP (Locs.to_loc l) fvs' expression_inc in
           let stmt_cond, new_fvs2 = fc_get_statement pointers SKIP (Locs.to_loc l) fvs' expression_cond in
           let init_set = snd @@ join fc_body assigns_init in
           let init_full = join_at_last (snd @@ join SKIP assigns_cond) init_set in
           let fvs'' = add_fvs fvs' (new_fvs@new_fvs1@new_fvs2) in
           (* let cont = join cont assigns in (** BE SURE TO REMOVE IT *) *)
           begin
             let for_full = trans_body labels pointers body fvs'' while_statement in
             let for_full' =
               match for_full with
               | WHILE (exp, patterns, BLOCK (stmt, bp, bl), formula, rest, _) ->
                  let all_inc = snd @@ join stmt_inc assigns_inc in
                  let all_cond = snd @@ join stmt_cond assigns_cond in
                  
                  let body_n_inc = BLOCK (join_at_last all_cond (join_at_last all_inc stmt), bp, bl) in
                  
                  (*            let new_body = join_at_last (join SKIP assigns_cond) body_n_inc in
                   *)            
                  join_at_last (WHILE (exp, patterns, body_n_inc, formula, rest, Locs.to_loc l)) init_full
               | IF _ -> for_full
               | _-> for_full
             in
             for_full'
           end
         with err -> raise err
       end
    | C.GOTO (str, l) ->
       let e = List.exists (fun (x,_) -> x = str) labels in
       if e then
         let (_, b) = try List.find (fun (x,_) -> x = str) labels with _ ->
                        print_endline (Locs.print (Locs.to_loc l));
                        raise (StError "TR-5")
         in
         b
       else
         body
    | C.RETURN (expression, l) ->
       begin
         let (r, expression') = BExp.extract_if_from_exp [] l expression in
         
         match r with
           Some pre_progs ->
            let p' = fst @@ translate pointers fvs  (pre_progs @ [C.RETURN (expression', l)]) in
            compose body p'
         | None ->
            begin
              let (expression, map) = extract_ref_call ~level:false [] expression' in
              
              let (fvs', assigns) = get_assignments false l fvs map in
              let loc = Locs.to_loc l in
              
              try
                let ee = Term.toTerm (packs fvs ) expression in
                let res = snd @@ join (RETURN (Term.be_typed fvs' to_static l ee, SKIP, loc)) assigns in
                let res' = enblock map res body loc fvs' fvs  in
               
                res'
              with
                ExpToBExp ->
                let be = BExp.toBExp (packs fvs ) loc expression in
                let p = IF (be, RETURN (Term.one, SKIP, loc), RETURN (Term.zero, SKIP, loc), body, loc) in
               
                snd @@ join p assigns
            end
       end
    | C.CONTINUE (l) -> CONTINUE (body, Locs.to_loc l)
    | _ -> body
  ;;

  let rec static_to_global = function
    | SKIP -> (SKIP, [])
    | FAIL -> (FAIL, [])
    | ASSERT (a, p, b) ->
       let p', sts = static_to_global p in 
       (ASSERT (a, p', b), sts)
    | ASSIGN (v, e_term, p, l) ->
       let p', sts = static_to_global p in
       (ASSIGN (v, e_term, p', l), sts)
    | IF (b, p1, p2, p, l) ->
       let p1', sts1 = static_to_global p1 in
       let p2', sts2 = static_to_global p2 in
       let p', sts = static_to_global p in
       (IF (b, p1', p2', p', l), sts1@sts2@sts)
    | WHILE (b, bs, p1, a, p, l) ->
       let p1', sts1 = static_to_global p1 in
       let p', sts = static_to_global p in
       (WHILE (b, bs, p1', a, p', l), sts1@sts)
    | PROCCALL (fname, ps, i, p, l) ->
       let p', sts = static_to_global p in
       (PROCCALL (fname, ps, i, p', l), sts)
    | CONS (e, a, p, l) ->
       let p', sts = static_to_global p in
       (CONS (e, a, p', l), sts)
    | MUTATION (t1, a, t2, p, l) ->
       let p', sts = static_to_global p in
       (MUTATION (t1, a, t2, p', l), sts)
    | LOOKUP (v, t1, a, p, l) ->
       let p', sts = static_to_global p in
       (LOOKUP (v, t1, a, p', l), sts)
    | DISPOSE (t, p, l) ->
       let p', sts = static_to_global p in
       (DISPOSE (t, p', l), sts)
    | MALLOC (v, tl, p, l) ->
       let p', sts = static_to_global p in
       (MALLOC (v, tl, p', l), sts)
    | BLOCK (t, p, l) ->
       let t', sts1 = static_to_global t in
       let p', sts2 = static_to_global p in
       (BLOCK (t', p', l), sts1@sts2)
    | DECL (a, len, init_data, p, l) ->
       let p', sts = static_to_global p in
       if String.get (Exp.toStr a) 0 = '^' then
         (p', sts@[DECL (a, len, init_data, SKIP, l)])
       else
         (DECL (a, len, init_data, p', l), sts)
    | RETURN (i, p, l) ->
       let p', sts = static_to_global p in
       (RETURN (i, p', l), sts)
    | BREAK (p, l) ->
       let p', sts = static_to_global p in
       (BREAK (p', l), sts)
    | CONTINUE (p, l) ->
       let p', sts = static_to_global p in
       (CONTINUE (p', l), sts)
    | LABEL (lbl, vs, p, l) ->
       let p', sts = static_to_global p in
       (LABEL (lbl, vs, p', l), sts)
    | e -> (e, [])
  ;;
end;;

module Structure = struct
  type t = string * (Exp.t * Term.t) list * Exp.t list option

  let all_struct_names : string list ref = ref []
                                        
  let pprint ((name, l_fields, t) : t) =
    p name;
    (
      match t with
        None -> pn ""
      | Some fs ->
         p "<"; iterS Exp.pprint "," fs; pn ">");
    pt "" 1;
    iterS (fun (l_field_name, value) -> Exp.pprint l_field_name; if value <> Term.NULL then (p "="; Term.pprint value)) ", "  l_fields; pn "";;

  let print ((name, l_fields, t) : t) =
    p "(";
    p ("\"" ^ name ^ "\"");
    p ",";
    print_list (fun (q,r) -> p "("; Exp.print q; p ","; Term.print r; p ")" ) l_fields;
    p ",";
    match t with
      None -> p "None"; p ")"
    | Some tl -> p "Some "; print_list Exp.print tl;
                 p ")"
                 
  let clone (_, fields, t) new_name =
    (new_name, fields, t)
    
  let exists l_structs name =
    V.mem name l_structs

  let find l_structs name =
    try
      V.find name l_structs
    with
    | _ -> raise (StError "Structure")

  let expand parent_field_name (l_field_name, exp) =
    (parent_field_name::l_field_name, exp)

  let rec cross_str n_str x_xs : Exp.t list list = []

  let get_union_field o_l_fields =
    o_l_fields

  let packs  = (V.empty, Block.to_static, !Block.array_aliases, !Block.structures, !Block.s_aliases)
    
  let set_type structures (sn, sfs, st) : t =
    match st with
      None ->
       begin
         try
           let flds = fst |>>| (List.filter (fun (fn, _) ->
                                    Exp.is_struct fn && Exp.get_struct_name fn = sn
                                  ) sfs) in
           (sn, sfs, Some flds)
         with
           _ ->
           (sn, sfs, None)
       end
    | Some _ -> (sn, sfs, st)

  let is_struct_type x =
    let rec is_struct_type' = function
      | C.Tstruct (string, field_group_list_option, _) -> true
      | C.Tunion (string, field_group_list_option, _) -> true
      | C.Tenum (string, field_group_list_option, _) -> true
      | _ -> false
    in
    match x with
      C.SpecTypedef -> false        
    | C.SpecCV cvspec -> false
    | C.SpecAttr attribute -> false
    | C.SpecStorage storage -> false
    | C.SpecInline -> false
    | C.SpecType typeSpecifier -> is_struct_type' typeSpecifier
    | C.SpecPattern string -> false

  let build_enum specs l_fields =
    let xs, fvs, _ =
    (fun (acc, fvs, i) (name, exp, loc) ->
      let i', texp =
        match exp with
          C.NOTHING -> Term.op i Term.one Op.ADD, i
        | _ ->
           let ti = Term.eval @@ Term.toTerm Block.dummy_packs exp in
           
           Term.op ti Term.one Op.ADD, ti
      in
      let simple_type = Exp.SIMPLE (Exp.simple_size "long") in
      
      let decl, fvs' = Block.build_dec false (Block.__E (name, [simple_type])) [] (Block.INIT_S texp) Block.SKIP (Locs.to_loc loc) specs  in
      (acc @ [decl], fvs@[fvs'], i')) |->> (([], [], Term.zero), l_fields) in
    xs, fvs
    
  let rec get_struct pointers (structures : t V.t) ptr_struct name data (fvs : Exp.attr list V.t) loc : t * (string * string * Exp.t list) list * t list * Block.t list * Exp.t list =
    let name = if name = "" then new_prog_var () else name in

    match data with
    | None -> (name, [], None), [], [], [], []
    | Some (l_field_group : C.field_group list) ->
       let p_field ((l_spec:C.specifier), (l_name_exp: (C.name * C.expression option) list)) =
         (** No nested structure *)
         (** s_type is the type of the current field list *)
         pn_s "STRUCTURE" "New Fields are being added";
         let s_type = Block.get_type_name_from_specifier l_spec in
         dbg "STRUCTURE" "s_type:" p s_type;
         let (_, lens) = if V.mem s_type !Block.array_aliases then V.find s_type !Block.array_aliases else ("", []) in
         (** translate a field *)
         let rec p_a_field is_ext (((field_name, decl_type, u_1, u_2), o_exp) : C.name * C.expression option)  =
           (** attrs' = [PTRPTR|PTR|ARRAY ( l* )|PROTO]* *)
           let attrs'' =
             try
               Block.get_basic_type_info decl_type fvs 
             with
               e ->
               pn name;
               raise e
           in
           
           let attrs' =
             if List.exists (function Exp.ARRAY _ -> true | _ -> false) attrs'' then
               (function Exp.ARRAY len -> Exp.ARRAY (len @ lens) | attr -> attr
               ) |>>| attrs''
             else if List.length lens > 0 then
               attrs'' @ [(Exp.ARRAY lens)]
             else
               attrs''
           in
           
           (** attrs  = [PTRPTR|PTR|ARRAY|PROTO|STRUCT <struct>]* *)
           let b1 = List.exists ((=)s_type) !all_struct_names in
           let b2 = exists structures s_type in
           let b3 = List.exists (fun (a,_) -> a = s_type) !Block.func_ptr_types in
           let b4 = List.exists is_struct_type l_spec in
           dbg "STRUCTURE" "is field type a fp?" pb b3;

           let rec is_simple s =
             if Exp.is_simple_type s then
               true, s
             else
               if V.mem s !Block.s_aliases then(
                 let y = V.find s !Block.s_aliases in
                 if y = s then
                   (
                     dbg "WARNING" "Exceptional type:" p y; 
                     (false, s)
                   )
                 else
                   is_simple y
               )
               else
                 false, ""
           in
           let is_s_simple, s_type' = is_simple s_type in
           dbg "STRUCTURE" "is_s_simple:" pb is_s_simple;
           
           let attrs'' =
             if is_s_simple then
               (Exp.SIMPLE (Exp.simple_size s_type'))::attrs'
             else
               (
                   if b1 || b2 || b4 then Exp.STRUCT (Block.get_original_type s_type)::attrs' else attrs'
               )
           in
               
           let attrs =
             if b3 then
               let attr1 = ((<>) Exp.PTR) |>- attrs'' in
               let arr_attr, arr'_attr = List.partition (function Exp.ARRAY _ -> true | _ -> false) attr1 in
               let attr = Block.get_func_ptr_attr s_type in
               match attr with
                 Exp.FUNC (ret, param_types)::_ ->
                  if List.exists ((=)Exp.PTR) attrs'' then
                    arr_attr @ [Exp.FUNCPTR (ret @ arr'_attr, param_types)]
                  else
                    raise (StError "func type field does not make sense")
               | Exp.FUNCPTR (ret, param_types)::_ ->
                  arr_attr @ [Exp.FUNCPTR (ret @ arr'_attr, param_types)]
               | _ -> attr @ arr_attr 
             else
               attrs''
           in
           dbg "STRUCTURE" "attr:" Exp.print (Exp.VAR (field_name, attrs));
           
           let attrs_for_check = Block.build_attributes false field_name pointers fvs  loc decl_type l_spec in

           let rec is_same ys = function
               [] -> List.length ys = 0
             | x::xs ->
                let (y, ys') = List.partition ((=) x) xs in
                match y with
                  [] -> false
                | _ -> if List.length ys' = 0 then false else is_same ys' xs
           in

           if not (is_same attrs_for_check attrs) then
             begin
               pn_s "DIFF_ATTR" "$$$ Different attributes";
               pf_s "DIFF_ATTR" Exp.print (Exp.VAR (field_name, attrs));
               pf_s "DIFF_ATTR" Exp.print (Exp.VAR (field_name, attrs_for_check));
             end;

           let rec nested_analysis = function
               C.ARRAY (dt, _, _)
             | C.PTR (_, dt)
               | C.PROTO (dt, _, _) ->
                nested_analysis dt
             | C.JUSTBASE ->
                
                pn_s "STRUCTURE" "@C";
                (** Check possibility of existing nested structure *)
                let is_union, nested_struct, nested_enum =
                  List.fold_left
                    (fun (is_union, acc, nested_enum) spec_elem ->
                      if acc = None then
                        match spec_elem with
                          C.SpecType (C.Tstruct (lname, o_l_field_group, _)) ->
                           false, Some (lname, o_l_field_group), []
                        | C.SpecType (C.Tunion (lname, o_l_field_group, _)) ->
                           Block.unions := lname::!Block.unions;
                           true, Some (lname, get_union_field o_l_field_group), []
                        | C.SpecType (C.Tenum (lname, o_l_field_group, _)) ->
                           Block.enums := lname::!Block.enums ;
                           let specs : C.specifier = [C.SpecType C.Tint] in
                           let (o_l_field_group' : C.field_group list option), l_fields =
                             match o_l_field_group with
                               None -> None, []
                             | Some l_field_group ->
                                let l_field_group' : C.field_group list =
                                  (fun (str, e, l) ->
                                    let name : C.name = (str, C.JUSTBASE, [], l) in
                                    let o_expression : C.expression option =
                                      match e with
                                        C.NOTHING -> None
                                      | _ -> Some e
                                    in
                                    (specs, [(name, o_expression)])
                                  ) |>>| l_field_group in
                                Some l_field_group', l_field_group
                           in
                           
                           false, Some (lname, o_l_field_group'), nested_enum @ [(specs, l_fields)]
                        | _ -> is_union, acc, nested_enum
                      else
                        is_union, acc, nested_enum
                    ) (false, None, []) l_spec in
                begin
                  match nested_struct with
                    Some (lname, o_l_field_group) ->
                     begin
                       pn_s "STRUCTURE" "@C1";
                       match o_l_field_group with
                         Some _ ->
                          (** Nested struct declaration *)
                          let name' =
                            if lname = "" then
                              let dummy = Exp.VAR ("", attrs) in
                              if Exp.is_struct dummy then
                                Exp.get_struct_name dummy
                              else new_prog_var ()
                            else
                              lname
                          in
                          
                          let (local_struct, p_s, aux_structs, aux_blocks, fvs') = get_struct pointers structures ptr_struct name' o_l_field_group fvs loc in
                          let enum_stmts, fvs = (fun _ nested_enum ->
                              match nested_enum with
                                (_, []) -> [], []
                              | (specs, field_groups) ->
                                 build_enum specs field_groups
                            ) |->> (([], []), nested_enum)
                          in
                          let fvs'' = fvs' @ fvs in
                          let field = (Exp.VAR (field_name, [Exp.NESTED;Exp.STRUCT (Block.get_original_type name')]@@@attrs), Term.NULL) in
                          (field, aux_structs @ [local_struct], aux_blocks@enum_stmts, p_s, fvs'')
                       | None ->
                          
                          let is_struct_exist = V.mem s_type structures in
                          if is_struct_exist then
                            begin
                              pn_s "STR" "Found a Structure";
                              let (str_name, l_fields, s_type) = find structures s_type in
                              let p_ss = List.filter (fun (f_type, s_name, f_name) -> str_name = s_name) ptr_struct in
                              let this_field = (field_name, attrs) in
                              let field_val = Term.NULL in
                              let p_s = (fun (f_type, s_name, f_name)-> (f_type, name, (Exp.VAR (field_name, attrs))::f_name)) |>>| p_ss in
                              let aux_struct : t list = [] in
                              ((Exp.VAR this_field, field_val), aux_struct, [], p_s, [])
                            end
                          else
                            begin
                              let exp =
                                match o_exp with
                                | None ->
                                   if (s_type, true) |<- pointers then
                                     begin
                                       Term.NULL
                                     end
                                   else
                                     begin
                                       Term.EXP (Exp.NOTHING)
                                     end
                                | Some exp ->
                                   Term.toTerm Block.dummy_packs exp
                              in
                              (Exp.VAR (field_name, attrs), exp), [], [], [], []
                            end
                     end
                  | None ->
                     pn_s "STRUCTURE" "@C2";
                     let is_struct_exist = V.mem s_type structures in
                     if is_struct_exist then
                       begin
                         pn_s "STRUCTURE" "@C2A";
                         pn_s "STR" "Found a Structure";
                         let (str_name, l_fields, s_type) = find structures s_type in
                         let p_ss = List.filter (fun (f_type, s_name, f_name) -> str_name = s_name) ptr_struct in
                         let this_field = Exp.VAR (field_name, attrs) in
                         let field_val = Term.NULL in
                         let p_s' = (fun (f_type, s_name, f_name)-> (f_type, name, (Exp.VAR (field_name, attrs))::f_name)) |>>| p_ss in
                         let aux_struct : t list = [] in
                         ((this_field, field_val), aux_struct, [], p_s', [])
                       end
                     else
                       begin
                         pn_s "STRUCTURE" "@C2B";
                         let exp =
                           match o_exp with
                           | None ->
                              if (s_type, true) |<- pointers then
                                begin
                                  Term.NULL
                                end
                              else
                                begin
                                  Term.EXP (Exp.NOTHING)
                                end
                           | Some exp ->
                              Term.toTerm Block.dummy_packs exp
                         in
                         let ee = Exp.VAR (field_name, attrs) in
                       
                         (ee, exp), [], [], [], []
                       end
                end
             | _ ->
                let v = Exp.VAR (field_name, attrs) in
                (Exp.VAR (field_name, attrs), Term.NULL), [], [], [(s_type, name, [v])], []
                
           in
           nested_analysis decl_type
         in
         let res = ((p_a_field false) |>>| l_name_exp) in
         let fields = (fun (a,_,_,_,_) -> a) |>>| res in
         let aux_structs = List.concat ((fun (_,b,_,_,_) -> b) |>>| res) in
         let aux_stmts = List.concat ((fun (_,_,c,_,_) -> c) |>>| res) in
         let p_s = List.concat ((fun (_,_,_,d,_) -> d) |>>| res) in
         let fvs' = List.concat ((fun (_,_,_,_,e) -> e) |>>| res) in
         fields, aux_structs, aux_stmts, p_s, fvs'
       in
       let x = (p_field |>>| l_field_group) in (* specifier * (name * expression option) list *)
       let xxx : (Exp.t * Term.t) list = List.concat ((fun (a,_,_,_,_) -> a) |>>| x) in
       let current_struct = (name, xxx, None) in
       let p_s = (List.concat ((fun (a,b,c,d,_) -> d) |>>| x)) in
       let aux_blocks : Block.t list = (List.concat ((fun (a,b,c,d,_) -> c) |>>| x)) in
       let aux_structs : t list = (List.concat ((fun (a,b,c,d,_) -> b) |>>| x)) in
       let current_struct' = set_type aux_structs current_struct in
       let fvs' = List.concat ((fun (_,_,_,_,e) -> e) |>>| x) in
         
       (current_struct', p_s, aux_structs, aux_blocks, fvs')
end;;

module Procedure = struct
  type t = Exp.t * Block.t list * Block.t
  module C = C
           
  let f_count = ref 1

  let to_var = function
      Block.ASSIGN (var, _, _, _)
    | Block.CONS (((Exp.VAR _) as var), _, _, _)
      | Block.CONS (((Exp.BINOP(Exp.VAR _, _, _)) as var), _, _, _)
      | Block.DECL (var, _, _, _, _)
      | Block.SARRAY (var, _, _, _, _)
      | Block.MALLOC (var, _, _, _) ->  var
    | x ->
       raise (StError "Procedure")

  let get_name (a, _, _) = a

  let pprint ((a, b, c):t) =
    let b = to_var |>>| b in
    pn ""; p (Block.extra ""); Exp.pprint a; pw "("; iterS Exp.pprint ", " b; pn " )";
    Block.pprint 0 c

  let print (a, b, c) =
    p "(";
    Exp.print a;
    p ","; 
    print_list Block.print b;
    p ",";
    Block.print c;
    p ")"
    
  let fptransform pointers structures fvs  ((spec, (s_name, dt, xx, l)), (block':C.block)) cabsloc fpdata =

    let block : C.block = VcpFpOnC.fp_transform_function fpdata s_name structures block' in
    block
  ;;


  let translate pointers structures fvs  ((spec, (s_name, dt, xx, l)), (block:C.block)) cabsloc fpdata : (t * Block.t list) =
    dbg "PROC" ("Translating (" ^ (string_of_int !f_count) ^ "): ") p (s_name);
    (* pn s_name; pn "=========="; *)

    f_count.contents <- !f_count + 1;
    let loc = Locs.to_loc l in
    let rec get_params _fvs = function
      | C.PROTO (_d, l_single_name, _) ->
         
         let process_single_name (acc, _fvs) (specifier, name) =
           (** Convert name into init_name list *)
           let l_init_name = [(name, C.NO_INIT)] in
           
           let (a, b, fvs', assns, _) = Block.get_declaration ~is_param:true (specifier, l_init_name) pointers l _fvs  in
           
           let b' : Block.t list =
             (fun b'' ->
               match b'' with
                 Block.DECL (x, len, data, p, l) ->
                  let x' = Exp.var_add Exp.PARAM x in
                  Block.DECL (x', len, data, p, l)
               | _ ->
                  b''
             ) |>>| b in
           (acc@[(a, (b', assns))], fvs')
         in
         
         let (a, fvs') =  process_single_name |->> (([], fvs), l_single_name) in

         (* if s_name = "sha1write_u8" then
           Block.print_fvs fvs'; *)
           
         let a = (fun (x, (y, z)) ->
             (x, List.hd y) (** ass is removed *)
           ) |>>| a in
         let aa = (fun (_, y) ->
             match y with
               Block.ASSIGN (Exp.VAR (x, _), _, _, _) -> raise (StError "Param should not be Assignment")
             | Block.DECL (Exp.VAR (x, _), _, _, _, _) ->
                if x = "" then
                  false
                else
                  true
             | _ -> true
           ) |>- a in
         
         let ((new_pointers', param_blocks), fvs') = get_params fvs' _d in
         let (aaa, bbb) = List.split aa in 
         ((aaa@new_pointers', param_blocks@bbb), fvs')
      | C.PTR (_, dt) -> get_params _fvs dt
      | C.JUSTBASE -> (([], []), _fvs)
      | C.PARENTYPE (_, _d, _) ->
         get_params _fvs _d
      | _ -> pw "WARNING in FUNCDEF"; (([], []), _fvs)
    in

    let ((new_pointers', param_blocks), all_fvs) = get_params fvs dt in
    
    let new_pointers = List.flatten new_pointers' in

    let (_,decl,_,_,_) = Block.get_declaration (spec, [(s_name, dt, xx, l), C.NO_INIT]) pointers l fvs in 
    let func_name = match decl with Block.DECL (v, _,_,_,_)::_ -> v | _ -> raise Error in
    
    let (s_func_name, func_attr) = Block.__V @@ func_name in
    
    let (s_func_name', func_attr'') =
      if Block.is_static func_attr || s_func_name = "main" 
      then (Block.to_static s_func_name, func_attr)
      else (s_func_name, func_attr)  in

    let func_attr' = (func_attr'' @@@ func_attr) in
    let func_name = (s_func_name', func_attr') in
    
    let params =
      (*
      (fun x ->
        match x with
      ) |>>| *)
      param_blocks in
    
    (* let fvs' = List.flatten lfvs' in *)
    
    pn_s "TRANS" (s_name ^ " BEGINs");
    
    let stmts = (* (transBlock f_e f_s *) block.C.bstmts in
    let return = C.RETURN (C.CONSTANT (C.CONST_INT "0"), l) in
    dbg "TRANS" "Transformed Program:\n" (iterS Cprint.print_statement "\n") (stmts @ [return]);
    pn_s "TRANS" (s_name ^ " ENDs");

    Block.in_func := s_name;
    let (body, _) =
      try
        Block.translate (pointers @ new_pointers) all_fvs  (stmts @ [return])
      with
        e ->
        
        dbg "EXCEP" "Confirmation:" pn cabsloc.C.filename;
        
        raise e
    in
    Block.in_func := "";
    let body'' = Block.BLOCK (body, Block.SKIP, loc) in

    let body''', sts = Block.static_to_global body'' in
    dbg "PROC" "" p ".";

    dbg "TRANS" "Translated Program:\n" (Block.pprint 0) body''';
    
    (* pw "PROC"; pw name; pw ":"; iterS p "," params; pn ""; *)
    ((Block.__E func_name, params, body'''), sts)
    
end;;

module Global = struct
  type t =
    | NA
    | STRUCT of Structure.t * Locs.t
    | STATEMENT of Block.t
    | PROC of Procedure.t * Formula.t * Formula.t * (Formula.t * Formula.t) list * Locs.t
    | FFILE of (string * string)

  module V = Map.Make(String)
           
  let rec pprint = function
    | NA -> ()
    | STATEMENT a -> begin Block.pprint 0 a end
    | PROC (a, b, c, d, _) ->
       begin
         Formula.pprint b;
         Procedure.pprint a;
         Formula.pprint c;
         pn "";
       end
    | STRUCT (st, _) -> Structure.pprint st
    | FFILE (st, _) ->
       let fin = open_in st in
       let pd : t = Marshal.from_channel fin in
       close_in fin;
       pprint pd

  let print = function
      NA -> p "NA"
    | STRUCT (st, l) ->
       p "Global.STRUCT ((";
       Structure.print st;
       p "),(";
       p (Locs.print l);
       p "))\n"
    | STATEMENT bl ->
       p "STATEMENT (";
       Block.print bl;
       p ")\n"
    | PROC (a, b, c, d, l) ->
       p "PROC ((";
       Procedure.print a;
       p "),";
       p "[([],[],[],[])]";
       p ",";
       p "[([],[],[],[])]";
       p ",";
       p "[]";
       p ",";
       p (Locs.print l);
       p ")\n"
    | FFILE (st, fn) -> p ("FFILE (\"" ^ st ^ "\", \"" ^ fn ^ "\"");;
       
  let add_to_structures st s =
    let (s_name, fld, _) = s in
    dbg "STRUCTURE" "New Structure added:" p s_name;

    let res =
      if V.mem s_name st then(
        let (_, flds, _) = V.find s_name st in
        let res =
          if List.length flds = 0 then(
            
            V.add s_name s (V.remove s_name st)
          )
          else
            st
        in
        res
      )
      else
        V.add s_name s st
    in
    
   (* try
      let (_, flds, _) = V.find s_name st in
       if s_name = "child_process" then(
         pi (List.length flds)
       );
       res
    with
      _ -> *)
      res

    
  

  
  let func_to_var name spec dt pointers fvs  (cabsloc : C.cabsloc ) : Exp.t =

    let ftype = Block.build_attributes true name pointers fvs  cabsloc dt spec in    
    Exp.VAR (name, ftype)
    (* let v =
      if Block.has_static spec then
        let file = (* flatten_path @@ make_path !funcdir cabsloc.C.filename name *) !cfunc ^ "%" ^ name in    
        Block.__E (file, Exp.STATIC::ftype)
      else if name = "main" then
        begin
          let file = (* flatten_path @@ make_path !funcdir cabsloc.C.filename name *) !cfunc ^ "%" ^ name in    
          pw "##"; pw cabsloc.C.filename; pn file;
          Block.__E (file, ftype)
        end
      else
        Block.__E (name, ftype)
    in *);;
    

  let get_array_dimension dt : Exp.t list =
    let rec aux = function
    | C.JUSTBASE -> []
    | C.PARENTYPE _ -> []
    | C.ARRAY (dt', _, exp) ->
       (Exp.eval @@ Exp.toExp (V.empty, V.empty, V.empty, V.empty, V.empty) exp) :: aux dt'
    | C.PTR (_, dt') ->
       aux dt'
    | C.PROTO (dt', _, _) ->
       aux dt'
    in
    List.rev (aux dt)
    
    
  let unfold_struct sofar_structures pointers ptr_struct loc fvs specs =
    
    let rec aux = function
    | [] -> ("nostruct", [], None), [], [], [], fvs
    | (y::ys) ->
       match y with
       
       | C.SpecType (C.Tstruct (str, o_l_fields, _)) ->
          
          let sofar_structures' = add_to_structures sofar_structures (str, [], None) in
          let (current_structs, p_s, aux_structs, aux_blocks, fvs') : Structure.t * (string * string * Exp.t list) list * Structure.t list * Block.t list * Exp.t list =
            Structure.get_struct pointers sofar_structures' ptr_struct str o_l_fields fvs  loc in
          let fvs'' = Block.add_fvs fvs fvs' in
          (current_structs, p_s, aux_structs, aux_blocks, fvs'')
       | C.SpecType (C.Tunion (str, o_l_fields, _)) ->
          Block.unions := str::!Block.unions;
          let sofar_structures' = add_to_structures sofar_structures (str, [], None) in
          let (current_structs, p_s, aux_structs, aux_blocks, fvs') : Structure.t * (string * string * Exp.t list) list * Structure.t list * Block.t list * Exp.t list =
            Structure.get_struct pointers sofar_structures' ptr_struct str o_l_fields fvs  loc in
          let fvs'' = Block.add_fvs fvs fvs' in
          (current_structs, p_s, aux_structs, aux_blocks, fvs'')
       | C.SpecType (C.Tenum (str, o_l_fields, _)) ->
          begin
            Block.enums := str::!Block.enums;
            match o_l_fields with
              None -> ("nostruct", [], None), [], [], [], fvs
            | Some l_fields ->
               let aux_blocks, fvs'' = Structure.build_enum specs l_fields in
               let fvs' = Block.add_fvs fvs fvs'' in
               (str, [], None), [], [], List.rev aux_blocks, fvs'
          end
       | _ -> aux (* sofar_structures pointers ptr_struct loc *) ys
    in
    aux specs
    

  (* let total = ref 0;; *)
    
  (** ptr_struct: (structure type, belongs to the structure, field name) *)
  let rec translate acc pointers (sofar_structures : Structure.t V.t) (ptr_struct : (string * string * Exp.t list) list) procs struct_map fvs fpdata = function
    | [] -> acc
    | x::_xs ->
       dbg "DT1" "Translating...: " Cprint.print_def x;
       match x with
       | C.TYPEDEF ((C.SpecTypedef::(C.SpecType (C.Tunion (a,b,c)))::_,z), locs) when not (b=None) ->
          (** typedef union X {...} x; *)
          
          let a' = if a = "" then "union_" ^ (new_prog_var ()) else a in
          Block.unions := a'::!Block.unions;
          let b' = Structure.get_union_field b in

          (** union X {...}; *)
          let d1 = C.ONLYTYPEDEF ((C.SpecType (C.Tunion (a',b',c)))::[], locs) in
          (** typedef union X x; *)
          let d2 = C.TYPEDEF ((C.SpecTypedef::(C.SpecType (C.Tunion (a',None,c)))::[], z), locs) in

          pn_s "STRUCTURE" ("@TYPEDEF UNION: typedef union " ^ a ^ " ...");
          
          (** transform to struct definition and then typedef : CAUTION for unnamed struct *)
          if Structure.exists sofar_structures a' then
            begin
              pn_s "STRUCTURE" "@UNION EXISTS";
              translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata (d2::_xs)
            end
          else
            begin
              pn_s "STRUCTURE" "@UNION DOESN'T EXISTS";
              translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata (d1::d2::_xs)
            end
          
       | C.TYPEDEF ((C.SpecTypedef::(C.SpecType (C.Tenum (a,b,c)))::_,z), locs) when not (b=None) ->
          (** typedef enum X {...} x; *)

          let a' = if a = "" then "enum_" ^ (new_prog_var ()) else a in
          let d1 = C.DECDEF (((C.SpecType (C.Tenum (a',b,c)))::[], []), locs) in

          pn_s "STRUCTURE" ("@TYPEDEF ENUM " ^ a ^ " ...");
          
          translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata (d1::_xs)
          
       | C.TYPEDEF ((C.SpecTypedef::(C.SpecType (C.Tstruct (a,b,c)))::_,z), locs) when not (b=None) ->
          (** typedef struct X {...} x; *)

          let a' = if a = "" then "struct_" ^ (new_prog_var ()) else a in 
          (** struct X {...}; *)
          let d1 = C.ONLYTYPEDEF ((C.SpecType (C.Tstruct (a',b,c)))::[], locs) in
          (** typedef struct X x; *)
          let d2 = C.TYPEDEF ((C.SpecTypedef::(C.SpecType (C.Tstruct (a',None,c)))::[], z), locs) in

          pn_s "STRUCTURE" ("@TYPEDEF STRUCTURE: typedef structure " ^ a ^ " ...");

          (** transform to struct definition and then typedef : CAUTION for unnamed struct *)
          if Structure.exists sofar_structures a' then
            begin
              pn_s "STRUCTURE" "@STRUCT EXISTS";
              translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata (d2::_xs)
            end
          else
            begin
              pn_s "STRUCTURE" "@STRUCT DOESN'T EXISTS";
              translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata (d1::d2::_xs)
            end
          
       | C.TYPEDEF ((specifier, name_list), cabsloc) when List.exists (fun (_, dt, _, _) -> Block.is_funcptr dt || Block.is_proto dt) name_list ->
          (** typedef for function and function pointers only *)
          
          pn_s "STRUCTURE" ("TYPEDEF(PROTO) ");
          List.iter (fun (name, dt, _, _) ->
              let ftype' = Block.build_attributes true name pointers fvs  cabsloc dt specifier in
              
              let ftype = ((<>) Exp.GLOBAL) |>- ftype' in
              (** the attribute is [FUNCPTR (ret|param)] itself *)
              Block.func_ptr_types := (name, ftype)::!Block.func_ptr_types
            ) name_list;
          
          translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata _xs

       | C.TYPEDEF (name_group, cabsloc) ->
          (** typedef type new_type *)

          pn_s "STRUCTURE" "@OTHER TYPEDEF";
          dbg "STRUCTURE" "name list:" (iterS Block.print_spec_elem ",") (fst name_group);
          dbg "STRUCTURE" "Data Type:" (iterS (fun (_, dt, _, _) -> Block.print_decl_type dt) "---") (snd name_group);

          let (spec, names) = try Block.get_name_group name_group with _ -> raise (StError ("TYPEDEF @ " ^ (Locs.to_str @@ Locs.to_loc cabsloc))) in
          let all_news = (fun (n,_,_,_) -> n) |>>| names in
          List.iter (fun n -> Block.s_aliases := V.add n spec !Block.s_aliases) all_news;

          let fptrs = (fun acc (n,_,_,is_fptr) -> if is_fptr then acc@[n] else acc) |->> ([], names) in
          Block.func_ptrs.contents <- !Block.func_ptrs @ fptrs;

          dbg "STRUCTURE" "Type:" pw spec;
          dbg "STRUCTURE" "Names:" (iterS (fun (n,_,_,_) -> pw n) ",") names;

          let new_struct_map = (fun (n, _, _, _) ->
              (n, spec)) |>>| names in (** (new structure, old structure) *)

          let proto_types = List.filter (fun (_, _, _, b) -> b) names in

            let _ =
              if List.length proto_types > 0 then
                let type_names = (fun (nm, _, _, _) -> nm) |>>| proto_types in
                let specs = fst name_group in
                let dps' = (fun (nm,dt,_,_) -> (nm,dt)) |>>| (snd name_group) in
                let dps = (fun (nm,_) -> nm |<- type_names) |>- dps' in
                List.iter (fun (name, dt) ->
                    let ftype = Block.build_attributes true name pointers fvs  cabsloc dt specs in
                    Block.func_ptr_types := !Block.func_ptr_types @ [(name, ftype)]) dps
              else
                ()
            in
            
            let vv = ((fun acc (na, dt, _, _) ->
                       let lens = get_array_dimension dt in
                       if List.length lens = 0 then
                         acc
                       else
                         V.add na (spec, lens) acc
                     ) |->> (!Block.array_aliases, snd name_group)) in
            Block.array_aliases := vv;
            
            if Structure.exists sofar_structures spec then
              begin
                pn_s "STRUCTURE" (spec ^ " is a struct and it is found");

                let p = (fun (n,_,_,_) -> (n, true)) |>>| ((fun (_,b,c,_) -> b || c) |>- names) in
                let s = Structure.find sofar_structures spec in

                let _names = (fun (n,_,_,_) -> n) |>>| names in
                let new_logical_structs = (Structure.clone s) |>>| _names in
                
                let l_ss = (fun (_,b,_) -> b = spec) |>- ptr_struct in
                let n_l_ss = List.concat ((fun n -> (fun (a,b,c) -> (a,n,c)) |>>| l_ss) |>>| _names) in

                let new_physical_structs = (fun g -> STRUCT (g, Locs.to_loc cabsloc)) |>>| new_logical_structs in
                let all_structs = add_to_structures |->> (sofar_structures, new_logical_structs) in

                if spec |<- !Block.unions then
                  Block.unions := !Block.unions @ _names;
                
                  translate (new_physical_structs @ acc) (pointers @ p) all_structs (ptr_struct @ n_l_ss) procs (new_struct_map @ struct_map) fvs fpdata _xs
              end
            else if (spec, true) |<- pointers then
              let p = (fun (n,_,_,_) -> (n, true)) |>>| names in
              translate acc (pointers @ p) sofar_structures ptr_struct procs (new_struct_map @ struct_map) fvs fpdata _xs
            else
              begin
                let (specs, _) = name_group in
                let rec extract_enum = function
                    [] -> None, []
                  | x::xs ->
                     begin
                       match x with
                         C.SpecType (C.Tenum (s, eo, _)) ->
                          begin
                            match eo with
                              None -> None, []
                            | Some el ->
                               Block.enums := s::!Block.enums;
                               let el', fvs =
                                 Structure.build_enum specs (List.rev el) in
                               let el'' = (fun e' -> STATEMENT e') |>>| el' in
                               Some el'', fvs
                          end
                       | _ -> extract_enum xs
                     end
                in
                let enum, fvs'' = extract_enum specs in
                let fvs' = (fun fvs fv ->
                    let (v, attr) = Exp.var fv in
                    V.add v attr fvs) |->> (fvs, fvs'') in
                match enum with
                | Some el ->
                   begin
                     translate (el @ acc) pointers sofar_structures ptr_struct procs (new_struct_map @ struct_map) fvs' fpdata _xs
                   end
                | None ->
                   let all_struct_map = (new_struct_map @ struct_map) in
                   
                   let pp = (fun (n,_,_,_) -> (n, true)) |>>| ((fun (n,b,c,_) ->
                                                                b || c) |>- names) in
                   translate acc (pointers @ pp) sofar_structures ptr_struct procs all_struct_map fvs' fpdata _xs
              end
         
       | C.FUNDEF ((spec, name), block, _, cabsloc) ->
            pn_s "STRUCTURE" "@ FUNDEF";
             
            let (fname, dt, _, _) = name in
            let v = func_to_var fname spec dt pointers fvs  cabsloc in

            dbg "STRUCTURE" "Func Name:" Exp.pprint v;
            let fvs' = Block.add_fv fvs v in
                                                                                                    
            (* let fvs'' =
              if Exp.is_static v then
                begin
                  let v' = Block.__E (Block.to_static fname, Exp.get_attributes v) in
                  Block.add_fv fvs' v'
                end
              else fvs'
            in *)
            
            Block.update_structures sofar_structures;
            Block.ptr_structs.contents <- ptr_struct;            
            
            let (((fld, _, _) as b), sts) =
              try
                let block' = Procedure.fptransform pointers sofar_structures fvs' ((spec,name), block) cabsloc fpdata in
                Procedure.translate pointers sofar_structures fvs' ((spec,name), block') cabsloc fpdata
              with
                StError s ->
                 pn_s "EXCEP" "StERROR EXCEPTION *****************************";
                 pn_s "EXCEP" s;
                 pf_s "EXCEP" Cprint.print_def x;
                 pn_s "EXCEP" "===============";
                 raise (StError s)
              | e ->
                 pn_s "EXCEP" "OTHER EXCEPTION ***********************";
                 pf_s "EXCEP" Cprint.print_def x;
                 pn_s "EXCEP" "===============";
                 raise e
            in
            let sts' = (fun d -> STATEMENT d) |>>| sts in
            let pr = PROC (b, Formula.empty, Formula.trueempty, [], Locs.to_loc cabsloc) in

            if !funcdir <> "" then
              begin
                let file = make_path !funcdir cabsloc.C.filename fname in
                let out = open_out file in
                Marshal.to_channel out pr [];
                close_out out;
                let fn = Block.__N fld in
                let ffile = FFILE (file, fn) in
                translate (ffile::sts'@acc) pointers sofar_structures ptr_struct procs struct_map fvs' fpdata _xs
              end
            else
              translate (pr::acc) pointers sofar_structures ptr_struct procs struct_map fvs' fpdata _xs
       
       | C.DECDEF ((spec, ((((name, dt, _, _), _)::_) as init_name_group)), loc )
            when Block.is_proto dt -> (** FUNCTION DECLARATION *)
          pn_s "STRUCTURE" "@6-A";

          Block.update_structures sofar_structures;
          Block.ptr_structs := ptr_struct;

          let v' = func_to_var name spec dt pointers fvs  loc  in
          
          let v = Exp.vars_add [Exp.GLOBAL] v' in
          let fvs'' = Block.add_fv fvs v in
            
          let (_,decl,func_names', _,_) = Block.get_declaration ~is_global:true (spec, init_name_group) [] loc fvs in
          (* let func_names = Exp.var_add (Exp.FUNC ([],[])) (List.hd func_names') in
          let func_names' = Block.__E (Block.__N v, Block.__A func_names) in *)
          (* let func_names'' = (fun v -> Exp.vars_add [Exp.GLOBAL] v) |>>| func_names' in *)
          (* let fvs'' = Block.add_fvs fvs func_names'' in *)
          let decl' = List.rev ((fun d -> STATEMENT d) |>>| decl) in
          let func_name' = match decl with Block.DECL (v,_,_,_,_) ::_-> v | _ -> raise Error in
          (* if name = "string_list_append" then(
            Block.print_fvs fvs'';
            raise Error); *)
          
          translate (decl' @ acc) pointers sofar_structures ptr_struct (func_name'::procs) struct_map fvs'' fpdata _xs

       | C.DECDEF ((spec, ((((name, dt, _, _), _)::_) as init_name_group)), loc )
            when Block.is_funcptr dt -> (** FUNCTION POINTER *)

          pn_s "STRUCTURE" "@6-B";
          Block.update_structures sofar_structures;
          Block.ptr_structs.contents <- ptr_struct;
          
          let v = func_to_var name spec dt pointers fvs  loc  in
          let fvs' = Block.add_fv fvs v in
          
          let (_,decl,func_names, _,_) = Block.get_declaration ~is_global:true (spec, init_name_group) [] loc fvs in
          let decl' = List.rev ((fun d -> STATEMENT d) |>>| decl) in
          let func_name' = match decl with Block.DECL (v,_,_,_,_) ::_-> v | _ -> raise Error in
          
          (* let fvs' = Block.add_fvs fvs func_names in *)
          translate (decl' @ acc) pointers sofar_structures ptr_struct (func_name'::procs) struct_map fvs' fpdata _xs
            
       | (C.DECDEF ((l_spec_elem, l_init_name), cabsloc)) as dec ->

          (** struct st_name var; *)
          (*        let y = Block.get_var_name_from_init_name l_init_name in *)
          (* let ns = Block.get_var_name_from_init_name l_init_name in *)
          let loc = Locs.to_loc cabsloc in
          pn_s "STRUCTURE" "@9";
          pf_s "@9" Cprint.print_def dec;
          pn_s "@9" "@9-0";
          (* try *)
          
          let ((g_name, g_fields, s_type) as g : Structure.t), h, aux_structs, aux_blocks, fvs = unfold_struct sofar_structures pointers ptr_struct cabsloc fvs l_spec_elem in
          
          (* if g_name = "nostruct" then
          translate pointers sofar_structures ptr_struct procs struct_map fvs xs
        else *)
          let l_spec_elem = (function C.SpecType (C.Tstruct (string1, o_l_field_group, l_attribute)) when string1 = ""->
                                       C.SpecType (C.Tstruct (g_name, o_l_field_group, l_attribute))
                                    | C.SpecType (C.Tunion (string1, o_l_field_group, l_attribute)) when string1 = "" ->
                                       let o_l_field_group' = Structure.get_union_field o_l_field_group in
                                       C.SpecType (C.Tstruct (g_name, o_l_field_group', l_attribute))
                                    | x -> x ) |>>| l_spec_elem in
          
          (** Perhaps, back-propagation *)
          (** list of alias that maps to g_name  *)
          let n_fields = List.length g_fields in
          dbg "STRUCTURE" "|fields|:" pi n_fields;
          (* if List.length g_fields > 0 then *)
          (* if n_fields > 0 then raise (StError ("@9 Found a non-0 fields struct in DECDEF at " ^ (Locs.to_str loc))); *) 
          begin
            let rec all_olds current =
              
              let old_refs = fst |>>| (List.filter (fun (al, old_s) -> al<>old_s && old_s = current) struct_map) in
              
              (fun acc old -> let all_others = all_olds old in acc@all_others) |->> (old_refs, old_refs)
            in
            
            
            let old_refs : string list = all_olds g_name in (* fst |>>| (List.filter (fun (_, old_s) -> old_s = g_name) struct_map) in *)
            
            (** if a struct is among those alias, substitute to the field with the new fields *)

            let aux_structs', structures'' =
              if n_fields > 0 then
                begin
                  if g_name |<- !Block.unions then
                    Block.unions := !Block.unions @ old_refs;
                  let old_structs = (fun old_ref -> (old_ref, g_fields, s_type)) |>>| old_refs in

                  let structures' = add_to_structures |->> (sofar_structures, old_structs) in (*  (fun (a, b, st) -> if a |<- old_refs then (a, g_fields, st) else (a, b, st)) |>>| sofar_structures in *)
                  let aux_structs''  = (g::aux_structs) in
                  let structures'' = add_to_structures |->> (structures', aux_structs'') in
                  
                  let aux_structs' = ((fun g -> STRUCT (g, loc)) |>>| (aux_structs'')) in
                  aux_structs', structures''
                end
              else
                let aux_structs' = [] in
                let structures'' = add_to_structures |->> (sofar_structures, aux_structs) in
                aux_structs', structures''
            in
            pn_s "@9" "@9-1";
            
            Block.update_structures structures'';
            Block.ptr_structs.contents <- ptr_struct;
            pn_s "@9" "@9-2";
   
              let (new_pointers, blocks, fvs', assns, newdecl), (is_success, err) =
                try
                  let res = Block.get_declaration ~is_global:true (l_spec_elem, l_init_name) pointers cabsloc fvs in
                  res, (true, "")
                with
                  Exp.VarNotFound str ->
                  ([],[],V.empty,[],Block.SKIP), (false, str)
              in
              (* if not is_success then
                begin
                  match _xs with
                    [] ->
                     raise (StError err)
                  | x::xs ->
                     let xs' = x::dec::xs in
                     pn "";
                     pl (List.length xs'); pn " :::: xs'";
                     translate acc pointers sofar_structures ptr_struct procs struct_map fvs xs'
                                                   (* with _ -> raise (StError "Last DecDef") *)
                end
              else *)
                begin
                  pn_s "@9" "@9-3";
                  dbg "TR" "Statements:\n" (iterS (Block.pprint 0) "") blocks;
            
                  let ptr_struct = Block.ptr_structs.contents in
                  let statements = (fun x -> STATEMENT x) |>>| (blocks @ aux_blocks) in
                  let assns' = (fun x -> [x]) |>>| assns in
                  pf_s "@9" pi (List.length assns');
                  pn_s "@9" "@9-4";
            
                  let assignments, _ = List.split ((fun ass ->
                                           (* iterS Cprint.print_statement "\n" ass; pn "..."; *)
                                           dbg "@9" "" (iterS Cprint.print_statement "\n") ass;
                                           Block.translate (pointers @ new_pointers) fvs'  ass
                                         ) |>>| (assns')) in
                  (* let assignments = Block.to_assignments' assns cabsloc in *)
                  dbg "TR" "Assignments:\n" (iterS (Block.pprint 0) "") assignments;
                  pn_s "@9" "@9-5";
                  let all_pointers = pointers @ new_pointers in
                  let all_fvs = fvs' in
                  let ass = (fun x -> STATEMENT x) |>>| assignments in
                  
                  let all_structs = List.rev (ass @ statements @ aux_structs') in
                  pn_s "@9" "@9-6";
                  (*  :: translate pointers structures'' (ptr_struct @ h) procs struct_map fvs xs in *)
                  translate (all_structs @ [(STATEMENT newdecl)] @ acc) all_pointers structures'' ptr_struct procs struct_map all_fvs fpdata _xs
                end
          end
       | C.ONLYTYPEDEF (specifier, cabsloc) as dec ->
          Cprint.print_def dec;
          let loc = Locs.to_loc cabsloc in
          pn_s "STRUCTURE" "@17";
          dbg "STRUCTURE" "dec:" Cprint.print_def dec;
          
          let ((g_name, g_fields, s_type) as g : Structure.t), h, aux_structs, aux_blocks, fvs = unfold_struct sofar_structures pointers ptr_struct cabsloc fvs specifier in
          
          pf_s "STRUCTURE" pi (List.length g_fields);
          
          if g_name = "nostruct" then
            translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata _xs
          else
            begin
              pn_s "STRUCTURE" ("@15 struct " ^ g_name ^ "{ ... }");
              let n_fields = List.length g_fields in
              
              let aux_structs' = (fun g -> STRUCT (g, loc)) |>>| (aux_structs) in
              let aux_blocks'  = (fun g -> STATEMENT g) |>>| aux_blocks in

              let rec all_olds current =
                
                let old_refs = fst |>>| (List.filter (fun (al, old_s) -> al<>old_s && old_s = current) struct_map) in
                
                (fun acc old -> let all_others = all_olds old in acc@all_others) |->> (old_refs, old_refs)
              in
              
              if n_fields > 0 then
                begin
                  let old_refs = all_olds g_name in
                  if g_name |<- !Block.unions then
                    Block.unions := !Block.unions @ old_refs;
                  dbg "STRUCTURE" "OLD Refs:" (iterS pw ",") old_refs;
                  
                  let structures_ref = (fun a -> (a, g_fields, s_type)) |>>| old_refs in
                  
                  let structures_new = g::structures_ref in
                  let all_structures' = add_to_structures |->> (sofar_structures, structures_new) in
                  let all_structures = add_to_structures |->> (all_structures', aux_structs) in
                  
                  let new_structs  = (fun g -> STRUCT (g, loc)) |>>| (structures_new) in
                  Block.update_structures all_structures;
                  let all = List.rev (aux_structs' @ aux_blocks' @ new_structs) in
                  translate (all @ acc) pointers all_structures (ptr_struct @ h) procs struct_map fvs fpdata _xs
                  
                end
              else
                begin
                  let all_structures = add_to_structures sofar_structures g in
                  let new_structs  = [STRUCT (g, loc)] in
                  
                  translate (aux_structs' @ aux_blocks' @ new_structs @ acc) pointers all_structures (ptr_struct @ h) procs struct_map fvs fpdata _xs
                  
                end
            end
       | _ ->
          pn_s "STRUCTURE" ("Something Else");
          translate acc pointers sofar_structures ptr_struct procs struct_map fvs fpdata _xs


  let is_fundef = function
      | C.TYPEDEF (name_group, cabsloc) -> false
      | C.FUNDEF (name, block, _, cabsloc) -> true
      | C.S_FUNDEF (pre, post, defs, cabsloc) -> true
      | C.DECDEF ((spec, ((name, C.PROTO (dt, l_single_name, b), xx, l), _)::_), loc ) -> false
      | C.DECDEF ((spec, ((name, (C.PTR (y, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((spec, ((name, C.PTR (y1, C.PTR (y2, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((l_spec_elem, l_init_name), cabsloc) -> false
      | C.ONLYTYPEDEF (specifier, cabsloc) -> false
      | _ -> false
    


  let rearrange (xs : Cabs.definition list) =

    let typedef_partition xs =
      let rec aux fvs (tr, fls) = function
          [] -> (tr, fls)
        | x::xs' ->
           match x with
           | C.TYPEDEF (name_group, cabsloc) ->
              let fv = VCabs.decl_fv (fst name_group) in
              aux (fvs@fv) (x::tr, fls) xs'
           | C.FUNDEF (name, block, _, cabsloc) -> aux fvs (tr, x::fls) xs'
           | C.S_FUNDEF (pre, post, defs, cabsloc) -> aux fvs (tr, x::fls) xs'
           | C.DECDEF ((spec, ((name, C.PROTO (dt, l_single_name, b), xx, l), _)::_), loc ) ->
                aux fvs (tr, x::fls) xs'
           | C.DECDEF ((spec, ((name, (C.PTR (y, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> aux fvs (tr, x::fls) xs'
           | C.DECDEF ((spec, ((name, C.PTR (y1, C.PTR (y2, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> aux fvs (tr, x::fls) xs'
           | C.DECDEF ((l_spec_elem, l_init_name), cabsloc) ->
              let names = (fun ((nm, _, _, _), _) -> nm) |>>| l_init_name in
              if List.exists (fun name -> name |<- fvs) names then
                aux fvs (x::tr, fls) xs'
              else
                aux fvs (tr, x::fls) xs'
           (** Before, it was true. It is made false for the example where a struct is initialized with a function name which is static and therefore renamed. Moreover, it seems to be natural to make it a declaration. *)
           | C.ONLYTYPEDEF (specifier, cabsloc) ->
              let fv = VCabs.decl_fv specifier in
              aux (fvs@fv) (x::tr, fls) xs'
           | _ -> aux fvs (tr, x::fls) xs'
      in
      aux [] ([],[]) (List.rev xs)
    in

    let is_decls = function
      | C.TYPEDEF (name_group, cabsloc) -> false
      | C.FUNDEF (name, block, _, cabsloc) -> false
      | C.S_FUNDEF (pre, post, defs, cabsloc) -> false
      | C.DECDEF ((spec, ((((_, dt, _, _), _)::_))), loc ) when Block.is_funcptr dt -> true
      | C.DECDEF ((spec, ((name, C.PROTO (dt, l_single_name, b), xx, l), _)::_), loc ) -> false
      | C.DECDEF ((spec, ((name, (C.PTR (y, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((spec, ((name, C.PTR (y1, C.PTR (y2, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((l_spec_elem, l_init_name), cabsloc) -> true (** Refer to the previous comments *)
      | C.ONLYTYPEDEF (specifier, cabsloc) -> false
      | _ -> false
    in

    let is_fundec = function
      | C.TYPEDEF (name_group, cabsloc) -> false
      | C.FUNDEF (name, block, _, cabsloc) -> false
      | C.S_FUNDEF (pre, post, defs, cabsloc) -> false
      | C.DECDEF ((spec, ((((_, dt, _, _), _)::_))), loc ) when Block.is_proto dt -> true
     (* | C.DECDEF ((spec, ((name, C.PROTO (dt, l_single_name, b), xx, l), _)::_), loc ) -> true
      | C.DECDEF ((spec, ((name, (C.PTR (y, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> true
      | C.DECDEF ((spec, ((name, C.PTR (y1, C.PTR (y2, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> true *)
      | C.DECDEF ((l_spec_elem, l_init_name), cabsloc) -> false
      | C.ONLYTYPEDEF (specifier, cabsloc) -> false
      | _ -> false
    in

   
    
    let is_static = function
      | C.TYPEDEF (name_group, cabsloc) -> false
      | C.FUNDEF ((l_spec_elem, name), block, _, cabsloc) ->
         Block.has_static l_spec_elem
      | C.S_FUNDEF (pre, post, defs, cabsloc) -> false
      | C.DECDEF ((spec, ((name, C.PROTO (dt, l_single_name, b), xx, l), _)::_), loc ) -> false
      | C.DECDEF ((spec, ((name, (C.PTR (y, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((spec, ((name, C.PTR (y1, C.PTR (y2, C.PROTO (dt1, l_single_name, b))), xx, l), _)::_), loc) -> false
      | C.DECDEF ((l_spec_elem, l_init_name), cabsloc) -> false
      | C.ONLYTYPEDEF (specifier, cabsloc) -> false
      | _ -> false
    in

    let rec unfold_struct_in_cabs loc = function
      | [] -> ([], [], [], false)
      | (y::ys) ->
         match y with (** x : spec_elem *)
         | C.SpecType (C.Tstruct (str, o_l_fields, a)) when o_l_fields <> None ->
            let str' = if str="" then new_prog_var () else str in
            
            let y' = (* C.SpecType (C.Tnamed str') *)
              C.SpecType (C.Tstruct (str', None, a)) in
            let st = C.SpecType (C.Tstruct (str', o_l_fields, a)) in
            ([str'], [st], y'::ys, o_l_fields <> Some [])
         | C.SpecType (C.Tunion (str, o_l_fields, a)) when o_l_fields <> None->
            let str' = if str="" then new_prog_var () else str in
            Block.unions := str::!Block.unions;
            let y' = C.SpecType (C.Tstruct (str', None, a)) in
            let o_l_fields' = Structure.get_union_field o_l_fields in
            let st = C.SpecType (C.Tunion (str', o_l_fields', a)) in
            ([str'], [st], y'::ys, o_l_fields <> Some [])
         | C.SpecType (C.Tenum (str, o_l_fields, a)) when o_l_fields <> None->
            let str' = if str="" then new_prog_var () else str in
            let y' = (* C.SpecType (C.Tnamed str') *)
              C.SpecType (C.Tnamed str') in
            let st = C.SpecType (C.Tenum (str', o_l_fields, a)) in
            ([str'], [st], y'::ys, o_l_fields <> Some [])
         | x ->
            let (a0, a, ys', bb) = unfold_struct_in_cabs loc ys in
            (a0, a, y::ys', bb)
    in
    
    
    let extract_struct_from_fundef = function
      | C.FUNDEF (name, block, bb, cabsloc) ->
         let rec extract_from_statements = function
           | C.DEFINITION d ->
              begin
                let (stn, st, d') =
                  match d with
                  | C.DECDEF ((l_spec_elem, l_init_name), loc) -> (** declarations *)
                     let (stn, st, lse, bb) = unfold_struct_in_cabs loc l_spec_elem in
                     let st' =
                       if bb then
                         [C.ONLYTYPEDEF (st, loc)]
                       else
                         []
                     in
                     (stn, st', [C.DECDEF ((lse, l_init_name), loc)])
                  | (C.TYPEDEF (names, loc) as d) ->
                     ([], [], [d])
                  | C.ONLYTYPEDEF (l_spec_elem, loc) ->
                     let (stn, st, lse, bb) = unfold_struct_in_cabs loc l_spec_elem in
                     
                     let st' = if bb then [C.ONLYTYPEDEF (st, loc)] else [] in
                     (stn, st', [])
                  | d ->
                     
                     ([], [], [d])
                in
                if List.length d' > 0 then
                  (stn, st, C.DEFINITION (List.hd d'), [])
                else
                  (stn, st, C.NOP (cabsloc), [])
              end
           | C.BLOCK (block, cabsloc) ->
              let res = extract_from_statements |>>| block.C.bstmts in
              let a,b,c,s =
                (fun (a,b,c,s) (a',b',c',s') ->
                    (a@a', b@b', c@s'@[c'],s)
                ) |->> (([],[],[],[]), res) in
              (a,b,C.BLOCK ({C.blabels=block.C.blabels; battrs=block.C.battrs; bstmts=c}, cabsloc),s)
           | C.SEQUENCE (statement1, statement2, cabsloc) ->
              let (a1,b1,c1,s1) = extract_from_statements statement1 in
              let (a2,b2,c2,s2) = extract_from_statements statement2 in
              (a1@a2, b1@b2, C.SEQUENCE (c1,c2,cabsloc), s1@s2)
           | C.IF (expression, statement1, statement2, cabsloc) ->
              
              let (a0,b0,c0,expression') = extract_from_expression cabsloc expression in
              let (a1,b1,c1,s1) = extract_from_statements statement1 in
              let (a2,b2,c2,s2) = extract_from_statements statement2 in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              let c2_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s2@[c2]}, cabsloc) in            
              (a0@a1@a2, b0@b1@b2, C.IF (expression',c1_block,c2_block,cabsloc), c0)
           | C.WHILE (formula, expression, statement, cabsloc) ->
              let (a0,b0,c0,expression') = extract_from_expression cabsloc expression in
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              
              (a0@a1,b0@b1,C.WHILE (formula, expression', c1_block, cabsloc), c0)
           | C.DOWHILE (formula, expression, statement, cabsloc) ->
              let (a0,b0,c0,expression') = extract_from_expression cabsloc expression in
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a0@a1,b0@b1,C.DOWHILE (formula, expression', c1_block, cabsloc), c0)
           | C.FOR (formula, for_clause, expression, expression2, statement, cabsloc) ->
              let (a0,b0,s0,expression') = extract_from_expression cabsloc expression in
              (* let (a02,b02,s02,expression2') = extract_from_expression cabsloc expression2 in *)
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a0@(*a02@*)a1,b0@(*b02@*)b1,C.FOR (formula, for_clause, expression', expression2, c1_block, cabsloc), s0(*@s02*))
           | C.SWITCH (expression, statement, cabsloc) ->
              let (a0,b0,s0,expression') = extract_from_expression cabsloc expression in
              let (a1,b1,c1,s1) = extract_from_statements statement in
              (a0@a1,b0@b1,C.SWITCH (expression', c1, cabsloc), s0@s1)
           | C.CASE (expression, statement, cabsloc) ->
              let (a0,b0,s0,expression') = extract_from_expression cabsloc expression in
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a0@a1,b0@b1,C.CASE (expression', c1_block, cabsloc),s0)
           | C.CASERANGE (expression, expression2, statement, cabsloc) ->
              let (a0,b0,s0,expression') = extract_from_expression cabsloc expression in
              let (a02,b02,s2,expression2') = extract_from_expression cabsloc expression2 in
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a0@a02@a1,b0@b02@b1,C.CASERANGE (expression', expression2', c1_block, cabsloc),s0@s2)
           | C.DEFAULT (statement, cabsloc) ->
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a1,b1,C.DEFAULT (c1_block, cabsloc),[])
           | C.LABEL (string, statement, cabsloc) ->
              let (a1,b1,c1,s1) = extract_from_statements statement in
              let c1_block = C.BLOCK ({C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = s1@[c1]}, cabsloc) in
              (a1,b1,C.LABEL (string, c1_block, cabsloc),[])
           | C.COMPUTATION (expression, cabsloc) ->
              let (a0,b0,s0,expression') = extract_from_expression cabsloc expression in
              (a0,b0,C.COMPUTATION (expression', cabsloc),s0)
           | s -> ([], [], s, [])

         and extract_from_expression loc = function
           | C.UNARY (unary_operator, expression) ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c, C.UNARY (unary_operator, expression'))
           | C.BINARY (binary_operator, expression1, expression2) ->
              
              let (a1,b1,c1,expression1') = extract_from_expression loc expression1 in
              let (a2,b2,c2,expression2') = extract_from_expression loc expression2 in
              (a1@a2, b1@b2, c1@c2, C.BINARY (binary_operator, expression1', expression2'))
           | C.QUESTION (expression1, expression2, expression3) ->
              let (a1,b1,c1,expression1') = extract_from_expression loc expression1 in
              let (a2,b2,c2,expression2') = extract_from_expression loc expression2 in
              let (a3,b3,c3,expression3') = extract_from_expression loc expression3 in
              (a1@a2@a3, b1@b2@b3, c1@c2@c3, C.QUESTION (expression1', expression2', expression3'))
           | C.CAST ((l_spec_elem, decl_type), init_expression) ->
              begin
                let rec extract_from_init_expression = function
                    C.NO_INIT -> ([],[],[],C.NO_INIT, false)
                  | C.SINGLE_INIT (expression) ->
                     let (a,b,c,expression') = extract_from_expression loc expression in
                     (a,b,c,C.SINGLE_INIT (expression'), false)
                  | C.COMPOUND_INIT es ->
                     let (a,b,c, es') =
                       (fun (a_s, b_s, c_s, acc) (iw, ie) ->
                         let (a,b,c,ie',_) = extract_from_init_expression ie in
                         (a_s@a, b_s@b, c_s@c, acc@[(iw, ie')])
                       ) |->> (([],[],[],[]), es) in
                     (a,b,c, C.COMPOUND_INIT es', true)
                in
                let (a,b,c, init_expression', is_compound_init) = extract_from_init_expression init_expression in
                let (stn, st, specifier, is_a_new_struct) = unfold_struct_in_cabs loc l_spec_elem in

                (** is_a_new_struct: if it is a new struct or union with non-zero number of fields
                 *  declr: the declared variable. Here it can be empty
                 *  st: extracted structure
                 *  stn: structure name (in string)
                 *  is_compound_init: need to take special initialization
                 *  is_a_new_struct: if it is a newly declared struct
                 *)
                
                if is_compound_init then
                  if is_a_new_struct then
                    let variable = new_prog_var () in
                    let cname = (variable, decl_type, [], loc) in
                    let l_init_name = [(cname, init_expression')] in
                    let variable_declr = C.DEFINITION (C.DECDEF ((specifier, l_init_name), loc))  in
                    let new_struct = C.ONLYTYPEDEF (st, loc) in
                    let expression = C.VARIABLE variable in
                    (a@stn, b@[new_struct], c@[variable_declr], expression)
                  else
                    (a, b, c, C.CAST ((l_spec_elem, decl_type), init_expression'))
                else
                  if is_a_new_struct then
                    (a, b, c, C.CAST ((l_spec_elem, decl_type), init_expression'))
                  else
                    (a, b, c, C.CAST ((l_spec_elem, decl_type), init_expression'))
              end
           | C.CALL (expression, expression_list) ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              let (a1,b1,c1,expression_list') = (fun (a_s, b_s, c_s, el) expression ->
                  let (a,b,c,expression') = extract_from_expression loc expression in
                  (a_s@a, b_s@b, c_s@c, el@[expression'])
                ) |->> (([],[],[],[]), expression_list) in
              (a@a1, b@b1, c@c1, C.CALL (expression', expression_list'))
           | C.COMMA (expression_list) ->
              let rec aux (a_s, b_s, c_s, el) = function
                  [] -> (a_s, b_s, c_s, el)
                | expression::others ->
                   let (a,b,c,expression') = extract_from_expression loc expression in
                   let current, expression'' =
                     if List.length c = 0 && List.length others > 0 then
                       [C.COMPUTATION (expression', loc)], []
                     else
                       c, [expression']
                   in
                   aux (a_s@a, b_s@b, c_s@current, el@expression'') others
              in
              let (a1,b1,c1,expression_list') = aux ([],[],[],[]) expression_list in  
              (a1, b1, c1, C.COMMA expression_list')
           | C.PAREN (expression) ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c, C.PAREN ( expression'))
           | C.EXPR_SIZEOF expression ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c,C.EXPR_SIZEOF ( expression'))
           | C.EXPR_ALIGNOF expression ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c, C.EXPR_ALIGNOF ( expression'))
           | C.INDEX (expression1, expression2) ->
              let (a1,b1,c1,expression1') = extract_from_expression loc expression1 in
              let (a2,b2,c2,expression2') = extract_from_expression loc expression2 in
              (a1@a2, b1@b2, c1@c2, C.INDEX (expression1', expression2'))
           | C.MEMBEROF (expression, string) ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c, C.MEMBEROF ( expression', string))
           | C.MEMBEROFPTR (expression, string) ->
              let (a,b,c,expression') = extract_from_expression loc expression in
              (a,b,c, C.MEMBEROFPTR ( expression', string))
           | C.GNU_BODY block ->
              begin
                match List.rev block.C.bstmts with
                  (expr::stmts) ->
                   begin
                     let (a1',b1',bstmts'') =
                       (fun (a_s, b_s, (el:Cabs.statement list)) statement ->
                         let (a,b,statement',additionals) = extract_from_statements statement in
                         let all_statements = el @ (additionals@[statement']) in
                         (a_s@a, b_s@b, all_statements)
                       ) |->> (([],[],[]), List.rev stmts) in
                     let (a,b,expression',additionals) = extract_from_statements expr in
                     let bstmts' = bstmts'' @ additionals in
                     let a1 = a1'@a in
                     let b1 = b1'@b in
                     
                     match expression' with
                     | (C.COMPUTATION (exp, cabsloc)) ->
                        let nv = C.VARIABLE (new_prog_var ()) in
                        let st = C.COMPUTATION (C.BINARY (C.ASSIGN, nv, exp), cabsloc) in
                        (a1,b1,bstmts'@[st],nv)
                     | C.DEFINITION definition ->
                        begin
                          match definition with
                            C.DECDEF ((_, l_init_name), _) ->
                             if List.length l_init_name > 1 then
                               begin
                                 iterS Cprint.print_statement "\n" bstmts';
                                 raise (StError "More than one definition computation statement at the last")
                               end
                             else
                               let ((name, _, _, _), _) = List.hd l_init_name in
                               begin
                                 let nv = C.VARIABLE (new_prog_var ()) in
                                 let st = C.COMPUTATION (C.BINARY (C.ASSIGN, nv, C.VARIABLE name), cabsloc) in
                                 (a1,b1,bstmts'@[st],nv)
                               end
                          | _ ->
                             iterS Cprint.print_statement "\n" bstmts';
                             raise (StError "Non computation definition at the last")
                        end
                     | C.ASM (_,_,_,cabsloc) 
                       ->
                        let nv = C.VARIABLE ("__dummy-for-asm-statement__") in
                        (a1,b1,bstmts',nv)
                        
                     | C.IF (cond, p1, p2, loc)  ->
                        let nv = C.VARIABLE (new_prog_var ()) in
                        (a1,b1,bstmts',nv)
                     | _ ->
                        iterS Cprint.print_statement "\n" bstmts';
                        raise (StError "Non computation statement at the last")
                        
                   end
                | _ ->
                   ([],[],[], C.NOTHING)
              end
           | e ->
              ([],[],[],e)
         in
         let (stn, (st : C.definition list),b) =
           (fun (stn, a,b) statement ->
             let (stn', a',b',additionals) = extract_from_statements statement in
             (stn@stn', a@a', b@additionals@[b'])) |->> (([], [], []), block.C.bstmts) in
         let block' = {C.blabels = block.C.blabels; battrs = block.C.battrs; bstmts = b} in
         let fd = C.FUNDEF (name, block', bb, cabsloc) in
         (stn, st, fd)
      | d -> ([], [], d)
    in
    
    let rec get_nm = function
        [] -> None
      | y::ys ->
         match y with
         | C.SpecType (C.Tstruct (str, o_l_fields, _))
           | C.SpecType (C.Tunion (str, o_l_fields, _)) ->
            Some str
         | C.SpecType (C.Tnamed nm) ->
            Some nm
         | C.SpecType _ ->
            get_nm ys
         | _ -> get_nm ys
    in


    let rec is_not_proto = function
        C.JUSTBASE -> true
      | C.PARENTYPE (_, dc, _) -> is_not_proto dc
      | C.ARRAY (dc, _, _) -> false
      | C.PTR (_, dc) -> is_not_proto dc
      | C.PROTO (_, _, _) -> false
    in
    
    let get_struct_alias_name (struct_names, aliases) = function
      | C.TYPEDEF ((specifier,l_names), _) ->
         let names = (fun acc (a,dc,_,_) ->
             if is_not_proto dc then
               a::acc
             else
               acc
           ) |->> ([], l_names) in
         
         let is_with_no_src = List.length specifier = 1 && List.hd specifier = C.SpecTypedef in
         let is_with_src = not is_with_no_src in

         let is_src_struct =
           List.exists
             (function C.SpecType (sp) ->
                        begin
                          match sp with
                            C.Tstruct (nm,_,_) -> nm <> ""
                          | C.Tunion _ -> true
                          | _ -> false
                        end
                     | _ -> false
             ) specifier
         in

         let is_src_proper_alias =
           List.exists
             (function C.SpecType sp ->
                        begin
                          match sp with
                            C.Tstruct (nm,_,_) -> nm <> ""
                          | C.Tunion (nm,_,_) -> nm <> ""
                          | C.Tnamed nm -> nm <> ""
                          | _ -> false
                        end
                     | _ -> false
             ) specifier
         in

         (*
         dbg "ALIAS" "specs:" (iterS print_spec_elem ", ") specifier;
         dbg "ALIAS" "is with no src:" pb is_with_no_src;
         dbg "ALIAS" "is src a strcut/union:" pb is_src_struct;
         dbg "ALIAS" "is src a proper alias:" pb is_src_proper_alias;
         dbg "ALIAS" "defined types are:" (iterS pw ",") names;
         dbg "ALIAS" "" p "";
          *)
         
         if is_with_src then
           let nm' = get_nm specifier in
           match nm' with
             None ->
              (struct_names, aliases)
           | Some nm ->
              let struct_names' =
                if is_src_struct then
                  if nm |<- struct_names then
                    struct_names
                  else
                    nm::struct_names
                else
                  struct_names
              in
              let aliases' =
                if is_src_proper_alias then
                  let new_aliases = (fun n -> (nm, n)) |>>| names in
                  aliases @ new_aliases
                else
                  aliases
              in
              (struct_names', aliases')
         else
           (struct_names, aliases)
      | C.DECDEF ((specifier, l_init_name), _) ->
         let is_it_a_struct =
           List.exists
             (function C.SpecType C.Tstruct _ -> true
                     | C.SpecType C.Tunion _ -> true
                     | _ -> false
             ) specifier
         in
         if is_it_a_struct then
           let nm' = get_nm specifier in
           match nm' with
             None ->
              (struct_names, aliases)
           | Some nm ->
              (nm::struct_names, aliases)
         else
           (struct_names, aliases)
      | _ ->
         (struct_names, aliases)
    in

    (*
    let get_struct_name acc = function
      | C.TYPEDEF ((specifier,l_init_name), _) ->
         pl (List.length l_init_name);
         
         acc
      | C.ONLYTYPEDEF (specifier, _) ->
         begin
           match get_nm specifier with
             None -> acc
           | Some nm ->
              nm::acc
         end
      | _ -> acc
    in
    let get_alias_names acc = function
      | C.TYPEDEF ((specifier, names), cabsloc) ->
         begin
           match get_nm specifier with
             None -> acc
           | Some _ ->
              acc @ ((fun (a,_,_,_) -> a) |>>| names)
         end
      | _ -> acc
    in
     *)
    
    let (typedefs, rest) = typedef_partition xs in
    let (struct_names, aliases) = get_struct_alias_name |->> (([], []), typedefs) in

    dbg "ALIAS" "Basic Struct/Unions:\n" (iterS pw ",") struct_names;
    dbg "ALIAS" "Aliases by Typedef:\n" (iterS (fun (a,b) -> p a; p " --> "; p b) ", ") aliases;
    
    let rec is_a_struct aliases struct_names a =
      if a |<- struct_names then
        true
      else
        try
          let (b, _) = List.find (fun (_, a') -> a = a') aliases in
          is_a_struct aliases struct_names b
        with
          _ -> false
    in

    let all_struct_names = (fun struct_names (_, a) ->
        if is_a_struct aliases struct_names a then
          a::struct_names
        else
          struct_names
      ) |->> (struct_names, aliases) in

    dbg "ALIAS" "Finally all global struct names:\n" (iterS pw ", ") all_struct_names;
    
    (* let struct_names = get_struct_name |->> ([], typedefs) in
    
    let struct_names_with_aliases = get_alias_names |->> (struct_names, typedefs) in *)
    let (decls, rest') = List.partition is_decls rest in

    let (statics, rest'') = List.partition is_static rest' in
    let (_funs, _others) = List.partition is_fundef statics in
    let (fundecs, rest''') = List.partition is_fundec rest'' in
    let (fundefs, _) = List.partition is_fundef rest''' in

    let fundef_to_fundec = (function C.FUNDEF ((specifier, name), _, cabsloc, _) ->
                                    C.DECDEF ((specifier, [(name, C.NO_INIT)]), cabsloc)
                                 | d -> d ) in
    let static_fundecs = fundef_to_fundec |>>| _funs in
    let fundef_fundecs = fundef_to_fundec |>>| fundefs in
    
    let (stn, extracted_structs, fundefs') = (fun (stn, extructed_structs, fundefs) fundef  ->
        dbg "TRANS" "Before Extraction\n" Cprint.print_def fundef;
        let (stn', ex, fd) = extract_struct_from_fundef fundef in
        dbg "TRANS" "After Extraction\n" Cprint.print_def fd;
        (stn@stn', extructed_structs@ex, fd::fundefs)
      ) |->> (([], [], []), _funs @ fundefs) in

(*    let extracted_struct_names = (fun a s ->
        match s with
          STRUCT ((n), _) -> n::a
        | _ -> a
      ) |->> ([], extracted_structs) in *)
    let all_decls = typedefs @ extracted_structs @ fundecs @ static_fundecs @ fundef_fundecs @ decls @ _others @ (List.rev fundefs') in
    
    dbg "ALLDEFS" "All definitions in final sort:\n" (iterS Cprint.print_def "\n") all_decls;

    
    (all_decls, all_struct_names)

  (*
  let rec get_functions = function
    | [] -> []
    | C.FUNDEF ((spec, (name, dt, xx, l)), _, _, _)::xs ->
      let (_,_,func_names,_,_) = Block.get_declaration ~is_global:true (spec, [(name, dt, xx, l), C.NO_INIT]) [] l in
      (List.hd func_names) :: (get_functions xs)
    | C.S_FUNDEF (_, _, C.FUNDEF ((spec,(name, dt, xx, l)), _, _, _), _)::xs ->
      let (_,_,func_names,_,_) = Block.get_declaration ~is_global:true (spec, [(name, dt, xx, l), C.NO_INIT]) [] l in
      (List.hd func_names) :: (get_functions xs)
    | _::xs -> get_functions xs
   *)
    
  let fv g =
    let aux = function
      | NA -> []
      | STATEMENT a -> []
      | PROC ((fn,_,b), _, _, _, _) ->
         fn :: (Block.fv b)
      | STRUCT (st, _) -> []
      | FFILE (_) -> []
    in
    List.concat (aux |>>| g)

  let get_all_fundef defs =
    is_fundef |>- defs
    
end;;

module Program = struct
  type t = Global.t list

  module V = Map.Make(String)
           
  let rearrange xs =
    Global.rearrange xs
    
  let translate (xs, all_struct_names) fpdata =
    Block.nopointermode := true;
    Block.ormode := true;
    Block.noformulamode := false;

    (* Block.functions.contents <- (Global.get_functions xs); *)
    (* iterS Exp.pprint ", " (!Block.functions); *)
    dbg "dbg" "Total Global:" pl (List.length xs);
    
    let (ys : Global.t list) =
      Structure.all_struct_names := all_struct_names;
      Global.translate [] [] V.empty [] [] [] V.empty fpdata xs
    in

    
    
    (*     pn "";
       iterS Global.pprint "" ys;  *)
    (* pn ""; pl (List.length ys); pw " /"; pl (List.length xs); pn ""; *)
    List.rev ys
end;;
