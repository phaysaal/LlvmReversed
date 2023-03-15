open ConsortAst
open Format
open PrettyPrint

(*
let is_operator_id s =
  let lbux = Lexing.from_string s in
  let t = Lexer.read lbux in
  match t with
  | Parser.ID _ -> false
  | _ -> true *)


let pp_rinit = function
  | Ast.RNone -> ps "_"
  | RVar v -> pv v
  | RInt i -> pprint_int i

let pp_imm = function
  | IInt i -> pprint_int i
  | IVar v -> pv v

let pprint_kv (k,v) =
  pl [
    pf "%s: " k;
    pp_rinit v
  ]

let upp_imm = ul pp_imm
    
let pp_ap = function
  | Ast.AVar v -> pv v
  | ADeref v -> pl [ ps "*"; pv v ]
  | AProj (v,ind) -> pl [ pv v; ps "."; pi ind ]
  | APtrProj (v,ind) -> pl [ pf "(*%s)." v; pi ind ]


                      (*
let rec pp_ref_ast (r: (RefinementTypes.refine_ap list, RefinementTypes.refine_ap) RefinementTypes.refinement) =
  let open RefinementTypes in
  match r with
  | Top -> ps "T"
  | ConstEq n -> pf "~ = %d" n
  | Relation { rel_op1; rel_cond; rel_op2 } ->
    let print_rimm () = function
      | RConst i -> pi i
      | RAp (`Sym i) -> pf "$%d" i
      | RAp (#Paths.concr_ap as c) -> ps @@ Paths.to_z3_ident c
    in
    let r1_printer () r1 = match r1 with
      | Nu -> ps "~"
      | RImm i -> print_rimm () i
    in
    pf "%a@ %s@ %a"
      r1_printer rel_op1
      rel_cond
      print_rimm rel_op2
  | And (r1,r2) ->
    pf "%a@ /\\@ %a"
      (ul pp_ref_ast) r1
      (ul pp_ref_ast) r2
  | _ -> failwith @@ "Cannot annotate with relation " (* ^ (string_of_refinement r) *)
                       *)
                      
let rec pp_lhs (l:lhs) =
  match l with
  | `OVar x -> pv x
  | `OInt i -> pi i
  | `ODeref v -> pl [
                   ps "*";
                   pv v
                   ]
  | `Nondet -> ps "_"
  | `BinOp (l1, op, l2) ->
     pl [
         pf "(%a %s %a)" (ul pp_lhs) l1 op (ul pp_lhs) l2
       ]
  | `Null -> ps "null"
  | `OBool b -> if b then ps "True" else ps "False"
  | `Mkref il -> pl [
                    pf "mkref %a" (ul pp_lhs) il
                  ]
  | `MkArray v -> pl [ pf "mkarray %a" (ul pp_lhs) v ]
  | `Call c -> pprint_fn_call c
  | `Tuple rinit ->
    pl [
        ps "(";
        psep ", " @@ List.map pp_lhs rinit;
        ps ")"
      ]
  | `Read (b,i) ->
    pf "%a[%a]" (ul pp_lhs) b (ul pp_lhs) i
  | `LengthOf v ->
     pf "%a.length" (ul pp_lhs) v

and pprint_fn_call ( callee, _, arg_names) =
  pl [
      pf "%s" callee;
      ps "(";
      psep ", " @@ List.map pp_lhs arg_names;
      ps ")"
    ]


let rec pp_lhs2 l =
  match l with
  | `Var x -> ps x
  | `Nondet -> ps "_"
  | `BinOp (l1, op, l2) ->      pl [
         pf "%a %s %a" (ul pp_lhs) l1 op (ul pp_lhs) l2
       ]
  
let rec pp_patt = function
  | Ast.PVar v -> pv v
  | Ast.PNone -> ps "_"
  | Ast.PTuple l -> pl [
                    ps "(";
                    psep ", " @@ List.map pp_patt l;
                    ps ")"
                  ]   

                  (*
let rec pp_typ t =
  let t_printer = match t with
    | Int r -> pf "%a int" (ul pp_ref_ast) r
    | Ref (t,o,_) -> pf "%a ref %f" (ul pp_typ) t o
    | Tuple (bl,t) ->
      let ty_printers =
        List.mapi (fun i t ->
          let binder_print = List.filter (fun (_,bind) ->
              match bind with
              | SProj i' when i' = i -> true
              | _ -> false
            ) bl |> List.fold_left (fun _ (v,_) ->
                pf "$%d: " v
              ) null
          in
          pl [
            binder_print;
            pp_typ t
          ]
        ) t in
      pl [
        ps "(";
        psep_gen (pf ",@ ") ty_printers;
        ps ")"
      ]
    (* TODO: make a generic version of type to string for god's sake *)
    | _ -> failwith @@ "Can't print type"
  in
  pb [
    t_printer
  ]
                   *)
(*
let pp_ty_env gamma =
  psep_gen semi @@
    List.map (fun (k, t) ->
      pf "%s: %a" k (ul pp_typ) t
    ) gamma
 *)
                  
let do_need_block all_seq = function
    | Seq _ -> true
      | Alias _ when all_seq -> true
      | Assert _ when all_seq -> true
      | Assign _ when all_seq -> true
      | EAnnot _ when all_seq -> true
      | Update _ when all_seq -> true
      | _ -> false;;

let do_need_semi = function
    (* | Seq _ -> true
    | Alias _ -> true
    | Assert _ -> true
    | Assign _  -> true
    | EAnnot _ -> true
    | Update _ -> true *)
  | Cond _ -> true
  | NCond _ -> true
  | Value _ -> true
    | _ -> false;;
  
let rec pp_expr ~ip:((po_id,pr_id) as ip) ~annot e =
  let e_printer =
    match e with
    | Seq (_, e1, e2) ->
       let need_block = do_need_semi e1 in
       pl [ 
           maybe_brace ~ip ~annot e1;
           if need_block then semi else ps ""; 
           pp_expr ~ip ~annot e2
         ]
    | Let (_, patt, lhs, body) ->
      maybe_brace ~ip ~annot ~all_seq:true ~pre:(pl [
            pf "let %a = %a in "
              (* po_id id *)
              (ul pp_patt) patt
              (ul pp_lhs) lhs
          ]) body
    | Assign (_, x, y) ->
      pl [
          pf "%s := %a" x (ul pp_lhs) y;
          semi
        ]
    | Update (_,x,i,e) ->
      pl [
          pf "%a[%a] <- %a" (ul pp_lhs) x (ul pp_lhs) i (ul pp_lhs) e;
          semi;
        ]
    | NCond (_,v,tr,fl) ->
      pp_ncond ~ip ~annot "ifnull" v tr fl
    | Cond (_,x,tr,fl) ->
      pp_cond ~ip ~annot "if" x tr fl
    | Alias(_,x,y) ->
      pl [
          pf "alias(%s = %a)" x (ul pp_ap) y;
        ]
    | Assert (_, { rop1; cond; rop2 }) ->
      pl [
          pf "assert(%a %s %a)" upp_imm rop1 cond upp_imm rop2;
          semi
        ]
    | Value (_,v) -> pl [
                     pf "%a" (ul pp_lhs) v
                       ]
    | Unit _ -> ps "()"
    | EAnnot(_,ty_env) ->
      ps ""
  in
  match e with
  | Seq _ -> e_printer
  | _ -> (fun ff ->
      annot e ff;
      e_printer ff)
       
and pp_cond ~ip:((po_id,_) as ip) ~annot cond v tr fl =
  pblock
    ~nl:false
    ~op:(pf "%s %a then {" cond (ul pp_lhs2) v)
    ~body:(pp_expr ~ip ~annot tr)
    ~close:(
      pblock ~nl:true ~op:(ps "} else {")
        ~body:(pp_expr ~ip ~annot fl)
        ~close:(ps "}")
    )

and pp_ncond ~ip:((po_id,_) as ip) ~annot cond v tr fl =
  pblock
    ~nl:false
    ~op:(pf "%s %s then {" cond v)
    ~body:(pp_expr ~ip ~annot tr)
    ~close:(
      pblock ~nl:true ~op:(ps "} else {")
        ~body:(pp_expr ~ip ~annot fl)
        ~close:(ps "}")
    )

and maybe_brace ~ip ~annot ?(all_seq=false) ?pre (e as tagged_e) : formatter -> unit =
  let need_block = do_need_block all_seq e in
  if need_block then
    pl [
      indent_from_here;
      (match pre with Some f -> f | None -> null);
      ps "{"; newline;
      pp_expr ~ip ~annot tagged_e; dedent; newline;
      ps "}"
    ]
  else
    pl [
      (match pre with
        Some f -> pl [ f; newline ]
      | None -> null);
      pp_expr ~ip ~annot tagged_e
    ]

let pprint_fn ~ip ~annot_fn ~annot ff (name, args, body) =
  pl [
    annot_fn name;
    pblock
      ~nl:true
      ~op:(pf "%s(%s) {" name @@ String.concat ", " args)
      ~body:(pp_expr ~ip ~annot body)
      ~close:(ps "}");
  ] ff

let pprint_prog ~ip ~annot_fn ~annot ff (fn,body) =
  pp_open_vbox ff 0;
  List.iter (pprint_fn ~ip ~annot_fn ~annot ff) fn;
  pp_close_box ff ();
  pp_open_vbox ff 1;
  fprintf ff "{@;";
  pp_expr ~ip ~annot body ff;
  pp_close_box ff ();
  pp_force_newline ff (); pp_print_string ff "}"

let id_printer labels =
  if labels then
    (fun () (id,_) -> pf ":%d" id),(fun () (id,_) -> pf "%d:" id)
  else
    (fun () _ -> (fun _ff -> ())),(fun () _ -> (fun _ff -> ()))
    
let pretty_print_program ?(with_labels=true) ?(annot_fn=(fun _ _ -> ())) ?(annot=(fun _ _ -> ())) out_chan prog =
  let ff = Format.formatter_of_out_channel out_chan in
  pprint_prog ~ip:(id_printer with_labels) ~annot_fn ~annot ff prog;
  Format.pp_print_newline ff ()

