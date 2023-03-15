module C = ConsortAst
module F = Ftools
module R = Random
         
module V = struct
  type t = Val of C.lhs | Arr of C.lhs * C.lhs | Ptr of C.lhs * string

  let rec compare_lhs a b =
    match a, b with
      C.(`OVar s1), C.(`OVar s2) -> String.compare s1 s2
    | C.(`OInt s1), C.(`OInt s2) -> s1-s2
    | _, _ -> failwith "Unsupported Compare"      
  ;;
  let compare a b =
    match a,b with
      Val a', Val b' -> compare_lhs a' b'
    | Arr (a1,b1), Arr (a2,b2) ->
       let r = compare_lhs a1 a2 in
       if r = 0 then
         compare_lhs b1 b2
       else
         r 
    | Ptr (a1,s1), Ptr (a2,s2) ->
       let r = compare_lhs a1 a2 in
       if r = 0 then
         String.compare s1 s2
       else
         r
    | Val _, _ -> -1
    | _, Val _ -> 1
    | Arr _, _ -> -1
    | _, Arr _ -> 1
  ;;
  let v = function
      Val l -> l
    | _ -> failwith "Not Val"
  ;;
  let pp = function
      Val (C.(`OVar s)) -> F.pn s
    | _ -> ()
end;;

module S = Set.Make(V)
module T = Set.Make(String)

let debug_str = "_ZN15FastVirtualScan21calculateVirtualScansEiddddddd";;
let selected = "_ZN15FastVirtualScan21calculateVirtualScansEiddddddd";;
let cur_f = ref "";;


let mk_arr a i = V.Arr (a, i)

let mk_ptr a s = V.Ptr (a, s)

let mk_val s = V.Val (C.(`OVar s))

             
let rec fv_lhs = function
    C.(`OVar s) 
  | C.(`ODeref s) -> S.singleton (mk_val s)
  | `Read (l1,l2)
  | `BinOp (l1,_,l2) -> S.union (fv_lhs l1) (fv_lhs l2)
  | `Mkref l
    | `LengthOf l
    | `MkArray l -> fv_lhs l
  | `Call (_, _, xs) | `Tuple (xs) ->
     List.fold_left (fun acc a -> S.union acc @@ fv_lhs a) S.empty xs
  | `Nondet _
    | `OBool _
    | `OInt _
    | `Null
  | _ ->
    S.empty

let rec fv_lhs1 = function
    C.(`Var s) -> S.singleton (mk_val s)
  | C.(`BinOp (l1,_,l2)) -> S.union (fv_lhs l1) (fv_lhs l2)  
  | C.(`Nondet) ->
     S.empty


let pn s =
  if !cur_f = debug_str then
    F.pn s
  else
    ();;
    
let rec is_necessary nvs = function
    Ast.PVar s as v -> if S.mem (mk_val s) nvs then (v, S.singleton (mk_val s)) else (Ast.PNone, S.empty)
  | PNone -> (Ast.PNone, S.empty)
  | PTuple (patts) -> let patts', nv = List.fold_left (fun (patts',nvs') patt ->
                                           let patt', nv = is_necessary nvs patt in
                                           (patts'@[patt'], S.union nvs' nv)  
                                         ) ([], S.empty) patts
                      in (PTuple patts', nv)
;;

let fv_rel r =
  let ss1 = match r.C.rop1 with C.IVar s -> S.singleton (mk_val s) | _ -> S.empty in
  let ss2 = match r.C.rop2 with C.IVar s -> S.singleton (mk_val s) | _ -> S.empty in
  S.union ss1 ss2

let rec slice av e =
  match e with
    C.Unit _ -> false, S.empty, e
  | Value (p, _) -> false, S.empty, C.Unit (p)
  | Cond (p, b, e1, e2) ->
     let r =
       begin
         pn "Cond Beg";
       let ia1, nv1, e1' = slice av e1 in
       let ia2, nv2, e2' = slice av e2 in
       let bv = fv_lhs1 b in
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (p)
       | false, _     -> true,  S.union bv nv2, C.Cond (p, b, e2', Unit (p))
       | _, false     -> true,  S.union bv nv1, C.Cond (p, b, Unit (p), e1')
       | _, _         -> true,  S.union bv (S.union nv1 nv2), C.Cond (p, b, e1', e2')
       end in
     pn "Cond End";
     r
  | NCond (p, s, e1, e2) ->
     let r =
       begin
         pn "NCond Beg";
       let ia1, nv1, e1' = slice av e1 in
       let ia2, nv2, e2' = slice av e2 in
       let bv = S.singleton (V.Val C.(`OVar s)) in
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (p)
       | false, _     -> true,  S.union bv nv2, C.NCond (p, s, e2', Unit (p))
       | _, false     -> true,  S.union bv nv1, C.NCond (p, s, Unit (p), e1')
       | _, _         -> true,  S.union bv (S.union nv1 nv2), C.NCond (p, s, e1', e2')
       end in
     pn "NCond End";
     r
  | Assign (p, v, lhs) ->
     if S.mem (V.Val (C.(`OVar v))) av then
       true, fv_lhs lhs, e
     else
       false, S.empty, C.Unit (p)
  | Update (p, lhs1, lhs2, lhs3) ->
     pn "Update Beg";
     let r =
       if S.mem (mk_arr lhs1 lhs2) av then
       true, fv_lhs lhs3, e
     else
       false, S.empty, e in
     pn "Update End";
     r
  | Let (p, patt, lhs, e1) ->
     let ia, nvs, e1' = slice av e1 in
     pn "Let Beg";
     let patt', nv = is_necessary nvs patt in
     let r =
       if S.is_empty nv then
         ia, nvs, e1'
       else
         let bv = fv_lhs lhs in
         true, S.union bv @@ S.union nv nvs, C.Let(p, patt', lhs, e1') in
     pn "Let End";
     r
  | Alias (p, s, a) ->
     false, S.empty, C.Unit (p)
  | Assert (p, r) ->
     if !cur_f = selected then
       true, fv_rel r, e
     else
       false, S.empty, C.Unit (p)
  | Seq (p, e1, e2) ->
     let ia2, nv2, e2' = slice av e2 in
     pn "Seq Beg";
     let ia1, nv1, e1' = slice (S.union av nv2) e1 in
     let r = begin
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (0,p)
       | false, true  -> true,  nv2,     e2'
       | true,  false -> true,  nv1,     e1'
       | _,     _     -> true,  S.union nv1 nv2, C.Seq (p, e1', e2')
       end in
     pn "Seq End";
     r
  | EAnnot (p, bs) ->
     false, S.empty, C.Unit p
;;

let rec get_calles_from_lhs stack fns l : T.t =
  match l with
    C.(`OVar s) -> T.empty 
  | C.(`ODeref s) -> T.empty
  | `Read (l1,l2) 
    | `BinOp (l1,_,l2) ->
     T.empty
  | `Mkref l
    | `LengthOf l
    | `MkArray l -> T.empty
  | `Call ((fn:string), _, _)
    ->
     if T.mem fn stack then
       T.empty
     else
       begin
         try
           let _,_,body = List.find (fun ((a:string),_,_) -> a=fn) fns in
           get_callees (T.add fn stack) fns body
         with
         | Not_found ->
            T.empty
       end
  | `Tuple (_)
  | `Nondet _
    | `OBool _
    | `OInt _
    | `Null ->
     T.empty

and get_callees stack fns e =
  match e with
    C.Unit _ -> T.empty
  | Value (p, l) ->
     get_calles_from_lhs stack fns l
  | Cond (p, b, e1, e2) ->
     let calls1 = get_callees stack fns e1 in
     let calls2 = get_callees stack fns e2 in
     T.union calls1 calls2
  | NCond (p, s, e1, e2) ->
     let calls1 = get_callees stack fns e1 in
     let calls2 = get_callees stack fns e2 in
     T.union calls1 calls2
  | Assign (p, v, l) ->
     get_calles_from_lhs stack fns l
  | Update (p, lhs1, lhs2, lhs3) ->
     T.empty
  | Let (p, patt, lhs, e1) ->
     get_callees stack fns e1
  | Alias (p, s, a) ->
     T.empty
  | Assert (p, r) ->
     T.empty
  | Seq (p, e1, e2) ->
     let calls1 = get_callees stack fns e1 in
     let calls2 = get_callees stack fns e2 in
     T.union calls1 calls2
  | EAnnot (p, bs) ->
     T.empty
;;    


  
let rec get_all_callees fns (fn, params, body) =
  let all_callees = get_callees T.empty fns body in
  List.filter (fun (fn,_,_) -> T.mem fn all_callees) fns
;;


let rec calls_lhs fns l =
  match l with
    C.(`OVar s) -> false, S.empty, l 
  | C.(`ODeref s) -> false, S.empty, l
  | `Read (l1,l2) 
    | `BinOp (l1,_,l2) ->
     false, S.empty, l
  | `Mkref l
    | `LengthOf l
    | `MkArray l -> false, S.empty, l
  | `Call (fn, n, (xs:C.lhs list)) as l
    ->
     begin
       try
         let _,params,_,necs = List.find (fun (a,_,_,_) -> a=fn) fns in
         let rs = List.map2 (
                      fun (a:C.lhs) b ->
                      if S.mem (mk_val b) necs then
                        (true, (fv_lhs a, a))
                      else (false, (fv_lhs a, C.(`OInt (Random.int 100+1))))
                    ) xs params in
         let xs' = List.split rs |> snd |> List.split |> snd in
         let rs' = List.filter fst rs |> List.map snd in
         let rs'3, _ = List.split rs' in
         let rs'' = List.fold_left S.union S.empty rs'3 in
         let l' = C.(`Call (fn, n, xs')) in
         (true, rs'', l')
       with
       | Not_found ->
          (false, S.empty, l)
       | e ->
          raise e
         (* (false, S.empty, l) *)
     end
  | `Tuple (_)
  | `Nondet _
    | `OBool _
    | `OInt _
    | `Null
  | _ ->
     false, S.empty, l
;;

let rec is_caller av fns e =
  match e with
    C.Unit _ -> false, S.empty, e
  | Value (p, l) ->
     pn "Val Beg";
     let ic, fvs, l' = calls_lhs fns l in
     pn "Val End";
     if ic then
       true, fvs, C.(Value (p,l'))
     else
       false, S.empty, e
  | Cond (p, b, e1, e2) ->
     let r =
       begin
         pn "Cond Beg";
       let ia1, nv1, e1' = is_caller av fns e1 in
       let ia2, nv2, e2' = is_caller av fns e2 in
       let bv = fv_lhs1 b in
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (p)
       | false, _     -> true,  S.union bv nv2, C.Cond (p, b, e2', Unit (p))
       | _, false     -> true,  S.union bv nv1, C.Cond (p, b, Unit (p), e1')
       | _, _         -> true,  S.union bv (S.union nv1 nv2), C.Cond (p, b, e1', e2')
       end in
     pn "Cond End";
     r
  | NCond (p, s, e1, e2) ->
     let r =
       begin
         pn "NCond Beg";
       let ia1, nv1, e1' = is_caller av fns e1 in
       let ia2, nv2, e2' = is_caller av fns e2 in
       let bv = S.singleton (V.Val C.(`OVar s)) in
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (p)
       | false, _     -> true,  S.union bv nv2, C.NCond (p, s, e2', Unit (p))
       | _, false     -> true,  S.union bv nv1, C.NCond (p, s, Unit (p), e1')
       | _, _         -> true,  S.union bv (S.union nv1 nv2), C.NCond (p, s, e1', e2')
       end in
     pn "NCond End";
     r
  | Assign (p, v, l) ->
     let ic, fvs, l' = calls_lhs fns l in
     if ic then
       true, S.union (S.singleton @@ mk_val v) fvs, C.Assign (p, v, l')
     else if S.mem (mk_val v) av then
       true, fv_lhs l, C.Assign (p, v, l')
     else
       ic, S.empty, C.Unit (p)
     
  | Update (p, lhs1, lhs2, lhs3) ->
     false, S.empty, C.Unit (p)
  | Let (p, patt, lhs, e1) ->
     let ia, nvs, e1' = is_caller av fns e1 in
     pn "Let Beg";
     let patt', nv = is_necessary nvs patt in
     let r =
       if S.is_empty nv then
         ia, nvs, e1'
       else
         let bv = fv_lhs lhs in
         true, S.union bv @@ S.union nv nvs, C.Let(p, patt', lhs, e1') in
     pn "Let End";
     r
  | Alias (p, s, a) ->
     false, S.empty, C.Unit (p)
  | Assert (p, r) ->
     false, S.empty, e
  | Seq (p, e1, e2) ->
     let ia2, nv2, e2' = is_caller av fns e2 in
     pn "Seq Beg";
     let ia1, nv1, e1' = is_caller (S.union av nv2) fns e1 in
     let r = begin
       match ia1, ia2 with
         false, false -> false, S.empty, C.Unit (0,p)
       | false, true  -> true,  nv2,     e2'
       | true,  false -> true,  nv1,     e1'
       | _,     _     -> true,  S.union nv1 nv2, C.Seq (p, e1', e2')
       end in
     pn "Seq End";
     r
  | EAnnot (p, bs) ->
     false, S.empty, C.Unit p
;;    

let is_caller_fn fns (fname, args, body) =
  cur_f := fname;
  pn (fname ^ " UPDATE BEGINs");
  let ic, nvs, body' = is_caller S.empty fns body in
  if ic then pn "ic is true" else pn "ic is false";
  ic, (fname, args, body', nvs)

let slice_e e = slice S.empty e

let rec update_funcs n fns nec_funcs =
  if n=0 then
    nec_funcs
  else
    let new_callers =
      List.fold_left (fun acc ((fname,_,_) as fn) ->
          if List.exists (fun (a,_,_,_) -> fname=a) nec_funcs then
            acc
          else
            let is_nec, fn' = is_caller_fn nec_funcs fn in
            if is_nec then
              acc @ [fn']
            else
              acc
        ) [] fns in
    F.pn ("New Callers: " ^ (string_of_int (List.length new_callers)));
    if List.length new_callers = 0 then
      nec_funcs
    else
      update_funcs (n-1) fns (new_callers @ nec_funcs)

let update_body fns body =
  let _, _, body' = is_caller S.empty fns body in
  body'


let slice_fns fns =
  print_endline "Slicer Begins";
  let asserted_funs = List.fold_left (fun acc (name, args, body) ->
                          cur_f := name;
                          let ia, nvs, body' = slice_e body in
                          if ia then (name, args, body', nvs)::acc
                          else acc
                        ) [] fns in
  
  let fns' = update_funcs 10 fns asserted_funs in
  let fns'' = List.map (fun (a,b,c,_) -> (a,b,c)) fns' in
  let callees = List.map (get_all_callees fns) fns'' |> List.concat in
  let fns''' = List.filter (fun (a,_,_) -> List.for_all (fun (b,_,_) -> b<>a) callees) fns'' in
  print_endline "Slicer Ends";
  fns'''@callees
              
let slice_prog (fns, body) =
  print_endline "Slicer Begins";
  Random.self_init ();
  let asserted_funs = List.fold_left (fun acc (name, args, body) ->
                          F.pn ("FN beg " ^ name);
                          cur_f := name;
                          let ia, nvs, body' = slice_e body in
                          S.iter V.pp nvs ;
                          F.pn ("FN end " ^ name);
                          
                          if ia then (name, args, body', nvs)::acc
                          else acc
                        ) [] fns in
  
  let fns' = update_funcs 10 fns asserted_funs in
  let fns'' = List.map (fun (a,b,c,_) -> (a,b,c)) fns' in
  let body' = update_body fns' body in
  print_endline "Slicer Ends";
  (fns'', body')
  
