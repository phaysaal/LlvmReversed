module G = Global
module S = Block
module V = Base
module E = V.Exp
module T = V.Term
module B = V.BExp
module FM = V.Formula


module DM = Map.Make(String)
module S_E = Set.Make(E);;
module S_S = Set.Make(String);;

let nec_funcs = ref S_S.empty;;
let funcs : Procedure.t list ref = ref [];;
let asserted = ref S_S.empty;;
let cur_fun = ref "";;
let is_asserted = ref false;;

let add_vs vars ls =
  List.fold_left (fun a v -> S_E.add v a) vars ls
  
let to_ss ls =
  add_vs S_E.empty ls
  
let rec prune_body ret_mode vs body =
  
  
  match body with
    S.ASSERT (a, y, l) ->
     asserted := S_S.add !cur_fun !asserted;
     is_asserted := true;
    let vars', y' = prune_body ret_mode vs y in
    let vars = V.Formula.fv (List.hd a) in
    let ss = to_ss vars in
    let y'3 = S.ASSERT (a, y', l) in
    S_E.union ss vars', y'3
  | SKIP ->
     S_E.empty, SKIP
  | ASSIGN (a, b, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     if S_E.mem a vars' || S_E.mem a vs || E.is_global a then
       let vars'' = add_vs vars' (T.fv b) in
       vars'', ASSIGN (a, b, y', l)
     else
       vars', y'
  | IF (a, b, c, y, l) ->
     
     let vars', y' = prune_body ret_mode vs y in
     
     is_asserted := false;
     let md1, _ = S.mod_free_var b in
     let cm1 = S_E.union vs @@ S_E.inter md1 vars' in
     let vs1, b' = prune_body ret_mode cm1 b in 
     let as1 = !is_asserted in
     is_asserted := false;
     let md2, _ = S.mod_free_var c in
     let cm2 = S_E.union vs @@ S_E.inter md2 vars' in
     let vs2, c' = prune_body ret_mode cm2 c in
     let as2 = !is_asserted in

     (* let b' =
       if as1 then
         b'
       else if S_E.is_empty cm1 then
         S.SKIP
       else
         b'
     in
     let c' =
       if as2 then
         c'
       else if S_E.is_empty cm2 then
         S.SKIP
       else
         c'
     in *)
     let vars'' = if as1 || as2 then S_E.union (S_E.union vs1 vs2) (add_vs vars' (B.fv a)) else vars' in
     let body' = if as1 || as2 then S.IF (a, b, c, y', l) else y' in
     
     vars'', body' 
  | WHILE (a, bs, b, c, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     let md1, _ = S.mod_free_var b in
     let cm1 = S_E.inter md1 vars' in
     let b' =  prune_body ret_mode cm1 b |> snd in
     if b' = S.SKIP then
       vars', y'
     else
       vars', WHILE (a, bs, b', c, y', l)
  | PROCCALL (z, a, b, i, y, l) ->  (* _ZL10getConnectiPiS_ *)
     let vars', y' = prune_body ret_mode vs y in
     
     let vs' = List.map T.fv b |> List.concat in
     let z' = match z with
         Some sz -> to_ss (sz::vs')
       | None -> to_ss vs' in
     let vars'' = S_E.union vars' z' in
     if S_E.is_empty (S_E.inter vars' z') || S_E.is_empty (S_E.inter vs z') then
       begin
         let ea = T.toStr a in
         (match z with
            Some ez ->
             let is_ez = S_E.mem ez z' in
             prune_funcs false is_ez vars'' ea
          | None -> prune_funcs false false vars'' ea);
         if S_S.mem ea !asserted then
           vars'', PROCCALL (z, a, b, i, y', l)
         else
           vars', y'
       end
     else
       begin
         (match z with
           Some _ -> prune_funcs true true vars'' (T.toStr a)
         | None -> prune_funcs true false vars'' (T.toStr a));
         vars'', PROCCALL (z, a, b, i, y', l)
       end
  | MUTATION (a, b, c, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     let ea = T.toExp a in
     if S_E.mem ea vars' || S_E.mem ea vs || E.is_global ea then
       let vars'' = add_vs vars' (T.fv c) in
       vars'', MUTATION (a, b, c, y', l)
     else
       vars', y'
  | LOOKUP (a, b, c, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     if S_E.mem a vars' || S_E.mem a vs (* || E.is_global a *) then
       let vars'' = add_vs vars' (T.fv b) in
       vars'', LOOKUP (a, b, c, y', l)
     else
       vars', y'
  | MALLOC (a, tl, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     if S_E.mem a vars' || S_E.mem a vs || E.is_global a then
       let vars'' = add_vs vars' (E.fv tl) in
       vars'', MALLOC (a, tl, y', l)
     else
       vars', y'
  | RETURN (i, y, l) ->
     if ret_mode then
       to_ss (T.fv i), RETURN (i, y, l)
     else
       S_E.empty, RETURN (i, y, l)
  | DECL (a, len, init_data, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     if S_E.mem a vars' || S_E.mem a vs || E.toStr a = "retval" then
       let fvs = S.fv_of_init init_data in
       let vars'' = S_E.union fvs (add_vs vars' (List.map E.fv len |> List.concat)) in
       vars'', DECL (a, len, init_data, y', l)
     else
       vars', y'
  | DISPOSE (a, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     let ea = T.toExp a in
     if S_E.mem ea vars' || S_E.mem ea vs || E.is_global ea then
       vars', DISPOSE (a, y', l)
     else
       vars', y'
  | BLOCK (a, y, l) ->
     let vars', y' = prune_body ret_mode vs y in
     let a' =  prune_body ret_mode vars' a |> snd in
     if a' = S.SKIP then
       vars', y'
     else
       vars', BLOCK (a', y', l)

and prune_func (rm:bool) vs fn = function
    (nm, params, body) when E.toStr nm=fn ->
     let f = !cur_fun in
     cur_fun := fn;
     let vars, body' = prune_body rm vs body in
     cur_fun := f;
     (nm, params, body')
  | e -> e

and prune_funcs nec rm vs fn =
  if nec then
    begin
      nec_funcs := S_S.add fn !nec_funcs ;
      funcs := List.map (prune_func rm vs fn) !funcs
    end
  else
    begin
      print_endline "------------------";
      print_endline ("PROC " ^ fn ^ " " ^ (if rm then "RETMODE" else "not RETMODE"));
      funcs := List.map (prune_func rm vs fn) !funcs;
      print_endline "==================";
      print_endline ("PROC " ^ fn ^ " " ^ (if S_S.mem fn !asserted then "ASSERTED" else "not ASSERTED"));
      if S_S.mem fn !asserted then
        nec_funcs := S_S.add fn !nec_funcs
    end
;;

let get_mod_body fn =
  let f = ref S.SKIP in
  List.iter (function (nm1,_,body) when nm1=fn -> f:=body | _ -> ()) !funcs;
  !f
;;

(*
let prune_progs globals =
  funcs := List.filter (function G.PROC _ -> true | _ -> false) globals;
  prune_funcs true false S_E.empty "main";
  let globals' = List.filter (function
                       G.PROC ((nm, params, body), a, b, c, d) when S_S.mem (E.toStr nm) !nec_funcs ->
                        true
                     | G.PROC _ -> false
                     | e -> true
                 ) globals in
  let globals'' = List.map (function
                        G.PROC ((nm, params, body), a, b, c, d) when S_S.mem (E.toStr nm) !nec_funcs ->
                         let body' = get_mod_body nm in
                         let body'' = if body' = S.SKIP then body else body' in
                         G.PROC ((nm, params, body''), a, b, c, d)
                      | e -> e
                    ) globals' in
  print_endline "*** *** *** *** *** *** ***";
  List.iter (G.pprint ) globals'';
  globals''
;;
 *)

let prune_procs procs =
  funcs := procs;
  prune_funcs true false S_E.empty "main";
  
  let procs' = List.filter (fun (nm, params, body) -> S_S.mem (E.toStr nm) !nec_funcs) procs in
  let procs'' = List.map (fun (nm, params, body) ->
                      let body' = get_mod_body nm in
                      let body'' = if body' = S.SKIP then body else body' in
                      (nm, params, body'')
                    ) procs' in
  print_endline "*** *** *** *** *** *** ***";
  procs''
;;


let rec get_block_for nec_vs body = body

let rec get_necessary_body_for_assert body =
  match body with
    S.SKIP ->
     false,
     S_E.empty,
     body
  | _ ->
     let (is_asserted, nec_vs, next') = get_necessary_body_for_assert @@ S.next body in
     match body with
       S.ASSERT (b, _, l) ->
        true,
        to_ss @@ FM.fv (List.hd b),
        S.ASSERT (b, next', l)
     | S.IF (b, p1, p2, _, l) ->
        let ia1, fvs1, p1' = get_necessary_body_for_assert p1 in
        let ia2, fvs2, p2' = get_necessary_body_for_assert p2 in
        if ia1 || ia2 then
          true, S_E.union nec_vs @@ to_ss @@ B.fv b, S.IF (b, p1', p2', next', l)
        else
          let mv1,_ = S.mod_free_var p1 in
          let mv2,_ = S.mod_free_var p2 in
          let mvs = S_E.inter nec_vs @@ S_E.union mv1 mv2 in
          if S_E.is_empty mvs then
            is_asserted,
            S_E.empty,
            S.SKIP
          else
            is_asserted,
            S_E.union nec_vs @@ to_ss @@ B.fv b,
            S.IF (b, get_block_for mvs p1, get_block_for mvs p2, next', l)
     | S.WHILE (b, a, p, c, _, l) ->
        let ia, fvs, p' = get_necessary_body_for_assert p in
        if ia then
          true, S_E.union nec_vs @@ to_ss @@ B.fv b, S.WHILE (b, a, p', c, next', l)
        else
          let mv,_ = S.mod_free_var p in
          let mvs = S_E.inter nec_vs mv in
          if S_E.is_empty mvs then
            is_asserted,
            S_E.empty,
            S.SKIP
          else
            is_asserted,
            S_E.union nec_vs @@ to_ss @@ B.fv b,
            S.WHILE (b, a, get_block_for mvs p, c, next', l)
     | S.BLOCK (p, _, l) ->
        let ia, fvs, p' = get_necessary_body_for_assert p in
        if ia then
          true,
          nec_vs,
          S.BLOCK (p', next', l)
        else
          let mv,_ = S.mod_free_var p in
          let mvs = S_E.inter nec_vs mv in
          if S_E.is_empty mvs then
            is_asserted,
            S_E.empty,
            next'
          else
            is_asserted,
            nec_vs,
            S.BLOCK (get_block_for mvs p, next', l)
     | S.ASSIGN (v, e, _, l) ->
        if S_E.mem v nec_vs then
          is_asserted,
          S_E.union nec_vs @@ to_ss @@ T.fv e,
          S.ASSIGN (v, e, next', l)
        else
          is_asserted,
          nec_vs,
          next'
     | _ ->
        is_asserted,
        nec_vs,
        next'
;;

(*
let get_necessary_funcs procs (pname, params, body) =
  let body'           = get_necessary_body_for_assert body in
  let callers, called = get_necessary_procs_for_asserted_body body' in
  let all_callers     = get_all_callers callers in
  let all_called      = get_all_called called in
  all_callers @ all_called
 *)

(*
let slice_program procs =
  let asserted_procs = List.filter (fun (_,_,body) -> S.has_assert body) procs in
  let procs' = List.fold_left (fun acc proc -> acc @ get_necessary_funcs procs proc) [] asserted_procs in
  procs'
 *)


(**
   Find the function 
 *)
