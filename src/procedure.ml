open Ftools
module V = Base
module B = Block
         
type t = V.Exp.t * B.t list * Block.t

let mk_procedure name params body =
  let params_body = (fun v -> B.decl v 1) |>>| params in
  (name, params_body, body)

let mk_main procs =
  let ctr = ref 0 in
  let procs = [List.hd (List.rev procs)] in
  let calls = List.fold_left (fun acc ((fname : V.Exp.t), params, _) ->
                  
                  let args = List.map (function B.DECL (V.Exp.VAR (v,a), _, _, _, _) ->
                                                 V.Term.EXP (V.Exp.VAR (v^(string_of_int !ctr),a))
                                              | _ -> failwith "Not a declaration"
                               ) params in
                  ctr := !ctr+1;
                  let call = B.PROCCALL (None, V.Term.EXP fname, args, 0, acc, V.Locs.dummy) in
                  let decls = List.fold_left (fun acc a ->
                                  let ea = V.Term.toExp a in
                                  let init = if V.Exp.is_struct ea then
                                               B.MALLOC (ea, (V.Exp.CONST 1), acc, V.Locs.dummy)
                                             else
                                               B.ASSIGN (ea, V.Term.EXP (V.Exp.CONST 1), acc, V.Locs.dummy)
                                  in
                                  B.DECL (ea, [], B.INIT_E,
                                          init, V.Locs.dummy)) call args in
                  decls) B.SKIP procs in 
  let body = B.BLOCK (calls, B.SKIP, V.Locs.dummy) in
  (V.Exp.VAR ("main",[]), [], body)
  
let f_count = ref 1
            
let to_var = function
    Block.ASSIGN (var, _, _, _)
    | Block.DECL (var, _, _, _, _)
    | Block.MALLOC (var, _, _, _) ->  var
  | x ->
     raise (StError "Procedure")

let print_var a =
  p "int ";
  ((if V.Exp.is_struct a then
      let st = V.Exp.get_struct_name a in
      pw st);
   if V.Exp.is_ptrptr a then
     pw "**"
   else if V.Exp.is_ptr a then
     pw "*");
  V.Exp.pprint a
;;
    
let pprint ((a, b, c):t) =
  let b = to_var |>>| b in
  pn ""; (* p (Block.extra ""); *)
  print_var a; pw "("; iterS print_var ", " b; pn " )";
  Block.pprint 0 c


let fp_cut (procs : t list) =
  let proc_names = List.map (fun (a,_,_) -> V.Exp.toStr a) procs in
  List.iter (fun p -> print_endline ("PROC >> " ^ p)) proc_names ;
  let fp_args    = List.fold_left (fun acc (_,_,body) ->
                       let fp_args = Block.get_fp_args proc_names body in
                       acc @ fp_args
                     ) [] procs in
  let fp_params  = List.map (fun (fn, arg, fp_arg) ->                       
                       try
                         let (pn, params, _) = List.find (fun (pn,_,_) -> pn=V.Term.toExp fn) procs in
                         let param = List.nth params arg in
                         (fn, V.Exp.toStr @@ to_var param, fp_arg)
                       with
                         Not_found ->
                         (fn, "", fp_arg)
                     ) fp_args in
  let module D = Map.Make(V.Term) in
  let module S = Map.Make(String) in
  let fp_map = List.fold_left (fun acc (fn, param, fp_arg) ->
                   try
                     let p_map = D.find fn acc in
                     try
                       let fp_args = S.find param p_map in
                       let p_map' = S.add param (fp_arg::fp_args) (S.remove param p_map) in
                       let map = D.add fn p_map' (D.remove fn acc) in
                       map
                     with
                       Not_found ->
                       let p_map' = S.add param [fp_arg] p_map in
                       let map = D.add fn p_map' (D.remove fn acc) in
                       map
                   with
                     Not_found ->
                     let p_map = S.add param [fp_arg] S.empty in
                     let map = D.add fn p_map (D.remove fn acc) in
                     map
                 ) D.empty fp_params in
  
  let procs' = List.map (fun (pn, params, body) ->
                   try
                     let p_map = D.find (V.Term.EXP pn) fp_map in
                     let body' = Block.replace_fp p_map body in
                     (pn, params, body')
                   with
                     Not_found ->
                     (pn, params, body)
                 ) procs in
  List.iter (fun p -> pprint p) procs';
  procs'
