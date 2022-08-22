open Ftools
module V = Base
module B = Block
         
type t = V.Exp.t * B.t list * Block.t

let mk_procedure name params body =
  let params_body = (fun v -> B.decl v 1) |>>| params in
  (name, params_body, body)

let f_count = ref 1
            
let to_var = function
    Block.ASSIGN (var, _, _, _)
  | Block.CONS (((V.Exp.VAR _) as var), _, _, _)
    | Block.CONS (((V.Exp.BINOP(V.Exp.VAR _, _, _)) as var), _, _, _)
    | Block.DECL (var, _, _, _, _)
    | Block.SARRAY (var, _, _, _, _)
    | Block.MALLOC (var, _, _, _) ->  var
  | x ->
     raise (StError "Procedure")

let print_var a =
  (* p "int ";
  ((if V.Exp.is_struct a then
      let st = V.Exp.get_struct_name a in
      pw st);
   if V.Exp.is_ptrptr a then
     pw "**"
   else if V.Exp.is_ptr a then
     pw "*");
  V.Exp.pprint a *)
  let s = B.string_of_decl a [] in
  p s
;;
    
let pprint ((a, b, c):t) =
  let b = to_var |>>| b in
  pn ""; (* p (Block.extra ""); *)
  print_var a; pw "("; iterS print_var ", " b; pn " )";
  Block.pprint 0 c
