open Ftools
module V = Base
   
type t = string * (V.Exp.t * V.Term.t) list * V.Exp.t list option

let pprint ((name, l_fields, t) : t) =
  p "struct "; p name; pn "{";
  (* (
    match t with
      None -> pn ""
    | Some fs ->
       p "<"; iterS V.Exp.pprint "," fs; pn ">"); *)
  pt "" 1;
  iterS (fun (l_field_name, value) ->
      let len = try V.Exp.get_array_length l_field_name with _ -> [] in
      let s = Block.string_of_decl l_field_name len in
      p s; (* V.Exp.pprint l_field_name; *)
      if value <> V.Term.NULL then
        (p "="; V.Term.pprint value);
      pn ";"
    ) ", "  l_fields;
  pn "}";;
