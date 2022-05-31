open Ftools
module V = Base
   
type t = string * (V.Exp.t * V.Term.t) list * V.Exp.t list option

let pprint ((name, l_fields, t) : t) =
  p name;
  (
    match t with
      None -> pn ""
    | Some fs ->
       p "<"; iterS V.Exp.pprint "," fs; pn ">");
  pt "" 1;
  iterS (fun (l_field_name, value) -> V.Exp.pprint l_field_name; if value <> V.Term.NULL then (p "="; V.Term.pprint value)) ", "  l_fields; pn "";;
