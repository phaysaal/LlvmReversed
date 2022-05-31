open Ftools
module V = Base
         
type t =
  | NA
  | STRUCT of Structure.t * V.Locs.t
  | STATEMENT of Block.t
  | PROC of Procedure.t * V.Formula.t * V.Formula.t * (V.Formula.t * V.Formula.t) list * V.Locs.t
  | FFILE of (string * string)

let rec pprint = function
  | NA -> ()
  | STATEMENT a -> begin Block.pprint 0 a end
  | PROC (a, b, c, d, _) ->
     begin
       Procedure.pprint a;
       pn "";
     end
  | STRUCT (st, _) -> Structure.pprint st
  | FFILE (st, _) ->
     let fin = open_in st in
     let pd : t = Marshal.from_channel fin in
     close_in fin;
     pprint pd
         
