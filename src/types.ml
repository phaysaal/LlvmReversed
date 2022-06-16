module L = Llvm
module V = Base
module E = V.Exp
module LT = L.TypeKind
module F = Ftools
module P = Print
          
let rec get_types ty =
  let tp = L.classify_type ty in
  F.dbgf "VAR" "Type: %s" (P.string_of_type ty);
  match tp with
    LT.Integer ->
     let n = L.integer_bitwidth ty in
     [E.SIMPLE (n/8)]
  | LT.Function ->
       let ret_type = get_types @@ L.return_type ty in
       (* let param_type = List.map get_types @@ (Array.to_list @@ L.param_types ty) in *)
       [E.FUNC (ret_type, [])]
  | LT.Void -> []
  |	LT.Half -> [E.SIMPLE (E.simple_size "short")]
  |	Float   -> [E.SIMPLE (E.simple_size "float")]
  |	Double  -> [E.SIMPLE (E.simple_size "double")]
  |	X86fp80 -> [E.SIMPLE 10]
  |	Fp128   -> [E.SIMPLE 16]
  |	Ppc_fp128 -> [E.SIMPLE 16]
  |	Label -> []
  |	Struct ->
     let stn =
       match L.struct_name ty with
         Some s -> (F.corr_structname s)
       | None -> ""
     in
     [E.STRUCT stn] (* @ get_types @@ L.element_type ty *)
  |	Array ->
     let sz = L.array_length ty in
     let size_e = E.CONST sz in
     [E.ARRAY [size_e]] @ get_types @@ L.element_type ty
  |	Pointer ->
     F.dbgf "VAR" "A Pointer ";
     [E.PTR] @ get_types @@ L.element_type ty
  |	Vector -> []
  |	Metadata -> []
  |	X86_mmx -> []
  | _ -> []
;;
