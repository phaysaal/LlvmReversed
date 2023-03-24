module L = Llvm
module V = Base
module E = V.Exp
module LT = L.TypeKind
module F = Ftools
module P = Print

type structure_t = string * L.lltype * E.attr list list
                 
let structures : structure_t list ref = ref []

let has_ptr = List.exists (function E.PTR -> true | _ -> false)
let has_array = List.exists (function E.ARRAY _ -> true | _ -> false)

let rec get_struct llt =
    match L.classify_type llt with
      LT.Struct ->
      Some llt
    | Integer ->
       None
    | Function ->
       None
    | Array
      | Pointer
      | Vector
      ->
       get_struct @@ L.element_type llt
    | _ -> None
;;
                                      
let add_structure name lltype converted_fields =
  F.dbgf "STRUCTADD" "%s is added with %d fields" name (List.length converted_fields);
  (* F.pw name; F.pn (L.string_of_lltype lltype); *)
  structures := (name, lltype, converted_fields)::!structures
;;
     
let rec construct_struct ty =
  F.dbgf "XXX" "@@STR: %s" (L.string_of_lltype ty);
  let fields', llfields = get_field_attrs ty in
  let frv = "STRUCT_" ^ (Block.fresh_var [] |> E.var_decode) in
  add_structure frv ty fields';
  
  List.iter add_rec_struct llfields;
  frv

and get_field_attrs ty =
  F.dbgf "STRUCTADD" "ty %s "  (L.string_of_lltype ty);
  let fields = L.struct_element_types ty in
  F.dbgf "STRUCTADD" "array lenght %d "  (Array.length fields);
  let l_fields = Array.to_list fields in
  let fields' = List.map (fun t ->
                    let tt = get_types t in
                    tt
                  ) l_fields in
  fields', l_fields
  
and get_structure ty =
  try
    List.find (fun (_, typ, _) -> ty=typ) !structures
    |> fun (name, _, _) -> name
  with
    Not_found ->
    let struct_name = construct_struct ty in
    struct_name

and get_struct_name ty =
  let stn =
    match L.struct_name ty with
      Some s -> F.corr_structname s
    | None ->
       get_structure ty
  in
  stn
  
and get_array_length ty =
  let tp = L.classify_type ty in
  match tp with
  |	LT.Array ->
     let sz = L.array_length ty in
     sz 
  |	LT.Pointer -> get_array_length @@ L.element_type ty
  |	Vector -> get_array_length @@ L.element_type ty
  | _ -> failwith "Not an array"
  
and get_types ty =
  let tp = L.classify_type ty in
  F.dbgf "VAR" "Type: %s" (P.string_of_type ty);
  match tp with
    LT.Integer ->
     let n = L.integer_bitwidth ty in
     [E.SIMPLE (n/8)]
  | LT.Function ->
       let ret_type = get_types @@ L.return_type ty in
       (* let param_type = List.map get_types @@ (Array.to_list @@ L.param_types ty) in *)
       if L.is_var_arg ty then
         [E.FUNC (ret_type, [E.ARRAY []])]
       else
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
     let stn = get_struct_name ty in
     F.dbgf "XXX" "## ST: %s" stn ;
     [E.STRUCT stn] (* @ get_types @@ L.element_type ty *)
  |	Array ->
     let sz = L.array_length ty in
     let size_e = E.CONST sz in
     let other_typ = get_types @@ L.element_type ty in
     let arr_typ, rest_typ = List.partition (function E.ARRAY _ -> true | _ -> false) other_typ in
     let size_all = List.fold_left (fun sz -> function E.ARRAY s -> sz@s | _ -> sz) [size_e] arr_typ in
     [E.ARRAY size_all] @ rest_typ 
  |	Pointer ->
     F.dbgf "VAR" "A Pointer ";
     let ptr_typ = get_types @@ L.element_type ty in
     if not (has_ptr ptr_typ) && has_array ptr_typ then
       ptr_typ
     else
       [E.PTR] @ ptr_typ
  |	Vector -> []
  |	Metadata -> []
  |	X86_mmx -> []
  | _ -> []

and add_rec_struct llt =
    match get_struct llt with
      None -> ()
    | Some llt ->
       let nm = L.struct_name llt in
       match nm with
         Some snm' ->
          let snm = F.corr_structname snm' in
          if List.exists (function (s,_,_) -> s=snm) !structures then
            begin () end
          else
            begin
              
              let fields', l_flds = get_field_attrs llt in
              add_structure snm llt fields';
              
              List.iter add_rec_struct l_flds;
              
            end
       | None ->
          ()    

;;
