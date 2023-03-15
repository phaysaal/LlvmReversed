module L = Llvm
         
let print_fun lv =
  Printf.printf "%s\n" (L.value_name lv);
  Llvm.iter_blocks
    (fun llbb ->
      Printf.printf "  bb: %s\n" (Llvm.value_name (Llvm.value_of_block (llbb))) ;
      Llvm.iter_instrs
        (fun lli ->
          Printf.printf "    instr: %s\n" (Llvm.string_of_llvalue lli)
        )
        llbb
    )
    lv
;;


let rec space i = if i = 0 then "" else "  " ^ space (i-1);;

let get_blk_name blk = L.value_name (L.value_of_block (blk));;

let print_blk ind llbb =
  Printf.printf "%sBLK: %s\n" (space ind) (get_blk_name llbb) ;
  Llvm.iter_instrs
    (fun lli ->
      Printf.printf "%s%s\n" (space ind) (Llvm.string_of_llvalue lli)
    )
    llbb;
  ()
;;


let rec print_blks ind i j blk_arr =
  if i <= j then
    begin
      let llbb = Array.get blk_arr i in
      print_blk ind llbb;
      print_blks ind (i+1) j blk_arr
    end
  else
    ()
;;

let rec string_of_typeval = function
  |	L.TypeKind.Void -> "void"
  |	Half -> "half"
  |	Float -> "float"
  |	Double -> "double"
  |	X86fp80 -> "x86fp80"
  |	Fp128 -> "fp128"
  |	Ppc_fp128 -> "ppc fp128"
  |	Label -> "label"
  |	Integer -> "int"
  |	Function -> "function"
  |	Struct -> "struct"
  |	Array -> "array"
  |	Pointer -> "pointer"
  |	Vector -> "vector"
  |	Metadata -> "metadata"
  |	X86_mmx -> "x86_mmx"
  | _ -> "others"
;;

let print_value_kind = function
  | L.ValueKind.NullValue -> "NullValue"
  |	Argument -> "Argument"
  |	BasicBlock -> "BasicBlock"
  |	InlineAsm -> "InlineAsm"
  |	MDNode -> "MDNode"
  |	MDString -> "MDString"
  |	BlockAddress -> "BlockAddress"
  |	ConstantAggregateZero -> "ConstantAggregateZero"
  |	ConstantArray -> "ConstantArray"
  |	ConstantDataArray -> "ConstantDataArray"
  |	ConstantDataVector -> "ConstantDataVector"
  |	ConstantExpr -> "ConstantExpr"
  |	ConstantFP -> "ConstantFP"
  |	ConstantInt -> "ConstantInt"
  |	ConstantPointerNull -> "ConstantPointerNull"
  |	ConstantStruct -> "ConstantStruct"
  |	ConstantVector -> "ConstantVector"
  |	Function -> "Function" 
  |	GlobalAlias -> "GlobalAlias"
  |	GlobalVariable -> "GlobalVariable" 
  |	UndefValue -> "UndefValue"
  |	Instruction _ -> "Instruction"
  | _ -> "Other Kind Value"
;;


let rec print_type llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> Printf.printf "  integer\n"
  | Llvm.TypeKind.Function -> Printf.printf "  function\n"
  | Llvm.TypeKind.Array    -> Printf.printf "  array of" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Pointer  -> Printf.printf "  pointer to" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Vector   -> Printf.printf "  vector of" ; print_type (Llvm.element_type llty)
  | Llvm.TypeKind.Struct   -> Printf.printf "  struct\n" ;
  | _                      -> Printf.printf "  other type\n"
;;

let rec string_of_type llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer  -> "integer"
  | Llvm.TypeKind.Function -> "function"
  | Llvm.TypeKind.Array    -> "array " ^ string_of_type (L.element_type llty)
  | Llvm.TypeKind.Pointer  -> "pointer " ^ string_of_type (L.element_type llty)
  | Llvm.TypeKind.Vector   -> "vector " ^ string_of_type (L.element_type llty)
  | Llvm.TypeKind.Struct   -> "struct" ;
  | _                      -> "other type\n"
;;



    (*Llvm.dump_module llm ;*)

  (* Printf.printf "*** lookup_function ***\n" ;
  let opt_lv = Llvm.lookup_function "main" llm in
  begin
  match opt_lv with
  | Some lv -> print_val lv
  | None    -> Printf.printf "'main' function not found\n"
  end ;

  Printf.printf "*** iter_functions ***\n" ;
  Llvm.iter_functions print_val llm ;

  Printf.printf "*** fold_left_functions ***\n" ;
  let count =
    Llvm.fold_left_functions
      (fun acc lv ->
        print_val lv ;
        acc + 1
      )
      0
      llm
  in
  Printf.printf "Functions count: %d\n" count ; *)

    

(*
let print_val lv =
  let v = L.value_name lv |> B.var in
  (* let op = L.operand lv 0 in *)
  let ty = L.type_of lv in
  let p =
    if is_array ty then
      let len = L.array_length (L.element_type ty) in
      let s = B.decl v len in
      s
    else
      begin
        (* pf "____________"; *)
        B.SKIP
      end
  in
  (* B.pprint 0 p; *)
  (*
  Printf.printf "Value\n" ;
  Printf.printf "  name %s\n" (Llvm.value_name lv) ;
  
  Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
  print_type llty ; *)
  ()
*)
