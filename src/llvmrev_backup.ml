module V = Base
module F = Ftools
module T = Trans
module CONF = Map.Make(String)
            
exception Err of string
type filename = string
type fullpath = string
type funcname = string
module VV  = Map.Make(String)
type t = filename * fullpath * Global.t list * Structure.t VV.t * bool * V.Formula.t * (string VV.t)

let conf = ref CONF.empty
       
let root_dir = ref "";;
let comp_dir = ref "";;
let compacted_dir = ref "";;
let transformed_dir = ref "";;
let trans_dir = ref "";;
let filesn = ref 0;;

let checkdir s =
  print_endline s;
  let _ = Sys.command ("mkdir " ^ s) in
  ()
;;

let manage_dirs () =
  let endslash s = String.sub s (String.length s - 1) 1 = "/" in
  let ifslash s = if endslash s then String.sub s 0 (String.length s - 1) ^ "_LlvmRev/" else s ^ "_LlvmRev/" in
  
  root_dir := Sys.argv.(1);
  let lr_dir = ifslash !root_dir in
  let slac_dir = lr_dir ^ "SlacData/" in
  
  if Sys.file_exists lr_dir then
    begin
      let _ = Sys.command ("rm -r " ^ lr_dir) in
      ()
    end;
  comp_dir := slac_dir ^ "LLVM/";
  trans_dir := slac_dir ^ "Translated/";
  T.func_dir := !trans_dir ^ "func/";
  compacted_dir := !trans_dir ^ "Compacted/";
  transformed_dir := !trans_dir ^ "FPTransformed/";

  checkdir lr_dir;
  checkdir slac_dir;
  checkdir !comp_dir;
  checkdir !trans_dir;
  checkdir !T.func_dir;
  checkdir !compacted_dir;
  checkdir !transformed_dir;
  ()
;;

let compile_a_cpp_file (path, file) =
  let extra =
    let rec aux i =
      if i < Array.length Sys.argv then
        Sys.argv.(i) ^ " " ^ (aux (i+1))
      else
        ""
    in
    aux 2 in 
  let str_cmd = "clang -fno-discard-value-names -emit-llvm " ^ extra ^ " -o " ^ !comp_dir ^ file ^ ".bc -c " ^ path in
  T.pf "%s\n" str_cmd;
  let r = Sys.command str_cmd in
  if r = 0 then
    file ^ ".bc"
  else
    raise (Err "clang command not successfull")
;;

let get_all_C_files curdir =
  let rec aux (curdir, dirname) =
    (** Read all the files of current directory *)
    let filesdirs = List.map (fun x -> (curdir ^ "/" ^ x, x)) (Array.to_list (Sys.readdir curdir)) in
    (** Partition among directory and files *)
    let (dirs, files) = List.partition (fun (x, _) -> Sys.is_directory x) filesdirs in
    (** Filter the *.c files *)
    let files =
      List.filter (fun (_, x) ->
          let len = String.length x in
          let r =
            if len > 2 && String.sub x (len-2) 2 = ".c" then
              true
            else if len > 4 && String.sub x (len-4) 4 = ".cpp" then
              true
            else
              false in
          r
        ) files in
    (** Get vpl of all the sources in current directory *)
    let defs = List.map compile_a_cpp_file files in
    (** Get vpl of all the sources in sub-directories *)
    let rdefs = List.flatten (List.map aux dirs) in
    defs @ rdefs
  in
  aux (curdir, curdir)
;;

let compile () =
  get_all_C_files !root_dir 
;;

let write_file i filename fullpath globals structures =
  let flattenpath = F.flatten_path fullpath in            
  let ffile = !trans_dir ^ "/" ^ flattenpath in
  let module_id = i in
  let _Fmod : t = (filename, fullpath, globals, structures, false, V.Formula.empty, VV.empty) in
  F.write_file ffile (module_id, _Fmod);
;;


let save_file i fname (f : Global.t list) =
  let fullpath = (!comp_dir ^ fname) in
  let structures = T.get_structures () in
  write_file i fname fullpath f structures
;;

let translate llvm_files =
  List.mapi (fun i f ->
      let f' = T.translate (!comp_dir ^ f) in
      F.pn_s "DEB" (f ^ " is finished translating");
      save_file i f f';
      (f, f')
    ) llvm_files
;;


let rec read_config ic =
  begin
    let line = input_line ic in
    match String.split_on_char '=' line with
      k::v::_ ->
       conf := CONF.add k v !conf;
    | _ -> ()
  end;
  read_config ic
;;

let configure () =
  if Sys.file_exists ".conf" then
    begin
      let ic = open_in ".conf" in
      try
        read_config ic
      with
        _ -> close_in ic
    end
;;

let _ =
  try
    manage_dirs ();
    configure ();
    let llvm_files = compile () in
    T.pf "Compilation to LLVM is finished\n";
    let gs = translate llvm_files in
    T.pf "Translation to SLAC C is finished\n";

    let consort = try CONF.find "CONSORT" !conf with Not_found -> "" in
    let consortoutdir = try CONF.find "CONSORTOUTDIR" !conf with Not_found -> "" in
    List.iter (fun (f, g) ->
                SlacToConsort.print_consort consort (consortoutdir ^ f) g) gs
  with
    e ->
    T.pf "Exception\n"; raise e
;;
