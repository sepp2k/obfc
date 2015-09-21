type llvm_or_native =
  | Llvm
  | Native

(* Assembly or object file*)
type result_type =
  | Object
  | Assembly
  | Executable

let file_extension llvm_or_native result_type =
  match llvm_or_native, result_type with
  | Llvm, Object -> ".bc"
  | Llvm, Assembly -> ".ll"
  | Llvm, Executable -> failwith "There's no such thing as an LLVM executable - this combination should have been rejected."
  | Native, Object -> ".o"
  | Native, Assembly -> ".s"
  | Native, Executable -> ""

let outfile_arg: string option ref = ref None
let infile_arg: string option ref = ref None
let llvm_or_native = ref Native
let result_type = ref Executable

let set_result_type t () =
  if !result_type <> Executable
  then raise (Arg.Bad "-S and -c can't be used in combination or more than once.")
  else result_type := t

let args =
  Arg.align [
      "-o", Arg.String (fun f -> outfile_arg := Some f), "outfile The name of generated bitcode file. If this option is not used, the name of the generated file will be that of the  source file with its extension changed to '.bc', '.ll', '.o', '.s' or nothing when emitting LLVM object code, LLVM assembly code, native object code, native assembly code or native executable respectively.";
      "-emit-llvm", Arg.Unit (fun () -> llvm_or_native := Llvm), " Emit LLVM bitcode (with -c) or assembly (with -S). Can't be used without -S or -c.";
      "-S", Arg.Unit (set_result_type Assembly), " Emit assembly code instead of an executable.";
      "-c", Arg.Unit (set_result_type Object), " Emit object file instead of an executable.";
    ]

let usage = Printf.sprintf "%s [options] brainfuck-file.bf" Sys.executable_name

let wrong_usage () =
  Arg.usage args usage;
  exit 1

let set_infile f =
  match !infile_arg with
  | None -> infile_arg := Some f
  (* Can't provide more than one filename *)
  | Some _ -> wrong_usage ()

let () =  Arg.parse args set_infile usage

(* Catch the illegal llvm+executable combination here, so we can assume that it
   doesn't occur in the rest of the code, allowing us to call file_extension *)
let () =
  match !llvm_or_native, !result_type with
  | Llvm, Executable ->
     prerr_endline "-emit-llvm has to be used with -S or -c";
     wrong_usage ()
  | _ ->
     ()

let infile =
  match !infile_arg with
  (* Have to provide a filename *)
  | None -> wrong_usage ()
  | Some f -> f

let outfile =
  match !outfile_arg with
  | None -> Utils.result_path infile (file_extension !llvm_or_native !result_type)
  | Some f -> f

open Llvm_target
let llvm_target_machine () =
  Llvm_all_backends.initialize ();
  let triple = Target.default_triple () in
  TargetMachine.create triple (Target.by_triple triple)

let my_module = Compiler.compile_file infile
let () =
  match !llvm_or_native, !result_type with
  | Llvm, Object ->
     if Llvm_bitwriter.write_bitcode_file my_module outfile
     then ()
     else begin
         prerr_endline "Error: Failed to write bitcode file.";
         exit 1
       end
  | Llvm, Assembly ->
     Llvm.print_module outfile my_module
  | Llvm, Executable ->
     failwith "There's no such thing as an LLVM executable - this combination should have been rejected."
  | Native, Object ->
     TargetMachine.emit_to_file my_module CodeGenFileType.ObjectFile outfile (llvm_target_machine ())
  | Native, Assembly ->
     TargetMachine.emit_to_file my_module CodeGenFileType.AssemblyFile outfile (llvm_target_machine ())
  | Native, Executable ->
     let obj_file = Filename.temp_file "obfc-tmp-obj-file-" ".o" in
     TargetMachine.emit_to_file my_module CodeGenFileType.ObjectFile obj_file (llvm_target_machine ());
     if Sys.command (Printf.sprintf "cc -o %s %s" (Filename.quote outfile) (Filename.quote obj_file)) = 0
     then ()
     else begin
         prerr_endline "Error: Failed to create executable.";
         exit 1
       end;
     Sys.remove obj_file
