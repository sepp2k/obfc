let outfile_arg: string option ref = ref None
let infile_arg: string option ref = ref None

let args =
  Arg.align [
      "-o", Arg.String (fun f -> outfile_arg := Some f), "outfile The name of generated bitcode file. If this option is not used, the name of the generated file will be that of the  source file with its extension changed to 'bc'.";
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

let infile =
  match !infile_arg with
  (* Have to provide a filename *)
  | None -> wrong_usage ()
  | Some f -> f

let outfile =
  match !outfile_arg with
  | None -> Utils.result_path infile "bc"
  | Some f -> f

let my_module = Compiler.compile_file infile
let _ = Llvm_bitwriter.write_bitcode_file my_module outfile
