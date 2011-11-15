open Llvm_executionengine;;

let opt = 2 in
let my_module, main = Compiler.compile_file Sys.argv.(1) in
let ee =
  (* Apparently initialize_native_target returns *false* when there is a native target
     and true when there isn't *)
  if not (initialize_native_target ()) then
    ExecutionEngine.create_jit my_module opt
  else
    begin
      prerr_endline "Could not initialize native target for JIT, using interpreter";
      ExecutionEngine.create my_module
    end
in
ExecutionEngine.run_static_ctors ee;
ignore (ExecutionEngine.run_function main [||] ee);
ExecutionEngine.dispose ee
