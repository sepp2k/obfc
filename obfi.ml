let module EE = Llvm_executionengine in
let options =
  { EE.default_compiler_options with
    EE.opt_level = 3
  }
in
let my_module = Compiler.compile_file Sys.argv.(1) in
let ee = EE.create ?options:(Some options) my_module in
EE.run_static_ctors ee;
let main_ctype = Foreign.funptr (Ctypes.(void @-> returning void)) in
ignore (EE.get_function_address "main" main_ctype ee ());
EE.dispose ee
