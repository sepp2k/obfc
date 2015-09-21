let my_module = Compiler.compile_file Sys.argv.(1)
let _ = Llvm_bitwriter.write_bitcode_file my_module (Utils.result_path Sys.argv.(1) "bc")
