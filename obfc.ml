let my_module, _ = Compiler.compile_file Sys.argv.(1) in
ignore (Llvm_bitwriter.write_bitcode_file my_module (Sys.argv.(1) ^ "c"))
