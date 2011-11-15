open Ocamlbuild_plugin;;

ocaml_lib ~extern:true "llvm";;
ocaml_lib ~extern:true "llvm_analysis";;
ocaml_lib ~extern:true "llvm_bitwriter";;
ocaml_lib ~extern:true "llvm_executionengine";;

flag ["link"; "ocaml"; "g++"] (S[A"-cc"; A"g++ -w"]);;
