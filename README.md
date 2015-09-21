OBFC - The Optimizing Brainfuck Compiler
========================================

Introduction
------------

This is OBFC, the optimizing Brainfuck compiler. Its goal is to generate
code using an as efficient structure as  possible by analyzing the Brainfuck
program.

Dependencies
------------
* Ocaml 4.02.0 or newer
* LLVM 3.6 or later with Ocaml bindings
* A C compiler accessible as `cc` (to link executables)
* ocaml-ctypes

Compilation
-----------

    ocamlbuild -use-ocamlfind obfc.native obfi.native

Usage
-----

To compile a program:

    ./obfc.native the-brainfuck-code.bf -o name-of-the-executable

The options -emit-llvm, -c and -S can be added as desired to produce LLVM (with -emit-llvm) or native (without -emit-llvm) object files (-c) or assembly files (-S).

To directly execute a Brainfuck file (using LLVM's JIT functionality):

    ./obfi.native the-brainfuck-code.bf