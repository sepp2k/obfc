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
* ocaml-ctypes

Compilation
-----------

    ocamlbuild -use-ocamlfind obfc.native obfi.native

Usage
-----

To compile a program to LLVM bitcode (which can then be converted to native
code using llc or run directly using lli):

    ./obfc.native the-brainfuck-code.bf

To directly execute a Brainfuck file (using LLVM's JIT functionality):

    ./obfi.native the-brainfuck-code.bf