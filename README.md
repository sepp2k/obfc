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
* LLVM 3.6 with Ocaml bindings
* ocaml-ctypes

Compilation
-----------

    ocamlbuild -use-ocamlfind obfc.native obfi.native

Usage
-----

To compile a program to LLVM bitcode (which can then be converted to native
code using llc or run directly using lli)¹:

    ./obfc.native the-brainfuck-code.b

To directly execute a Brainfuck file (using LLVM's JIT functionality):

    ./obfi.native the-brainfuck-code.b

Footnotes
---------

¹ Note that the generated bitcode file's name will be the name of the Brainfuck
file with a "c" appended at the end. This will have the desired effect of the
bitcode file ending in ".bc" if and only if the Brainfuck file ended in ".b".
I'm aware that this way of determining the file name is less than ideal.