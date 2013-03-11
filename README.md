
llvm-acse -- ACSE compiler with LLVM
======================================

Back in the old days, Doc. Andrea Di Biagio wrote a simple compiler, ACSE,
intended to be used by students of Politecnico di Milano to learn basic stuffs
about compiler front-ends, such as tokenizing the stream, building the
abstract syntax tree, and generating some kind of code.

The ACSE compiler is very simple: it support only few data-types and control
structures. Moreover, it does not support function definitions. This
"simplicity" is due to the fact that students are usually required to modify the
compiler in a 45-minutes test in class, with pen and paper, and without trying
compiling their code.

The original ACSE compiler is distributed through a web-site accessible only by
students of the Formal Languages and Compilers Course -- at least until I was at
Politecnico. However, there is not any kind of constraint about distributing the
sources and the documentation of ACSE.

At the time of writing, I am at Barcelona Supercomputing Center, waiting for a
free slot on the cluster in order to run my tests. Since I have nothing to do, I
started rewriting ACSE in order to exploit the LLVM compiler suite.

Major differences with respect to original ACSE
-----------------------------------------------

The language supported by this version of ACSE is the same of the original ACSE
compiler, however there are four major differences inside the compiler itself:

1. the scanner is written _by hand_; in the original ACSE compiler ``flex`` was
   used to automatically generate the scanner
2. the parser is written _by hand_; in the original ACSE compiler ``bison`` was
   used to automatically generate a LALR parser
3. the parser is actually an LL parser, and it explicitly builds the AST
4. code emission is not performed together with parsing; llvm-acse does require
   two steps to generate code: parsing the input stream to build an AST, and
   later visiting the AST to emit code

The whole compiler heavily exploits LLVM libraries and build system. LLVM must
be installed before building llvm-acse

Quick install
-------------

To be written when a all compiler tools have been written.

Authors
-------

This version of ACSE has been developed by me,
[Ettore Speziale](mailto:speziale.ettore@gmail.com). Please notice I am not
anymore at Politecnico di Milano, so student emails about ACSE will be silently
dropped.
