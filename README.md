# Unlambda-Compilers

The Unlambda home page leaves off an as-yet unanswered question: Can Unlambda Be Compiled?

At this point, we must make an important distinction between compilation and interpretation. For our purposes,
"interpreting" a program means to create a representation of the original program as data, and then have a function,
`interpret : ProgramAsData -> Result`, which executes the representation of the program. Notice that this is
distinct from translating the program into _native_ executable code of the target language.

Previous efficient Unlambda implementations (such as `mendelson-unlambda.c` [here](ftp://ftp.madore.org/pub/madore/unlambda/contrib/) or [ReLambda](https://github.com/MattX/relambda)) compile Unlambda to a "bytecode." A representation of
this bytecode is captured as data and then interpreted (by C or by Rust respectively).

The compilers presented here do not work this way. For now, only one of them is publically available via 
this repository. It emits a bytecode which is implemented in C as macros, which themselves expand into 
executable C code. The key difference is that the output C code is not capable of executing a different Unlambda program
if it were presented as bytecode; an interpreter would be able to interpret arbitrary bytecode programs.

More writing about the implementation, complications with compiliing Unlambda, and the solutions used here will
follow. For now, note that the simple POC compiler here currently does not have a garbage collector and has a
fixed heap size, so nontrivial Unlambda programs (especially those using church numerals) will likely run out of
heap space and segfault quickly.
