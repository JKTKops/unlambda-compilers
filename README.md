# Unlambda-Compilers

The Unlambda home page leaves off with a formerly unanswered question: Can Unlambda Be Compiled?

At this point, we must make an important distinction between compilation and interpretation. For our purposes,
"interpreting" a program means to create a representation of the original program as data, and then have a function,
`interpret : ProgramAsData -> Result`, which executes the representation of the program. Notice that this is
distinct from translating the program into _native_ executable code of the target language.

Here, we present various compilers for Unlambda (at the time of writing, there are 3, but I plan to explore some more!),
which gives a positive answer to the question. Unlambda can in fact be compiled into a "reasonable" language like C.
Furthermore, the object code produced by our compilers consistently outperforms the best Unlambda interpreters.

Previous efficient Unlambda implementations such as `mendelson-unlambda.c`, which can be found on the Unlambda
Homepage (GitHub MarkDown won't render links to FTP repositories), and [ReLambda](https://github.com/MattX/relambda) 
compile Unlambda to a "bytecode." A representation of this bytecode is captured as data and then 
interpreted (by C or by Rust respectively).

The compilers presented here do not work this way. We present two compilers targetting C in
this repository. They emit a low-level virtual machine code which is implemented in C as macros, which expand into 
executable C code. The key difference is that the output C code is not capable of executing a different Unlambda program
if it were presented (somehow) as data; an interpreter would be able to interpret arbitrary bytecode programs.

The C Compilers use a small lisp dialect as an intermediate representation. The strategy for compiling this
dialect is inspired by the 90-minute scheme compiler, but spins off in various directions from the inspiration and
is written in Haskell rather than Scheme.

### Implemented Strategies

The C-target compilers here currently do not have a garbage collector and have a
fixed heap size, so nontrivial Unlambda programs (especially those using church numerals) will likely run out of
heap space and segfault quickly. The "duplicate" C-target compiler will fail on most Unlambda programs with
non-trivial nesting depth of applications that have `d` as a free variable. The "delay" C-target compiler will
do better, but it uses flat closure conversion which is still easily overwhelemed. I plan to experiment with linked
closure conversion in the near future.

The Scheme-target compiler works more consistently. The object code is probably portable but was aimed at the
Chez Scheme system.

### Usage

A small bash script is provided in `unlc` for running the compilers conveniently. It requires `runhaskell` to be
on the path. If the backend is C, it expects `gcc` to be on the path. If the backend is Scheme, it expects
`scheme` and `petite` (both from the Chez Scheme system) to be on the path. It is not particularly smart and lacks
options, so use the compilers manually for finer control.

`unlc` calls `gcc` and `scheme` with the equivalent of `-O3` always. For `scheme`, this can lead to long compilation
times because Chez finds a lot to optimize (no surprise there). For GCC it's not as much of a problem as the code being
compiled is lower-level; however our C-target compilers do very little optimization on the IR at the time of writing.
Such optimization was not the topic of the thesis.

`unlc` must be run from the same directory as the Haskell source for the backend you want to use. This is easy to change
if you like, but doing it this way let us iterate faster while testing. Type `unlc --help` for a usage message. It supports
`--backend (c|scheme)` for backend selection (default `scheme`). On the `c` backend, it supports `--delay-arg` and
`--duplicate-arg` to select the mode used for the `apply` macro in the IR. If the input file is `file.unl`, the output
will be an artifact in `file.{c,scheme,so}` and an executable in `file`. Installing executables from the `c` backend is
trivial. Installing executables from the `scheme` backend is a little bit harder; `scheme` produces a shared object
binary and the executable uses `petite` to link it to a scheme runtime for execution. Make sure the executable can
access the shared object. It's just a bash script, so modify it as needed.

If you actually use these compilers for anything (academic or recreational) please let me know!

### Where can I read more?

More writing about the implementation, complications with compiling Unlambda, and the solutions used here can be found
in my undergraduate senior thesis, Compiling Unlambda, which is not currently available online.

