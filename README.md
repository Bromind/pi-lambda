# pi-lambda

## Dependencies

To build: 
 - ocamlc (tested version 4.04.2)
 - menhir (tested version 20171013)
 - ocamllex (tested version 4.04.2)
It also uses `unix.cma` and `threads.cma` to build, which should be included in your ocaml install.

To run: 
 - Your terminal should print utf8 characters. The types contains greek characters such as "α", etc. 

## Build & run

Build process:

```sh
$ git clone https://github.com/Bromind/pi-lambda.git
$ cd pi-lambda/src
$ make all
```
This builds `./inter` and `./repl`. `./inter` is an interpreter (it accepts the file with `--input file`) and `./repl` is a *read/eval/print loop*.

## Short explanation 

This language is a merge between the λ-calculus and the π-calculus, meaning that you can define functions (e.g. the identity function):

```
\a -> a
```

but you can also have communication channels on which you can send or deliver data:

```
\a -> #c. c[a].a
```

The above program is a function which opens a channel `c` and sends the argument on that channel (`c[a].a` means "send `a` on `c` and continue with `a`"). Similarly, receiving a data is done with the following:

```
#c. c, x > x
```

This opens a channel `c` and waits for a value `x` to be returned (`c, x > x` means "wait for a message `x` on `c` and continue with `x`").

This is not quite useful without the possibility to run processes in parallel:

```
\a -> \b -> [a || b]
```

This waits for two terms `a` and `b` and then executes them in parallel. Notice that, in the ocaml interpreter, they are executed concurrently (no true parallelism in ocaml).

The type system basically ensures that no channel leaks, for instance the term:

```
#c. [#c'. c[c'].c' || c, x > x]
```

is rejected. Indeed, if it was accepted, then it could reduce to:

```
#c. [#c'. c' || c']
```

but the channel `c'` is not bound in the second branch of the parallel execution.

## Contribution

This project is in its very early stage, so I don't really expect any contribution. If you really want to contribute you can:
 - Test it, and report any unexpected behaviour (open an issue). If I also don't expect the behaviour, I'll fix it. If the behaviour is intended, I'll explain it.
 - Propose improvements (also open an issue). If it makes sense, I'll add it to the roadmap.

## Roadmap

See `objectives.md` for the roadmap.
