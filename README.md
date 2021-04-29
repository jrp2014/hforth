# hforth

## Overview

A bare bones Forth-like interpreter framework in Haskell
based on `hsc3-forth` from: http://rohandrape.net/t/hsc3-forth

There is a library, `HForth` that provides a data and control stack as well as a
number of basic primitives. The data stack is generic. so you can adapt it to
the base type of your choice. (The example apps use `HForth` with Rationals.)
As well as the generic data stack type, Strings can be stored on the data stack.
There is no access to raw addresses.

## Language features

`HForth` has

- some execution control primitives
  - ':' and ';' define words
  - `IF` `ELSE` `THEN`
  - `DO` `LOOP` `I` `J`
  - `EXIT` `?EXIT` exit (conditionally) the excution of the current word
  - `<R` `R>` call and return
  - `EMIT` `KEY` `TYPE` perform IO
  - `.S` prints the data stack
  - `'` pops the top of the stack and prints it
  - `FORK` `KILL` `KILLALL` allow a word to run in parallel, and be terminated
  - `PAUSE` pauses the current thread
  - `S"` pushes a string onto the stack.
  - `'` puts the next word onto the stack and `EXECUTE` executes it
  - `INCLUDED` reads code from a file
  - `BYE` quits
  - `VMSTAT` and `TRACE` provide debugging information
- local words (enclosed in `{ }` can be defined in a word definition
- `DROP` `DUP` `OVER` `PICK` `ROT` `SWAP` `2DUP` `0<` `-` are stack primitives
- `RECURSIVE` allows / forbids recursive name definitions depending on whether
  the top of the stack is true or false

## Tutorial

There is a [tutorial](fs/tutorial.fs) file. The [fs](fs) folder contains some further examples.

## Example usage

```console
> cabal run h-forth -- --help
Up to date
H-FORTH

Usage: h-forth [-r|--recursive] [-t|--tracing INT] [FORTH SOURCE FILENAMES...]
  A rudimentary Forth repl written in Haskell

Available options:
  -r,--recursive           Allow recursive word definitions
  -t,--tracing INT         tracing level (-1 to 3) (default: 1)
  -h,--help                Show this help text
```

`rat-forth` provides an example of how to define some further basic (arithmetic)
primitive words.

To run the tutorial, you need a couple of files defining additional words.

```sh
> cabal run rat-forth  --  fs/stdlib.fs  fs/ratlib.fs fs/tutorial.fs
```

A further example is

```sh
> cabal run rat-forth -- fs/preForth-rts.pre fs/preForth-i386-backend.pre fs/preForth.pre
```
