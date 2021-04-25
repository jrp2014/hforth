# hforth

A Forth-like interpreter framework in Haskell
based on `hsc3-forth` from: http://rohandrape.net/t/hsc3-forth

Two examples are provided:
* `h-forth` TODO
* `rat-forth` uses `Rational` stack data

## Usage

```sh
> cabal run  h-forth -- --help
Up to date
H-FORTH

Usage: h-forth [-r|--recursive] [-t|--tracing INT] [FORTH SOURCE FILENAMES...]
  A rudimentary Forth repl written in Haskell

Available options:
  -r,--recursive           Allow recursive word definitions
  -t,--tracing INT         tracing level (-1 to 3) (default: 1)
  -h,--help                Show this help text
```

## Language features

There is a data stack and a return stack.

The data stack can store a type that you can specify (eg, Int, Rational, Float) and `String`

Primitive words are:

```Forth
\
\   : ;
\   IF ELSE THEN
\   DO I J LOOP
\   { } (LOCAL)
\   ' EXECUTE
\   S"
\   EXIT ?EXIT
\   FORK KILL KILLALL
\   BYE
\   ( ) \
\
\   DUP SWAP DROP
\   OVER ROT 2DUP
\   0< -
\
\   EMIT . .S KEY TYPE
\   INCLUDED ( S -- )
\
\   <R R>
\
\   RECURSIVE ( flag -- )
\   VMSTAT
\   TRACE ( level -- )
```

Further words can be included based on these. For example, using 'Rational' as
a data stack base type, words from `preForth` can be imported by

```sh
> cabal run rat-forth -- fs/preForth-rts.pre fs/preForth-i386-backend.pre fs/preForth.pre
RAT-FORTH
LOAD-FILES: fs/preForth-rts.pre,fs/preForth-i386-backend.pre,fs/preForth.pre
INCLUDED: fs/preForth-rts.pre
DEFINE: ?dup
DEFINE: 0=
DEFINE: negate
DEFINE: +
DEFINE: 1+
DEFINE: 1-
DEFINE: =
DEFINE: <
DEFINE: >
DEFINE: over
DEFINE: rot
DEFINE: nip
DEFINE: 2drop
DEFINE: pick
DEFINE: roll
DEFINE: case?
DEFINE: bl
DEFINE: space
DEFINE: tab
DEFINE: cr
DEFINE: (/mod
DEFINE: 10*
DEFINE: (10u/mod
DEFINE: 10u/mod
DEFINE: (u.
DEFINE: u.
DEFINE: (.
DEFINE: dec.
DEFINE: show
DEFINE: tail
DEFINE: (_dup
DEFINE: _dup
DEFINE: _drop
DEFINE: (_swap
DEFINE: _swap
 OK
INCLUDED: fs/preForth-i386-backend.pre
DEFINE: replace
DEFINE: alter
DEFINE: ."dd"
DEFINE: >"dd"
DEFINE: ."db"
DEFINE: >"db"
DEFINE: >"ds"
DEFINE: ."nest"
DEFINE: ."unnest"
DEFINE: ."lit"
DEFINE: ,string
DEFINE: ,line
DEFINE: ,word
DEFINE: ,>word
DEFINE: ,nest
DEFINE: ,unnest
DEFINE: ,n
DEFINE: ,u
DEFINE: ,_lit
DEFINE: ,lit
DEFINE: ,comment
DEFINE: label
DEFINE: body
DEFINE: ,codefield
DEFINE: ,code
DEFINE: ,end-code
DEFINE: bodylabel
DEFINE: ,tail
DEFINE: ."done"
DEFINE: ."last:"
DEFINE: ,end
DEFINE: header
 OK
INCLUDED: fs/preForth.pre
DEFINE: skip
DEFINE: scan
DEFINE: (line
DEFINE: line
DEFINE: token
DEFINE: ?;
DEFINE: ?;<
DEFINE: pre
DEFINE: code
DEFINE: ?'x'
DEFINE: ?digit
DEFINE: ?'-'
DEFINE: ((?#
DEFINE: (?#
DEFINE: ?-#
DEFINE: ?+-#
DEFINE: ?#
DEFINE: ?lit
DEFINE: ?\
DEFINE: ?tail
DEFINE: ?word
DEFINE: ]
DEFINE: (:
DEFINE: :'
DEFINE: ?:
DEFINE: ?code
DEFINE: ?pre
DEFINE: quit
DEFINE: cold
 OK
 OK
-27 dec.
-27 OK

```

Not supported are

- direct memory access
