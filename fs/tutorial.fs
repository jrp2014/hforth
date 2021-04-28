( hsc3 forth -- or American Primitive, Vol. 1 )

\ hsc3-forth is a simple forth interpreter.

\ There is one data type, the SUPERCOLLIDER UNIT GENERATOR, a data stack, and a return stack.

\ The primitive words are:
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
\   PAUSE
\   VMSTAT
\   TRACE ( level -- )

( Help Forth )

\ Many words have more or less the same meaning as in ANS.
\ In these cases DPANS'94 numbers are given for reference.
\ These can be resolved as http://forth.sf.net/std/dpans/dpans6.htm#6.1.2165

( Discarding Forth )

\ There are two comment forms, ( comments until ) and \ comments until \n.

\ ( = 6.1.0080
\ \ = 6.2.2535

( Number Forth )

\ Arithmetic operations (from stdlib.hs and ratlib.hs) at constants render constants.

2 2 + . \ 4 \ + = 6.1.0120 \
2 1 - . \ 1 \ - = 6.1.0160 \
3 4 + 5 * . \ 35 \ * = 6.1.0090 \
3 4 5 * + . \ 23 \
2 negate . \ -2 \ NEGATE = 6.1.1910 \
-1 abs . \ 1 \ ABS = 6.1.0690 \

( Fractional Forth )

5 2 / . \ 5/2 \
20/15 . \ 4/3 \

( Integral Forth )

\ The printer prints integer constants without a fractional part, but .

10 2 div . \ 5 \ / = 6.1.0230 \
5 2 div . \ 2 \
7 3 mod . \ 1 \

( Eq Forth )

\ h-forth adopts 0 as False and anything else as True.

0 1 = . \ 0 \ = = 6.1.0530 FALSE = 6.2.1485 \
1 1 = . \ -1 \ TRUE = 6.2.2298 \

( Ord Forth )

\ The comparison operators.

1 2 < . \ TRUE \ < = 6.1.0480 \
2 1 < . \ FALSE \
1 1 < . \ FALSE \
1 1 <= . \ TRUE \
2 3 min . \ 2 \ MIN = 6.1.1880 \
3 2 min . \ 2 \
1 3 max . \ 3 \ MAX = 6.1.1870 \

( Stack Forth )

1 2 DROP . \ 1 \ DROP = 6.1.1260 \
1 2 .S OVER .S DROP DROP DROP .S \ <2> 1 2 <3> 1 2 1 <0> \ OVER = 6.1.1990 .S = 15.6.1.0220 \
1 2 .S SWAP .S DROP DROP .S \ <2> 1 2 <2> 2 1 <0> \ SWAP = 6.1.2260 \
1 2 3 .S ROT .S DROP DROP DROP .S \ <3> 1 2 3 <3> 2 3 1 <0> \ ROT = 6.1.2160 \
1 2 .S NIP .S DROP .S \ <2> 1 2 <1> 2 <0> \ NIP = 6.2.1930 \
1 2 .S TUCK .S DROP DROP DROP .S \ <2> 1 2 <3> 2 1 2 <0> \ TUCK = 6.2.2300 \
1 2 2DUP .S . . . . .S \ <4> 1 2 1 2 2 1 2 1 <0> \ 2DUP = 6.1.0380 \
1 2 3 4 5 2 PICK .S . . . . . . .S \ <6> 1 2 3 4 5 3 3 5 4 3 2 1 <0> \ PICK = 6.2.2030 \

( Block Forth )

\ COLON and SEMICOLON introduce new words.

: squared dup * ; \ : = 6.1.0450, ; = 6.1.0460 \
5 squared . \ 25 \
7 squared . \ 49 \

: cubed dup squared * ;
-5 cubed . \ -125 \

: fourth-power squared squared ;
3 fourth-power . \ 81 \

( Conditional Forth )

: _ 0 if S" #T" type else S" #F" type then ; \ IF = 6.1.1700, ELSE = 6.1.1310, THEN = 6.1.2270 \
_ \ #F \

 ( Do Forth )

: FIVE 5 0 DO 5 LOOP ; \ DO = 6.1.1240, LOOP = 6.1.1800 \
FIVE .S . . . . . \ <5> 5 5 5 5 5 5 5 5 5 5 \

: N-DUP 0 DO DUP LOOP ;
: N-DROP 0 DO DROP LOOP ;
5 4 N-DUP .S \ <5> 5 5 5 5 5 \
5 N-DROP .S \ <0> \

\ I fetches the loop counter

: SEVEN-ELEVEN 11 7 DO I . LOOP ; \ I = 6.1.1680 \
SEVEN-ELEVEN \ 7 8 9 10 \

: MTABLE 11 1 DO DUP I * . LOOP DROP ;
5 MTABLE \ 5 10 15 20 25 30 35 40 45 50 \

\ J fetches the outer loop counter

: TBL 3 1 DO 12 10 DO I J / . LOOP LOOP ; \ J = 6.1.1730 \
TBL \ 10 11 5 11/2 \

( Printing Forth )

\ EMIT prints a character.

: STAR 42 EMIT ; STAR STAR STAR \ *** \ EMIT = 6.1.1320 \
: STARS 0 DO STAR LOOP ; 10 STARS \ ********** \
: F 5 0 DO CR LOOP ; F \ \N\N\N\N\N \
: BOX 0 DO CR DUP STARS LOOP DROP ; 3 3 BOX \ \N***\N***\N*** \
: \STARS 0 DO CR I SPACES 10 STARS LOOP ; 3 \STARS

\ TYPE prints a string

S" STRING" TYPE \ STRING \ TYPE = 6.1.2310 \
: _ S" STRING" TYPE ; _ \ STRING

( Local Forth )

\ h-forth allows LOCAL words using the { NAME ... } syntax.

: SWAP' { A B } B A ; \ (LOCAL) = 13.6.1.0086 \
1 2 SWAP' . . \ 1 2 \

: PATTERN { A B C } A B C B C B A ;
1 2 3 PATTERN . . . . . . . \ 1 2 3 2 3 2 1 \

\ Multiple sets of LOCAL words are allowed.

: G { A } 2 { B } A B A ;
1 G . . . \ 1 2 1 \

( Interrupting Forth )

\ SIGINT is caught and the next VM operation will raise an error.

1 trace 
: ENDLESS INF 0 DO S" MSG: " TYPE I . CR 1/10 PAUSE LOOP ;

\ To send SIGINT from EMACS type C-cC-i

( Pausing Forth )

\ PAUSE suspends the thread of the current VM.

\ PAUSE doesn't re-instate interrupts

5 PAUSE \ NON-INTERRUPTIBLE
.S

( Fork Forth )

\ The VM can be FORKed, and the fork can be KILLed

: ENDLESS INF 0 DO S" MSG" TYPE CR 1 PAUSE LOOP ;
FORK ENDLESS .S
KILL .S

\ The forked word can read from the stack, but it does so in the new thread.

: N-MESSAGES 0 DO I . CR DUP PAUSE LOOP ;
1/2 10 FORK N-MESSAGES .S \ <3> 1/2 10 THREAD-ID

\ Here the interval and the count remain on the stack, along with the THREAD-ID.

KILL . . .S \ 10 1/2 <0>

\ The VM keeps a list of all running threads, and they can call be killed together (C-cC-s)

KILLALL

( Inclusive Forth )

\ s" /home/rohan/sw/hsc3-graphs/gr/jmcc/jmcc-why-supercollider.fs" INCLUDED

\ If the file is a process we can FORK INCLUDED, with the normal FORK stack rules.

\ s" /home/rohan/sw/hsc3-graphs/gr/jmcc/jmcc-alien-meadow.fs" FORK INCLUDED .S
\ KILL . . \ 45 STRING:"/home/rohan/sw/hsc3-graphs/gr/jmcc-alien-meadow.fs"

( Quoting Forth )

\ ' puts the EXECUTION TOKEN (XT) of the subsequent word onto the stack.
\ EXECUTE takes the token and applies it.

' + . \ XT:+ \ ' = 6.1.0070 \
' + 1 2 ROT EXECUTE . \ 3 \

( Return Forth )

1 >r .s r> . \ <0> 1
\ >r \ ERROR

( Labeled Forth )

\ s" LABEL" LABEL . \ "LABEL"

( Fibonacci Forth )

: FIB 0 1 ROT 0 DO OVER + SWAP LOOP DROP ;
: FIBS 0 DO I FIB . LOOP ;
50 FIBS \ 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
.S

( Trouble Forth )

VMSTAT \ PRINT VM STATUS
2 TRACE \ SET TRACE LEVEL PRIORITY, 2=HIGH, 1=MEDIUM, 0=LOW (DEFAULT=-1, NO TRACING)

( Rat Forth )

\ RAT-FORTH uses the same Forth interpreter as H-FORTH.
\ RAT-FORTH knows only rational numbers.

5 2 / . \ 5/2 \
5 2 DIV . \ 2 \
5 2 MOD . \ 1 \
5 2 DIV-MOD . . \ 2 1 \
1/10 .  \ 1/10 \
0.1 . \ 3602879701896397/36028797018963968 \

( ANS FORTH )

\ ANS FORTH is something else altogther.
\ HSC3 FORTH uses ANS FORTH names where it makes sense.
\ ANS FORTH requires floating point literals be written 1.1e0 etc.
\ ANS FORTH has a separate floating point stack, printed using f.

: F. . ;
1.1E0 2.2E0 3.3E0 F. F. F. \ 3.3 2.2 1.1 \

( Finishing Forth )

\ BYE \ C-cC-q
