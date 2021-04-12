( hsc3 forth -- or American Primitive, Vol. 1 )

\ hsc3-forth is a simple forth interpreter.

\ There is one data type, the SUPERCOLLIDER UNIT GENERATOR, a data stack, and a return stack.

\ The primitive words are:
\   : ;
\   IF ELSE THEN
\   DO I J LOOP
\   { } (LOCAL)
\   ' EXECUTE

( Emacs Forth )

\ hsc3-forth is an interpreter, you can use it directly or you can use emacs using hsc3-forth-mode.

\ Commands are listed below and are C-cC- prefixed.  <point> is the word at the cursor.

\   > -- Start hsc3-forth if not running, show interpreter frame
\   c -- Send current line
\   d -- Send current region
\   g -- Send DRAW
\   e -- Send PP
\   a -- Send PLAY
\   u -- Send <point> ?
\   j -- Send <point> MANUAL
\   k -- Send STOP
\   s -- Send KILLALL
\   q -- Send BYE
\   i -- Send !SIGINT!
\   p -- Send SC3-STATUS

( Help Forth )

\ Many words have more or less the same meaning as in ANS.
\ In these cases DPANS'94 numbers are given for reference.
\ These can be resolved as http://forth.sf.net/std/dpans/dpans6.htm#6.1.2165

\ ? prints information about a UGEN, in EMACS type C-cC-u.

S" sinosc" ? \ S" = 6.1.2165 \

\ SinOsc [KR,AR] freq=440.0 phase=0.0

S" EnvGen" ?

\ EnvGen [KR,AR] gate=1 levelScale=1 levelBias=0 timeScale=1 doneAction=0 *envelope=0
\     MCE, REORDERS INPUTS: [5,0,1,2,3,4,5], ENUMERATION INPUTS: 4=DoneAction, 5=Envelope UGen

\ MANUAL opens the SuperCollider manual at the required page, in emacs type C-cC-j.

\ DPANS opens the DPANS glossary at the indicated entry, in emacs type C-cC-f

( Discarding Forth )

\ There are two comment forms, ( comments until ) and \ comments until \n.

\ ( = 6.1.0080
\ \ = 6.2.2535

( Number Forth )

\ SC3 UGens are numerical.
\ UGens may be constant, and math operations at constants render constants.

2 2 + . \ 4 \ + = 6.1.0120 \
2 1 - . \ 1 \ - = 6.1.0160 \
3 4 + 5 * . \ 35 \ * = 6.1.0090 \
3 4 5 * + . \ 23 \
2 negate . \ -2 \ NEGATE = 6.1.1910 \
-1 abs . \ 1 \ ABS = 6.1.0690 \

( Fractional Forth )

\ UGENS are real valued, / is real valued division, and % real valued modulo.

1.1 2.2 3.3 . . . \ 3.3 2.2 1.1 \ . = 6.1.0180 \
5 2 / . \ 2.5 \ F/ = 12.6.1.1430 \
7.5 3.75 % . \ 0 \

( Integral Forth )

\ The printer prints integer constants without a fractional part.
\ There is an integer division UGen.

10 2 div . \ 5 \ / = 6.1.0230 \
5 2 div . \ 2 \
7 3 % . \ 1 \

( Eq Forth )

\ SC3 treats signals less than or equal to zero as False and greater than zero as True.
\ hsc3-forth adopts 1 as True.

0 1 = . \ FALSE \ = = 6.1.0530 FALSE = 6.2.1485 \
1 1 = . \ TRUE \ TRUE = 6.2.2298 \

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
TBL \ 10 11 5 5.5 \

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

\ hsc3-forth allows LOCAL words using the { NAME ... } syntax.

: SWAP' { A B } B A ; \ (LOCAL) = 13.6.1.0086 \
1 2 SWAP' . . \ 1 2 \

: PATTERN { A B C } A B C B C B A ;
1 2 3 PATTERN . . . . . . . \ 1 2 3 2 3 2 1 \

\ Multiple sets of LOCAL words are allowed.

: F { A } 2 { B } A B A ;
1 F . . . \ 1 2 1 \

( Ugen Forth )

440 0 SINOSC.AR 0.1 * 0 SWAP OUT  -1 ADD-TO-HEAD 1 PLAY-AT
440 441 2 MCE 0 SINOSC.AR 0.1 * PLAY
440 441 2 MCE 0 SINOSC.AR 1 0 SINOSC.KR 0.1 * ABS * PLAY
WHITENOISE.AR 0.05 * PLAY

\ STOP frees all nodes at SCSYNTH (C-cC-k)

STOP

\ Filters may elide the operating rate.

WHITENOISE.AR HPZ1.AR 0.1 * .
WHITENOISE.AR HPZ1 0.1 * .

\ Oscillators may not.

440 0 SINOSC 0.1 * . \ ERROR

\ Operators have both text and symbolic names.

440 0 SINOSC.AR WHITENOISE.AR ADD -45 DBAMP MUL PLAY \ QUIETLY NOW

( Insensitive Forth )

440 0 SINOSC.AR 0.1 MUL PLAY \ CASE INSENSITIVE
441 0 sinosc.ar 0.1 mul play \ case insensitive
442 0 SinOsc.ar 0.1 Mul Play \ Case Insensitive

( Pretty Forth )

\ pretty-print prints an hsc3-forth representation of the graph.
\ A flag tells whether to print the UId of each NON-DET UGen.
\ PP is an abreviation of FALSE PRETTY-PRINT (C-cC-e).

: G 440 0 SINOSC.AR 1 0 SINOSC.KR 0.1 * * WHITENOISE.AR 0.1 * + ;
G TRUE PRETTY-PRINT
G PP

\ PP is only moderately perspicuous, it does not see through MCE.

: G 440 441 2 MCE 0 SINOSC.AR 1 0 SINOSC.KR 0.1 * * WHITENOISE.AR 0.1 * + ;
G PP

( Drawing Forth )

\ DRAW draws a UGEN graph via the graphviz DOT language interpreter (C-cC-g)

WHITENOISE.AR 0.1 * DUP - DRAW \ SILENCE \
WHITENOISE.AR 0.1 * 2 CLONE UNMCE - DRAW \ NOISE \

( Naming Forth )

440 RAND_ 440 + 0 SINOSC.AR 0.1 * DRAW \ RAND_ IS A UNARY OPERATOR
440 880 RAND.IR 0 SINOSC.AR 0.1 * DRAW \ RAND.IR IS A UGEN

( Random Forth )

: RANDOM-SINE 1900 2300 RAND.IR 0 SINOSC.AR -1 1 RAND.IR 0.05 0.1 RAND.IR PAN2.AR ;
: _ 4 0 DO RANDOM-SINE PLAY LOOP ; _

STOP

\ NON-DET operators are not marked, the below has one Rand_ operator.

500 RAND_ 500 RAND_ - 0 SINOSC.AR 0.1 * DRAW

( Un-Random Forth )

\ The non-deterministic UGENS get identifiers from a counter, which can be set.

1376523 SET-UID

\ The UNRAND transformation lifts scalar random UGENS to constants.

0 1 RAND.IR 2 CLONE .S \ <1> [UGEN:RAND UGEN:RAND]
UNRAND . \ [0.6768026553207348 0.21705544209066452]

\ How un-random is it?  Not very.....  This requires attention.

: RND RAND.IR UNRAND ;
: _ 100 0 DO 0 1 RND . LOOP ; _

\ UNRAND is applied internally when a constant value is required, ie. for PAUSE.

: _ 20 0 DO 0 1 RAND.IR I . DUP DUP . UNRAND . CR PAUSE LOOP ; _

\ CHOOSE is a composite i-rate UGEN.

: RHARM 13 1 DO I 100 * LOOP 12 MCE CHOOSE 0 SINOSC.AR -1 1 RAND.IR 0.05 PAN2 ;
RHARM 1 3 9 INF OVERLAP-TEXTURE \ C-cC-i to interrupt

( Enveloped Forth )

: WITH-TRIANGLE-ENV { DUR LVL } 1 1 0 1 REMOVE-SYNTH DUR LVL ENV-TRI ENVGEN.KR * ;
WHITENOISE.AR 10 0.1 WITH-TRIANGLE-ENV PLAY
RANDOM-SINE 5 0.1 WITH-TRIANGLE-ENV PLAY
STOP

( Interrupting Forth )

\ SIGINT is caught and the next VM operation will raise an error.

: ENDLESS INF 0 DO S" MSG: " TYPE I . CR 0.1 PAUSE LOOP ;
ENDLESS

\ To send SIGINT from EMACS type C-cC-i

( Pausing Forth )

\ PAUSE suspends the thread of the current VM.

: ANON 11 1 DO I 4 / DUP . CR PAUSE RANDOM-SINE 5 0.1 WITH-TRIANGLE-ENV PLAY LOOP ;
ANON 5 PAUSE STOP

\ PAUSE doesn't re-instate interrupts

10 PAUSE \ NON-INTERRUPTIBLE
.S

( Schedule Forth )

\ Since RANDOM-SINE takes time, there is audible scattering.

: _ 25 0 DO RANDOM-SINE PLAY LOOP ; _
STOP

\ SCHED is a variant of PLAY that forward schedules, allowing for precise alignment.

: _ TIME 0.1 + { T } 25 0 DO RANDOM-SINE T SCHED LOOP ; _
STOP

( Textural Forth )

\ The SC2 TEXTURE functions are implemented, see OVERLAP-TEXTURE.FS.

RANDOM-SINE 2 3 5 XFADE-TEXTURE
RANDOM-SINE 2 3 6 12 OVERLAP-TEXTURE

( Fork Forth )

\ The VM can be FORKed, and the fork can be KILLed

: ENDLESS INF 0 DO S" MSG" TYPE CR 1 PAUSE LOOP ;
FORK ENDLESS .S
KILL .S

\ The forked word can read from the stack, but it does so in the new thread.

: N-MESSAGES 0 DO I . CR DUP PAUSE LOOP ;
0.5 10 FORK N-MESSAGES .S \ <3> 0.5 10 THREAD-ID

\ Here the interval and the count remain on the stack, along with the THREAD-ID.

KILL . . .S \ 10 0.5 <0>

\ The VM keeps a list of all running threads, and they can call be killed together (C-cC-s)

KILLALL

( Inclusive Forth )

s" /home/rohan/sw/hsc3-graphs/gr/jmcc/jmcc-why-supercollider.fs" INCLUDED

\ If the file is a process we can FORK INCLUDED, with the normal FORK stack rules.

s" /home/rohan/sw/hsc3-graphs/gr/jmcc/jmcc-alien-meadow.fs" FORK INCLUDED .S
KILL . . \ 45 STRING:"/home/rohan/sw/hsc3-graphs/gr/jmcc-alien-meadow.fs"

( Quoting Forth )

\ ' puts the EXECUTION TOKEN (XT) of the subsequent word onto the stack.
\ EXECUTE takes the token and applies it.

' + . \ XT:+ \ ' = 6.1.0070 \
' + 1 2 ROT EXECUTE . \ 3 \

\ Words can be somewhat higher order.

: SIG 1 50 20 REMOVE-SYNTH XLINE.KR 0 IMPULSE.AR 0.01 0.2 DECAY2 600 0 FSINOSC.AR 0.25 * * ;
: CMB 0.1 0.1 1 COMBN ;
: POST-PROC { F } 0 2 IN.AR F EXECUTE 0 SWAP OUT -1 ADD-TO-TAIL 1 PLAY-AT ;

SIG PLAY ' CMB POST-PROC
STOP

( Return Forth )

1 >r .s r> . \ <0> 1
>r \ ERROR

( Labeled Forth )

s" LABEL" LABEL . \ "LABEL"

( Fibonacci Forth )

: FIB 0 1 ROT 0 DO OVER + SWAP LOOP DROP ;
: FIBS 0 DO I FIB . LOOP ;
50 FIBS \ 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 ...
.S

( Dynamic Forth )

\ The UGen words are not all pre-defined but are created dynamically.
\ The dictionary initially contains no UGen words.
\ The dynamic lookup happens if the word is not in the dictionary.
\ This is why eventual lookup failure is reported as DYNAMIC FAILED.

NOT_A_UGEN.AR \ ERROR
MISTYPED-WORD \ ERROR

( Trouble Forth )

0 1 RAND.IR DUP . PAUSE \ RAND
440 0 SINOSC.AR PAUSE \ ERROR (NON-CONSTANT)

: _ WHITENOISE.AR 0 DO I . LOOP ; _ \ THIS _SHOULD_ BE A NON-CONSTANT ERROR

0 1 2 3 4 4 MCE OUT.KR PP \ THIS _SHOULD_ ADD THE MCE CONSTRUCTOR TO THE MCE INPUT

VMSTAT \ PRINT VM STATUS
2 TRACE \ SET TRACE LEVEL PRIORITY, 0=HIGH, 1=MEDIUM, 2=LOW (DEFAULT=-1, NO TRACING)

( Finishing Forth )

BYE \ C-cC-q

( Collecting Forth )

5 10 2 series . \ [10,12,14,16,18] \
5 3 6 geom . \ [3,18,108,648,3888] \

( Rat Forth )

\ RAT-FORTH uses the same Forth interpreter as HSC3-FORTH.
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
