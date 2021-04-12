\ OPERATOR ALIASES

: = == ;
: div IDiv ;
: not 0= ; \ NOT is not in ANS FORTH, see DPAN for discussion

: negate Neg ; \ stdlib REDEFINE

\ CONSTANTS

: inf 9e8 ; \ Pseudo infinity
: epsilon 1e-8 ; \ Psuedo epsilon (instead of zero in EXP contexts)
: pi 3.141592653589793 ;

\ MATH

: exp2 2 swap ** ;

\ ABBREVIATIONS

: mce2 2 mce ;
: mrg2 2 mrg ;
: clone2 2 clone ;
: play 0 swap Out -1 add-to-head 1 play-at ;
: pp false pretty-print ;

\ ENVELOPE CURVE ENUMERATION

: env-step 0 ;
: env-lin 1 ;
: env-exp 2 ;
: env-sin 3 ;
: env-welch 4 ;
: env-num 5 ;
: env-sqr 6 ;
: env-cub 7 ;
: env-hold 8 ;

\ DONE ACTION ENUMERATION

: do-nothing 0 ;
: pause-synth 1 ;
: remove-synth 2 ;
: remove-group 14 ;

\ ADD ACTION ENUMERATION

: add-to-head 0 ;
: add-to-tail 1 ;
: add-before 2 ;
: add-after 3 ;
: add-replace 4 ;

\ GEN FLAG ENUMERATION

: gen-normalize 1 ;
: gen-wavetable 2 ;
: gen-clear 4 ;

\ LOOP ENUMERATION

: no-loop 0 ;
: with-loop 1 ;

\ WARP ENUMERATION

: linear 0 ;
: exponential 1 ;

\ ENVELOPE NODES

: no-reset-node -99 ;
: no-loop-node -99 ;

\ ENVELOPE CONSTRUCTORS

: env-perc { atk rel } 0 2 -99 -99 1 atk 5 -4 0 rel 5 -4 12 mce ;
: env-linen { atk sus rel lev } 0 3 -99 -99 lev atk 1 0 lev sus 1 0 0 rel 1 0 16 mce ;
: env-tri { dur lvl } dur 2 / { dur' } 0 2 -99 -99 lvl dur' env-lin 0 0 dur' env-lin 0 12 mce ;
: env-asr { atk lvl rel c } 0 2 1 -99 lvl atk c 0 0 rel c 0 12 mce ;

\ COMPOSITE UGENS

: Choose { u } 0 u chan 1 - IRand.ir u Select ;
: LinLin_muladd { sl sr dl dr } dr dl - sr sl - / { m } m dl m sl * - ;
: LinLin LinLin_muladd MulAdd ;
: Rand2.ir { n } n Neg n Rand.ir ;
: SoundIn { u } NumOutputBuses.ir u + 1 In.ar ;
: TChoose { t a } 0 a chan t TIRand a Select ;

\ LOCAL BUFFERS

: as-local-buf { arr }
    arr chan { len }
    1 len LocalBuf.ir { buf }
    buf 0 len arr SetBuf.ir { set }
    buf set 2 mrg ;

\ COMMANDS

: b_allocRead s" ,isii" s" /b_allocRead" async ;

\ COLLECTION

: series { n z k } n 0 do k i * z + loop n mce ;
: geom { n z k } n 0 do k i ** z * loop n mce ;
