: nip ( p q -- q ) swap drop ;
: tuck ( p q -- q p q ) swap over ;

: 1+ 1 + ;
: 1- 1 - ;
: 0= 0 = ;

: true 1 ; \ In ANS Forth true is -1.  In SC3 true is 1.
: false 0 ;

: bl 32 ;
: space bl emit ;
: spaces 0 do space loop ;
: cr 10 emit ;
