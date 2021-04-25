: negate 0 swap - ;
: abs dup 0 < if negate then ;
: min 2dup < if drop else nip then ;
: max 2dup > if drop else nip then ;
: recip 1 swap / ;
: inf 900000000 ; \ Pseudo infinity
