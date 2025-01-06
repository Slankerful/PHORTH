\ dumps the contents of memory for a length of len 
\ starting at c-addr 
BASE @
DECIMAL
: DUMP ( c-addr len -- )
    8 /MOD          \ Aim to dump in lines of at most 8 chars
    SWAP -ROT       \ Get remainder address and number of iterations on stack 
    0 ?DO            \ start the main loop and bring the memory address to top of stack
        DUP ." <" S>D (D.) TYPE ." >"  SPACE     \ print address at start of line
        8 0 DO      \ format in lines of 8
            DUP     \ dup address
            C@ 9 .R        \ print the value
            CHAR+       \ increment address by 1 char
        LOOP
        CR
    LOOP
    SWAP            \ get remainder to top of stack
    ?DUP 0<>         \ is remainder not 0?
    IF
       SWAP DUP ." <" S>D (D.) TYPE ." >"  SPACE \ print out address
       SWAP
       0 DO
        DUP
        C@ 9 .R
        CHAR+
       LOOP
       CR
    THEN
    DROP
;

BASE !

