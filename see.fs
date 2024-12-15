: END&START ( endof-word start-of-word )
    PARSE-NAME SEARCH-WORD ( xt f -1 | xt f 1 )
    ?DUP 0= IF -13 THROW THEN
    ROT XT>LINK
    HERE LATEST
    BEGIN
        2 PICK OVER <>
    WHILE
        NIP DUP @
    REPEAT
    DROP 4 - SWAP ;

: MC? ( start-of-word -- flag )
    >CFA DUP @      ( cfa code-pointer )
    SWAP CELL+ =    ( does pointer pointer to next cell? )
;

: SEE
    PARSE-NAME SEARCH-WORD ( c-addr u 0 | xt f -1 | xt f 1 )
    ?DUP 0= IF ERRWORD 2! -13 THROW THEN
    ROT         ( f -1 xt )
    XT>LINK     ( f -1 link )
    HERE LATEST ( f -1 link here latest ) ( start with end of last word and start of last word in dictionary )
    BEGIN
        2 PICK OVER     ( f -1 link here latest link latest )
         <>             ( f -1 link here latest f2 )
    WHILE
        NIP DUP @       ( f -1 link latest prev )
    REPEAT
    DROP 4 - SWAP       ( f -1 end-of-word start-of-word )
     DUP                 
     LINK>NAME           ( f -1 end-of-word start-of-word nfa )
    [CHAR] : EMIT SPACE
    .NAME DROP
    DUP MC? DROP
    LINK>NAME NAME>BODY
    BEGIN
         2DUP >
     WHILE
        DUP @ ( f -1 pfa cfa2 )
         CASE  
            ['] LIT OF
                CELL+ DUP @ . ( Numeric literal )
            ENDOF
            ['] LITS OF
                [CHAR] S EMIT [CHAR] " EMIT SPACE
                CELL+ DUP @  ( Get length word )
                SWAP CELL+ SWAP ( Get string address and set up for type )
                2DUP TYPE
                [CHAR] " EMIT SPACE
                + ALIGNED       ( Skip over string )
                4 -             ( Because of CELL+ later )
            ENDOF
            ['] 0BRANCH OF
                ." 0BRANCH ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
            ['] BRANCH OF
                ." BRANCH ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
            ['] EXIT OF
                ." EXIT "
            ENDOF
         XT>LINK LINK>NAME .NAME DROP ( f -1 pfa )
         DUP
       ENDCASE
       CELL+
    REPEAT
     2DROP
     [CHAR] ; EMIT 
     1 =
     IF ."  IMMEDIATE " THEN
     -1 =
     IF ."  COMPILE-ONLY " THEN 
     CR
 ;
    
