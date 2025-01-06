: MC? ( start-of-word -- flag )
    >CFA DUP @      ( cfa code-pointer )
    SWAP CELL+ =    ( does pointer pointer to next cell? )
;

: SEE-NAME 
    XT>LINK LINK>NAME
    .NAME DROP
;

: SEE
    PARSE-NAME SEARCH-WORD ( c-addr u 0 | xt f -1 | xt f 1 )
    ?DUP 0= IF ERRWORD 2! -13 THROW THEN
    ROT         ( f -1 xt )
    DUP 
    @ VAR_DODOES =
    IF ." VARIABLE " SEE-NAME 2DROP EXIT THEN
    DUP @ 2VAR_DODOES =
    IF ." 2VARIABLE " SEE-NAME 2DROP EXIT THEN
    DUP @ CONST_DODOES =
    IF ." CONSTANT " SEE-NAME 2DROP EXIT THEN
    DUP @ 2CONST_DODOES =
    IF ." 2CONSTANT " SEE-NAME 2DROP EXIT THEN
    [CHAR] : EMIT SPACE
    DUP
    SEE-NAME
    CELL+
    DUP
    BEGIN
         @    
         DUP ['] EXIT <>
    WHILE
        DUP 
         CASE  
            ['] LIT OF
                DROP CELL+ DUP @ . ( Numeric literal )
            ENDOF
            ['] LITS OF
                DROP
                [CHAR] S EMIT [CHAR] " EMIT SPACE
                CELL+ DUP @  ( Get length word )
                SWAP CELL+ SWAP ( Get string address and set up for type )
                2DUP TYPE
                [CHAR] " EMIT SPACE
                + ALIGNED       ( Skip over string )
                4 -             ( Because of CELL+ later )
            ENDOF
            ['] 0BRANCH OF
                DROP
                ." 0BRANCH ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
            ['] BRANCH OF
                DROP
                ." BRANCH ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
            ['] EXIT2 OF
                DROP
                ." EXIT "
            ENDOF
            ['] (DO) OF
                DROP
                ." DO "
            ENDOF
            ['] (LOOP) OF
                DROP
                ." LOOP ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
                ['] (+LOOP) OF
                DROP
                ." +LOOP ( "
                CELL+ DUP @
                .
                [CHAR] ) EMIT SPACE
            ENDOF
                ['] (DOES>) OF
                DROP
                ." DOES> "
                CELL+
            ENDOF
         XT>LINK LINK>NAME .NAME DROP ( f -1 pfa )
       ENDCASE
       CELL+
        DUP
    REPEAT
     2DROP
     [CHAR] ; EMIT 
     1 =
     IF ."  IMMEDIATE " THEN
     -1 =
     IF ."  COMPILE-ONLY " THEN 
     CR
 ;
    
