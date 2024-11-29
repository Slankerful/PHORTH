vocabulary assembler
: init-asm also assembler ;
: deinit-asm previous ;

: code
    create
    here                \ Get address of pfa
    -1 cells allot      \ Step back onto cfa which CREATE created
    ,                   \ Compile the pfa address into the cfa
    init-asm            \ Add assembler to vocab order
;

hex                     \ Everything number from now on is in HEX

:   end-code
    ea000000            \ Branch opcode
    next                \ address of next routine
    here -              \ Calculate offset to _next
    8 -                 \ adjust for PC offset
    4 /                 \ Divide by 4 as branch address is word aligned
    00ffffff            \ Mask out opcode bits
    and or              \ combine the opcode and offset
    ,                   \ compile into word being defined by code
    deinit-asm
;

: under+ ( n1 n2 n3 -- n1+n3 n2 )
    rot         ( n1 n2 n3 -- n2 n3 n1 )
    +           ( n2 n3 n1 -- n2 n1+n3 )
    swap        ( n2 n1+n3 -- n1+n3 n2 )
;

also assembler definitions

: enumerate: ( N start "name1" ... "nameN" -- )
    dup under+ ?do i constant loop ;

6 70000000 enumerate: register shifted #immediate psr cxsf-mask offset
2 70000006 enumerate: multimode register-list

: regs: 10 0 do i register 2constant loop ;

regs: R0 R1 R2 R3 R4 R R6 R7 R8 R9 R10 R11 R12 R13 R14 R15

000000 psr 2constant CPSR
400000 psr 2constant SPSR

: nand ( x1 -- x2 )
    invert and
;

: ?register ( n -- )
    register <> abort" Invalid operand, need a register R0..R15"
;

: ?psr ( n -- )
    psr <> abort" Invalid operand, need speical register SPSR or CSPR"
;

\ Bit masks
: bit: ( n "name" -- )
    1 swap
    lshift
    constant
;
: bits ( 0 bit1 ... bitN -- x )
    0
    begin
        over
        or swap
        0=
    until
;

19 bit: %I      18 bit: %P      17 bit: %U      16 bit: %B      15 bit: %W

variable instruction
variable had-cc
: encode ( x1 -- )
    instruction @
    or
    instruction !
;

: ?can-cc ( -- )
    had-cc @ abort" Attempt to specify condition code twice."
;

: cc: ( x "name" -- )
    create ,
    does>
        @
        ?can-cc
        1c lshift encode
        true had-cc !
;

: ccs: 
    do
        i cc:
    loop
;

8  0 ccs: EQ    NE    CS    CC    MI    PL    VS    VC
4  2 ccs:             HS    LO
6  0 ccs: ?=    ?<>   u>=   u<    ?0<   ?0>=
10 8 ccs: HI    LS    GE    LT    GT    LE    AL    NV
0E 8 ccs: ?u>   ?u<=  ?>=   ?<    ?>    ?<=

: invert-cc
    had-cc @ 0= abort" No condition code specified for instruction"
    instruction @
    1 1c lshift xor
    instruction !
;

: <instruction ( x -- )
    dup 0F0000000 and
    if
        had-cc @ abort" Condition code not allowed for instruction"
    else
        had-cc @ 0= 
        if
            al
        then
    then
    encode
;

: instruction> ( -- x )
    instruction @
    0 instruction !
    false had-cc !
;

: register-operand: ( bit-offs "name" -- )
    create ,
    does>
        @
        >r
        ?register
        r> lshift
        encode
;

10 register-operand: Rn,        0C register-operand: Rd,
10 register-operand: RdHi,      0C register-operand: RdLo,
8  register-operand: Rs,         0 register-operand: Rm,

: psr,
    ?psr
    encode
;

: cxsf-mask: ( #bit "name" -- )
    1 swap
    lshift
    cxsf-mask 2constant
;

: cxsf, 
    begin
        dup cxsf-mask = 
    while
        drop
        encode
    repeat
;

10 cxsf-mask: C     11 cxsf-mask: X     12 cxsf-mask: S     13 cxsf-mask: F

\ Right-hand side operands
: lshift32  ( x1 n -- x2 )
    lshift
    0FFFFFFFF and
;
: lrotate32 ( x1 n -- x2 )
    2dup
    lshift32
    >r
    20 swap
    -
    rshift
    r> or
;

: # ( n -- x )
    16 0 
    ?do
        dup 0 100 within
        if
            I 8 lshift
            or
            %i
            or
            #immediate
            unloop
            exit
        then
        2 lrotate32
    loop
    abort" Immediate operand cannot be expressed as a shifted 8 bit value"
;

: ?shift ( x1 -- x1 )
    dup 1 20 within
    0= abort" Invalid shift value"
;

: #shift: ( mask "name" -- )
    create ,
    does>
        @
        >r ?shift
        7 lshift
        >r
        ?register
        r> or
        r> or
        shifted
;

: rshift: ( mask "name" -- )
    create ,
    does>
        >r ?register
        8 lshift
        >r
        ?register
        r> or
        r> or
        010 or
        shifted
;

: rrx ( n-reg 'register' -- operand 'shifted )
    ?register
    060 or
    shifted
;

000 dup #shift: #lsl    rshift: lsl         020 dup #shift: #lsr    rshift: lsr
040 dup #shift: #asr    rshift: asr         060 dup #shift: #ror    rshift: ror

: ?rhs ( 'shifted' | 'register' | '#immediate' )
    >r r@
    shifted
    <>  r@
    #immediate
    <> and
    r>
    register
    <> and
    abort" Need a (shifted) register or immediate value as operand"
;

: ?#shifted-register ( x 'shifted' | 'register' -- x )
    >r r@
    shifted
    <> r>
    register
    <> and
    abort" Need a (shifted) register here"
    dup 
    010 and
    abort" Shift by register not allowed here"
;

: rhs, ( x 'r-shifted' | '#-shifted' -- )
    ?rhs encode
;

: rhs', ( x 'r-shifted' | '#-shifted' -- )
    dup shifted = 
    abort" Shifted registers not allowed here"
    rhs,
;

\ Addressing modes
: offset: ( 0 bit1 ... bitN "name" -- )
    bits
    offset
    2constant
;

0 %P %I %U      offset: +]
0 %P %I         offset: -]
0 %P %I %U %W   offset: +]!
0 %P %I    %W   offset: -]!
0    %I %U      offset: ]+
0    %I         offset: ]-
0 %P            offset: #]
0 %P       %W   offset: #]!
0               offset: ]#

: ]
    0 #]
;

: [#] ( addr -- R15 offs 'offset' ) \ generate PC-relative address
    >R R15
    R>
    here 8 +
    -
    #]
;

: multimode: ( 0 bit1 ... bitN "name" -- )
    bits
    multimode
    2constant
;

0               multimode: DA
0    %U         multimode: IA
0 %P            multimode: DB
0 %P %U         multimode: IB
0       %W      multimode: DA!
0    %U %W      multimode: IA!
0 %P    %W      multimode: DB!
0 %P %U %W      multimode: IB!

: ?offset ( 'offset' -- )
    offset <>
    abort" Invalid operand, need an address offset e.g. ' Rn ] ' "
;

: ?multimode ( 'offset' -- )
    multimode <>
    abort" Need an address mode for load/store multiple: DA etc."
;

: ?upwards ( n1 -- n2 )
    dup 0<
    if
        negate
    else
        %U encode
    then
;

: ?post-offset ( x 'offset' -- x )
    ?offset
    dup %P
    and 0<>
    abort" Only post-indexed addressing, ]#, ]+ or ]-, allowed here"
;

: ?0#] ( 0 'offset' -- )
    ?offset
    0 #]
    drop d<>
    abort" Only addresses without offset, e.g. R0 ] allowed here"
;

: #offset12, ( n -- )
    ?upwards
    dup
    000 1000 within 0=
    abort" Offset out of range"
    encode
;

: #offset8, ( n -- )
    ?upwards
    dup
    000 1000 within 0=
    abort" Offset out of range"
    %B encode
    dup 
    0F and      \ low nibble
    encode
    0F0 and
    4 lshift    \ high nibble
    encode
;

: R#shifted-offset, ( n 'register' | 'shifted-reg' -- )
    ?#shifted-register
    encode
;

: R-offset, ( n 'shifted-reg' -- )
    ?register 
    encode
;

: offs12, ( x1..xn 'offset' -- )
    ?offset
    dup encode
    %I and 0=
    if
        #offset12,
    else
        R#shifted-offset,
    then
;

: offsP, ( x1..xn 'offset' -- )
    2dup
    ?post-offset
    drop
    offs12,
;

: offs8, ( x1..xn 'offset' -- )
    ?offset
    dup %I
    nand
    encode
    %I and 0=
    if
        #offset8,
    else
        R-offset,
    then
;

: mmode, ( x 'multimode' -- )
    ?multimode
    encode
;

\ Branch offsets
2 80000000
enumerate: forward backward

: ?branch-range ( offset -- offset )
    dup
    -2000000 2000000 
    within 0=
    abort" Branch destination out of range"
;

: ?branch-offset ( offset -- offset )
    ?branch-range
    dup 3 and 0<>
    abort" Branch destination not 4 byte-aligned"
;

: ?branchX-offset ( offset -- offset )
    ?branch-range
    dup 1 and 0<>
    abort" Thumb-mode branch destination is not 2 byte-aligned"
;

: branch-addr>offset ( src dest -- offset )
    swap
    8 +
    -
;

: branch-offset>bits ( offset -- x)     \ Offset of B, BL instruction
    ?branch-offset
    2 rshift
    0FFFFFF and
;

: branchX-offset>bits ( offset -- x )   \ Offset of BLX instruction
    ?branchX-offset
    dup
    2 rshift
    0FFFFFF and
    swap 2 and
    17 lshift
    or
;

: branch-addr, ( addr -- x)
    here swap
    branch-addr>offset
    branch-offset>bits
    encode
;

: branchX-addr, ( addr -- x)
    here
    swap
    branch-addr>offset
    branchX-offset>bits
    encode
;

: a<mark ( -- addr 'backward' )
    here backward
;

: a<resolve ( addr 'backward' -- addr )
    backward
    <> abort" Expect assembler backward reference on stack"
;

: a>mark ( -- addr 'forward' addr )
    here
    forward
    over
;

: a>resolve ( addr 'forward' -- )
    forward <>
    abort" Expect assembler forward reference on stack"
    dup here
    branch-addr>offset
    branch-offset>bits
    over
    @
    0FF000000 and
    or
    swap !
;

\ Comment fields (SVC/SWI)
: ?comment ( x -- x )
    dup
    0 01000000 within
    0= abort" Comment field is limited to 24 bit values"
;
: comment, ( x -- )
    ?comment
    encode
;

\ Register lists (for LDM and STM )
: { ( -- mark )
    77777777 ;
: } ( mark reg1 .. regN -- reglist )
    0
    begin
        over
        77777777 <>
    while
        swap
        ?register
        1 rot
        lshift or
    repeat
    nip
    register-list
;

: R-R ( reg1 regN -- reg1 reg2 .. regN )
    ?register
    swap
    ?register
    1+
    ?do
        I
        register
    loop
;

: ?register-list ( 'register-list' -- )
    register-list <>
    abort" Need a register list { .. } as operand"
;

: reg-list, ( x 'register-list' -- )
    ?register-list
    encode
;

\ Mnemonics
: instruction-class: ( xt "name" -- )
    create ,
    does> @ ( mask xt "name" -- )
        create , ,
        does> 2@ ( mask xt -- ) 
            >r
            <instruction
            r> execute
            instruction>
            ,
;

:noname rhs, Rn, Rd, ;              instruction-class: data-op:
:noname rhs, Rn, ;                  instruction-class: cmp-op:
:noname rhs, Rd, ;                  instruction-class: mov-op:
:noname psr, Rd, ;                  instruction-class: mrs-op:
:noname rhs', cxsf, psr, ;          instruction-class: msr-op:
:noname offs12, Rn, Rd, ;           instruction-class: mem-op:
:noname offsP, Rn, Rd, ;            instruction-class: memT-op:
:noname offs8, Rn, Rd, ;            instruction-class: memH-op:
:noname ?0#] Rn, Rm, Rd, ;          instruction-class: memS-op:
:noname reg-list, mmode, Rn, ;      instruction-class: mmem-op:
:noname branch-addr, ;              instruction-class: branch-op:
:noname Rs, Rm, Rn, ;               instruction-class: RRR-op:
:noname Rn, Rs, Rm, Rd, ;           instruction-class: RRRR-op:
:noname Rs, Rm, RdHi, RdLo, ;       instruction-class: RRQ-op:
:noname comment, ;                  instruction-class: comment-op:
:noname Rm, ;                       instruction-class: branchR-op:
:noname branchX-addr, ;             instruction-class: branchX-op:

00000000 data-op: AND,      00100000 data-op: ANDS,
00200000 data-op: EOR,      00300000 data-op: EORS,
00400000 data-op: SUB,      00500000 data-op: SUBS,
00600000 data-op: RSB,      00700000 data-op: RSBS,
00800000 data-op: ADD,      00900000 data-op: ADDS,
00A00000 data-op: ADC,      00B00000 data-op: ADCS,
00C00000 data-op: SBC,      00D00000 data-op: SBCS,
00E00000 data-op: RSC,      00F00000 data-op: RSCS,
01100000 cmp-op:  TST,      0110F000 cmp-op:  TSTP,
01300000 cmp-op:  TEQ,      0130F000 cmp-op:  TEQP,
01500000 cmp-op:  CMP,      0150F000 cmp-op:  CMPP,
01700000 cmp-op:  CMN,      0170F000 cmp-op:  CMNP,
01800000 data-op: ORR,      01900000 data-op: ORRS,
01A00000 mov-op:  MOV,      01B00000 mov-op:  MOVS,
01C00000 data-op: BIC,      01D00000 data-op: BICS,
01E00000 mov-op:  MVN,      01F00000 mov-op:  MVNS,

04000000 mem-op:  STR,      04100000 mem-op:  LDR,
04400000 mem-op:  STRB,     04500000 mem-op:  LDRB,
04200000 memT-op: STRT,     04300000 memT-op: LDRT,
04600000 memT-op: STRBT,    04700000 memT-op: LDRBT,
000000B0 memH-op: STRH,     001000B0 memH-op: LDRH,
001000F0 memH-op: LDRSH,    000000D0 memH-op: LDRSB,
01000090 memS-op: SWP,      01400090 memS-op: SWPB,

08000000 mmem-op: STM,      08100000 mmem-op: LDM,
08400000 mmem-op: ^STM,     08500000 mmem-op: ^LDM,

010F0000 mrs-op:  MRS,
0120F000 msr-op:  MSR,

0A000000 branch-op:  B,     0B000000 branch-op:  BL,
012FFF10 branchR-op: BX,
012FFF30 branchR-op: BLX,   FA000000 branchX-op: #BLX,
0F000000 comment-op: SWI,   0F000000 comment-op: SVC,

00000090 RRR-op:  MUL,      00100090 RRR-op:  MULS,
00200090 RRRR-op: MLA,      00300090 RRRR-op: MLAS,
00800090 RRQ-op:  UMULL,    00900090 RRQ-op:  UMULLS,
00A00090 RRQ-op:  UMLAL,    00B00090 RRQ-op:  UMLALS,
00C00090 RRQ-op:  SMULL,    00D00090 RRQ-op:  SMULLS,
00E00090 RRQ-op:  SMLAL,    00F00090 RRQ-op:  SMLALS,

\ Labels and branch resolving
: label here constant ;
: if-not, a>mark B, ;
: if, invert-cc if-not, ;
: ahead, al if-not, ;
: then, a>resolve ;
: else, a>mark AL B, 2swap then, ;
: begin, a<mark ;
: until-not, a<resolve B, ;
: until, invert-cc until-not, ;
: again, AL until-not, ;
: while-not, if-not, ;
: while, invert-cc while-not, ;
: repeat, 2swap again, then, ;
: repeat-until-not, 2swap until-not, then, ;
: repeat-until, invert-cc repeat-until-not, ;

R15 2constant PC
R14 2constant LR
R13 2constant SP
R12 2constant IP
R11 2constant FP

: noop ;
: next, \ Do 32-bit branch to NOOP
    PC PC -4
    #]
    LDR,
    ['] NOOP
    @
    ,
;

previous definitions decimal

\ Example
\ code dup
\    r0 sp ] ldr,
\    r0 sp -4 #]! str,
\ end-code

