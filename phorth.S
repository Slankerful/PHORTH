#include <asm/unistd.h>

.cpu cortex-a53
.fpu neon-fp-armv8
.section forthcode,"axw"
@ stack pointer
DSP .req    r13
@ Return stack pointer
RSP .req    r11
@ Forth word instruction pointer
FIP .req    r10
.set F_IMM,0x80000000       @ Immediate bit
.set F_LEN,0x0FFFFFFF       @ Mask to give only length
.set F_SMG,0x40000000       @ Smudge bit
.set F_CON,0x20000000       @ Compile only bit

.macro pushdsp reg
str \reg,[DSP,#-4]!
.endm

.macro push2dsp
stmfd DSP!,{r0-r1}
.endm

.macro popdsp reg
ldr \reg,[DSP],#4
.endm

.macro pop2dsp
ldmia DSP!,{r0-r1}
.endm

.macro pushrsp reg
str \reg,[RSP,#-4]!
.endm

.macro poprsp reg
ldr \reg,[RSP],#4
.endm

.macro next
b _next
.endm
@ _docol is  the code run to execute every Forth word
@ that was defined by using a colon definition.
@ It runs straight into _next.
@ If we have reached here we are in a situation where we
@ have a Forth word being executed with structure:
@ Calling word:
@ +--------+------------------+---------------+---------------+-----+-----------------+
@ | header | address of docol | word1 address | word2 address | ... | address of exit |
@ +--------+------------------+---------------+---------------+-----+-----------------+
@                                       |
@                   +-------------------+     
@                   |
@ Called word:      v
@ +--------+------------------+---------------+---------------+-----+-----------------+
@ | header | address of docol | word3 address | word4 address | ... | address of exit |
@ +--------+------------------+---------------+---------------+-----+-----------------+
@ Now this word itself was called from another Forth word so the FIP points
@ to where we are in that calling word. Hence we push that FIP onto the return stack.
@ At the end of a word it executes EXIT which pops the FIP and continues execution from
@ the calling word's next definition.
_docol:
pushrsp FIP             @ store away the current FIP on the return stack
add FIP,r0,#4           @ expects cfa of word to execute in r0
			            @which will be there because _next would have loaded that.

@ _next is routine to go to next Forth word
_next:
ldr r0,[FIP],#4         @ Get code field address of next word to execute and bump up FIP
			            @ to point to next word
ldr r1,[r0]             @ Execute code pointed to by CFA
bx r1

.set link,0


@ Macro to define header of a Forth word
@ Structure:
@ +------------+------------------+------+
@ | Link field | length and flags | name |
@ +------------+------------------+------+
@ This macro also set the link field to point to the previous entry
@ in the dictionary (which is zero for the first word).
@ It sets up a global lbal for link address and set the variable 'link'
@ to that address so that the next definition can refer to it.
@ It also sets up a global label pointing to the address after the header
@ (the code field)
.macro defhead name,len,flags=0,label
.align 2
.global link_\label
link_\label:
.int link
.set link,link_\label   @ set up link for next definition
.word \len + \flags
.ascii "\name"
.align 2
.global \label
\label:
.endm

@ Uses the defhead macro to define the header part of a Forth word
@ that was created using a colon (:) definition.
@ It then places the address of the code required to execute each of
@ the words defined in that colon definition (docol)
.macro defword name,len,flags=0,label
defhead "\name",\len,\flags,\label
.int _docol
.endm

@ Uses the defhead macro to set up the header
@ then places an address which points directly to the next address
@ so that the machine code gets executed
.macro defcode name,len,flags=0,label
defhead "\name",\len,\flags,\label
.int code_\label
.global code_\label
code_\label:
.endm
@ macro to define a word based on a create...does> defining word
.macro defdoes name,len,flags=0,label,doesaddr
defhead "\name",\len,\flags,\label
.int \doesaddr
.endm

@ A macro to define a variable
@ First defines the header and then puts a code field
@ that points to the code which will push the address
@ of the parameter field which contains the actual value.
.macro defvar name,len,flags=0,label,initial=0
defhead "\name",\len,\flags,\label
.int var_dodoes
.global var_\label
var_\label:
.int \initial
.endm

@ A macro to define a constant
@ First defines the header and then puts a code field
@ that points to the code which will push the value
@ in the following address onto the stack
.macro defconst name,len,flags=0,label,value
defhead "\name",\len,\flags,\label
.int const_dodoes
const_\label:
.int \value
.endm

.macro defvalue name,len,flags=0,label,value=0
defhead "\name",\len,\flags,\label
.int val_dodoes
value_\label:
.int \value
.endm

.macro defenv name,len,label
.align 2
.global link_\label
link_\label:
.int e_link
.set e_link,link_\label
.word \len
.ascii "\name"
.align 2
.global \label
\label:
.int _docol
.endm

.macro throwmsg string
.data 1
msg_label\@:
.int msg_end\@ - msg_string\@
.int msg_string\@
.data 2
msg_string\@:
.ascii "\string"
msg_end\@:
.endm

throwmsg   "ABORT"                                         @ -01
throwmsg   "ABORT\""                                       @ -02
throwmsg   "stack overflow"                                @ -03
throwmsg   "stack underflow"                               @ -04
throwmsg   "return stack overflow"                         @ -05
throwmsg   "return stack underflow"                        @ -06
throwmsg   "do-loops nested too deeply during execution"   @ -07
throwmsg   "dictionary overflow"                           @ -08
throwmsg   "invalid memory address"                        @ -09
throwmsg   "division by zero"                              @ -10
throwmsg   "result out of range"                           @ -11
throwmsg   "argument type mismatch"                        @ -12
throwmsg   "undefined word"                                @ -13
throwmsg   "interpreting a compile-only word"              @ -14
throwmsg   "invalid FORGET"                                @ -15
throwmsg   "attempt to use zero-length string as a name"   @ -16
throwmsg   "pictured numeric output string overflow"       @ -17
throwmsg   "parsed string overflow"                        @ -18
throwmsg   "definition name too long"                      @ -19
throwmsg   "write to a read-only location"                 @ -20
throwmsg   "unsupported operation (e.g., AT-XY on a too-dumb terminal)" @ -21
throwmsg   "control structure mismatch"                    @ -22
throwmsg   "address alignment exception"                   @ -23
throwmsg   "invalid numeric argument"                      @ -24
throwmsg   "return stack imbalance"                        @ -25
throwmsg   "loop parameters unavailable"                   @ -26
throwmsg   "invalid recursion"                             @ -27
throwmsg   "user interrupt"                                @ -28
throwmsg   "compiler nesting"                              @ -29
throwmsg   "obsolescent feature"                           @ -30
throwmsg   ">BODY used on non-CREATEd definition"          @ -31
throwmsg   "invalid name argument (e.g., TO xxx)"          @ -32
throwmsg   "block read exception"                          @ -33
throwmsg   "block write exception"                         @ -34
throwmsg   "invalid block number"                          @ -35
throwmsg   "invalid file position"                         @ -36
throwmsg   "file I/O exception"                            @ -37
throwmsg   "non-existent file"                             @ -38
throwmsg   "unexpected end of file"                        @ -39
throwmsg   "invalid BASE for floating point conversion"    @ -40
throwmsg   "loss of precision"                             @ -41
throwmsg   "floating-point divide by zero"                 @ -42
throwmsg   "floating-point result out of range"            @ -43
throwmsg   "floating-point stack overflow"                 @ -44
throwmsg   "floating-point stack underflow"                @ -45
throwmsg   "floating-point invalid argument"               @ -46
throwmsg   "compilation word list deleted"                 @ -47
throwmsg   "invalid POSTPONE"                              @ -48
throwmsg   "search-order overflow"                         @ -49
throwmsg   "search-order underflow"                        @ -50
throwmsg   "compilation word list changed"                 @ -51
throwmsg   "control-flow stack overflow"                   @ -52
throwmsg   "exception stack overflow"                      @ -53
throwmsg   "floating-point underflow"                      @ -54
throwmsg   "floating-point unidentified fault"             @ -55
throwmsg   "QUIT"                                          @ -56
throwmsg   "exception in sending or receiving a character" @ -57
throwmsg   "[IF], [ELSE], or [THEN] exception"             @ -58

.align 5
defvar "STATE",5,,state @ Core
@ ( -- a-addr )
@ a-addr is the address of a cell holding the compilation-state flag.
defvar "CURRENT",7,,current,f_wordlist  @ wordlist for compilation
@ Current is a variable holding thea address of the  wordlist that words are to be compiled into
@ So CURRENT @ gives that address and CURRENT @ @ gives the lfa of the latest word in that wordlist
@ Initially things are set up so that it contains the address of the FORTH wordlist which in turn
@ has the link field address (lfa) of THROW
defvar "SP0",3,,sp0    @top of data stack
defvar "RP0",3,,rp0
defvar "BASE",4,,base,10    @numeric base for reading/writing numbers @ Core
defvar ">IN",3,,toin,0 @ Core
defvar "EOL",3,,eol,0       @ Indicates an end-of-line detected by WORD
defvar "BLK",3,,blk,0
defvar "HLD",3,,hld
defvar "RAKEVAR",7,,rakevar     @ Helps in raking up LEAVEs
defvar "THROWFRAME",10,,throwframe,0    @ Holds throw frame for catch/throw
defvar "VOC-LINK",8,,voclink,forth+8
defvar "DSSTART",7,,dsstart,ds

defvar "R/O",3,,rdonly,114         @ "r"        @ File
defvar "R/W",3,,rdwrite,11122      @ "r+"       @ File
defvar "W/O",3,,wronly,119         @ "w"        @ File


defvalue "SOURCE-ID",9,,sourceid,0 @ Core Ext
@ Identifies the input source as follows:
@   0       User input device
@   -1      String (via EVALUATE)
@   file-id Text file identified by file-id @ File Ext
defvalue "BAL",3,,bal,0
defvalue "HERE",4,,here       @address of top of dictionary space @ Core

defvalue "BCM_BASE",8,,bcmbase,0x3F000000
defvalue "GPIO_BASE",9,,gpiobase,0x3F200000

defconst "DOCOL",5,,docol,_docol
defconst "DOVAL",5,,doval,val_dodoes
defconst "TIB",3,,tib,line_buffer
defconst "FIB",3,,fib,file_buffer
defconst "STRB",4,,strb,string_buffer
defconst "PAD",3,,pad,pad_start @ Core Ext
@ ( -- c-addr )
@ Returns address of a transient region for use by programmers.
defconst "PADTOP",6,,padtop,pad_top
defconst "HOLDTOP",7,,holdtop,hold_top
defconst "FALSE",5,,false,0 @ Core Ext
defconst "TRUE",4,,true,-1 @ Core Ext
defconst "F_IMMED",7,,f_immed,F_IMM
defconst "F_CMPONLY",9,,f_cmponly,F_CON
defconst "F_SMUDGE",8,,f_smudge,F_SMG
defconst "MAXNEG",6,,maxneg,0x80000000
defconst "THROWTABLE",10,,throwtable,msg_label1
defconst "NEXT",4,,next,_next
defconst "DSTOP",5,,dstop,ds_top
defconst "BSP#",4,,bsphash,127
defconst "DEL#",4,,delhash,8
defconst "LF#",3,,lfhash,10

defconst "SYS_EXIT",8,,sys_exit,__NR_exit
defconst "SYS_EXIT",8,,SYS_EXIT,__NR_exit
defconst "SYS_OPEN",8,,SYS_OPEN,__NR_open
defconst "SYS_CLOSE",9,,SYS_CLOSE,__NR_close
defconst "SYS_READ",8,,SYS_READ,__NR_read
defconst "SYS_WRITE",9,,SYS_WRITE,__NR_write
defconst "SYS_CREAT",9,,SYS_CREAT,__NR_creat
defconst "SYS_BRK",7,,SYS_BRK,__NR_brk

defconst "O_RDONLY",8,,__O_RDONLY,0
defconst "O_WRONLY",8,,__O_WRONLY,1
defconst "O_RDWR",6,,__O_RDWR,2
defconst "O_CREAT",7,,__O_CREAT,0100
defconst "O_EXCL",6,,__O_EXCL,0200
defconst "O_TRUNC",7,,__O_TRUNC,01000
defconst "O_APPEND",8,,__O_APPEND,02000
defconst "O_NONBLOCK",10,,__O_NONBLOCK,04000

defvar "SOURCEVAR",9,,sourcevar,line_size
@ A 2 cell variable containing the address and length of the input buffer.
.int line_buffer

defvar "ERRWORD",7,,errword,0
.int 0

defvar "#ORDER",6,,hashorder,1
@ ( -- u )
@ A variable that contains the number of wordlists defined
@ followed by a list of wordlists
@ Initially set up with only FORTH in the wordlist
.int f_wordlist
.int 0
.int 0
.int 0
.int 0
.int 0
.int 0
.int 0

f_wordlist:
.int link_throw
.int link_forth

defconst "FORTH-WORDLIST",14,,forth_wordlist,f_wordlist @ Search
defconst "ENVIRONMENT-WORDLIST",20,,environment_wordlist,e_wordlist

.set e_link,0
defenv "/COUNTED-STRING",15,scounted_string
.int lit,0xffffff
.int exit

defenv "/HOLD",5,shold
.int lit,0x200
.int exit

defenv "/PAD",4,spad
.int lit,0x200
.int exit

defenv "ADDRESS-UNIT-BITS",17,addrunitbits
.int lit,8
.int exit

defenv "FLOORED",7,floored
.int true
.int exit

defenv "MAX-CHAR",8,maxchar
.int lit,127
.int exit

defenv "MAX-D",5,maxd
.int lit,0xFFFFFFFF
.int lit,0x7FFFFFFF
.int exit

defenv "MAX-N",5,maxn
.int lit,0x7FFFFFFF
.int exit

defenv "MAX-U",5,maxu
.int lit,0xFFFFFFFF
.int exit

defenv "MAX-UD",6,maxud
.int lit,0xFFFFFFFF
.int lit,0xFFFFFFFF
.int exit

defenv "RETURN-STACK-CELLS",18,retstackcells
.int lit,0x200
.int exit

defenv "STACK-CELLS",11,stackcells
.int lit,0x200
.int exit

e_wordlist:
.int link_stackcells
.align 5

.section forthcode,"axw"
@ EXIT finishes Forth word definitions by popping the FIP from the return stack
@ and then executing NEXT
defcode "EXIT",4,F_CON,exit @ Core
@ Execution:
@ ( -- ) ( R: nest-sys -- )
@ Return control to the calling definition specified by nest-sys
poprsp FIP
next

defcode "SYSCALL0",8,,syscall0
popdsp r7
swi 0
pushdsp r0
next

defcode "SYSCALL1",8,,syscall1
popdsp r7
popdsp r0
swi 0
pushdsp r0
next

defcode "SYSCALL2",8,,syscall2
popdsp r7
popdsp r0
popdsp r1
swi 0
pushdsp r0
next

defcode "SYSCALL3",8,,syscall3
popdsp r7
popdsp r0
popdsp r1
popdsp r2
swi 0
pushdsp r0
next

defcode "SP@",3,,spfetch
@ ( -- addr )
@ Pushes the current stack pointer onto the stack.
@ Note it will be the stack pointer as it was before you
@ pushed this onto the stack.
mov r0,DSP
pushdsp r0
next

defcode "RP@",3,,rpfetch
@ ( -- addr )
@ Pushes the current return stack pointer onto the data stack
mov r0,RSP
pushdsp r0
next

defword "DEPTH",5,,depth @ Core
@ ( -- +n )
@ Places the number of single-cell values in the data stack
@ onto the data stack (does not count itself)
@ : DEPTH ( -- +n) SP@ SP0 @ SWAP - 4 / ;
.int spfetch
.int sp0,fetch
.int swap
.int minus
.int four,slash
.int exit

@ Returns the link field address of the latest definition in the current vocabulary
@ : LATEST ( -- n) CURRENT @ @ ;
defword "LATEST",6,,latest
.int current
.int fetch,fetch
.int exit

defword "UNUSED",6,,unused @ Core Ext
@ ( -- u )
@ u is the amount of space remaining in the region addressed by HERE, in address units.
@ : UNUSED
@   dstop here -
@ ;
.int dstop
.int here
.int minus
.int exit

defword "DECIMAL",7,,decimal @ Core
.int lit,10
.int base,store
.int exit

defword "HEX",3,,hex @ Core Ext
@ ( -- )
@ Set content of base to 16.
.int lit,16
.int base,store
.int exit

defcode "BYE",3,,bye @ Tools Ext
ldr r12,=var_sp0
ldr r12,[r12]       @ SP0 @ - Get address of start of stack
add r12,r12,#4      @ Remove safety zone
mov sp,r12          @ Reset stack pointer
ldmfd sp!,{pc}
.ltorg

defcode "DROP",4,,drop @ Core
@ ( a -- )
@ drop top item of stack
add dsp,dsp,#4
next

defcode "DUP",3,,dup
@ ( a -- a a ) 
@ duplicates top stack item
ldr r0,[DSP]
pushdsp r0
next

defcode "?DUP",4,,qdup @ Core
@ ( x -- 0 | x x )
@ Duplicate x if it is non-zero
ldr r0,[DSP]
cmp r0,#0
strne r0,[DSP,#-4]!
next

defcode "SWAP",4,,swap @ Core
@ ( x1 x2 -- x2 x1 )
@ Exchange the top two stack items.
pop2dsp
pushdsp r0
pushdsp r1
next

defcode "OVER",4,,over @ Core
@ ( x1 x2 -- x1 x2 x1 ) copy second element on top
ldr r0,[DSP,#4]
pushdsp r0
next

defcode "ROT",3,,rot @ Core
@ ( x1 x2 x3 -- x2 x3 x1 )
@ Rotate the top 3 stack entries bringing
@ the third item to the top
popdsp r0
popdsp r1
popdsp r2
pushdsp r1
pushdsp r0
pushdsp r2
next

defcode "-ROT",4,,nrot      @ ( a b c -- c a b )
popdsp r0
popdsp r1
popdsp r2
pushdsp r0
pushdsp r2
pushdsp r1
next

defcode "PICK",4,,pick @ Core Ext
@ ( xu...x1 x0 u -- xu...x1 x0 xu )
@ Remove u from the stack. Copy the xu item to the top of the stack
popdsp r0
ldr r1,[DSP,r0,lsl#2]
pushdsp r1
next

defcode "ROLL",4,,roll @ Core Ext
@ (xu xu-1...x1 x0 u -- xu-1...x1 x0 xu )
@ Romeve u. Rotate u+1 items on the top of the stack.
popdsp r0
cmp r0,#0
ble _roll_exit
ldr r1,[DSP,r0,lsl#2]!      @ Obtain xu
_roll_loop:
subs r0,r0,#1
ldr r2,[DSP,#-4]!
str r2,[DSP,#4]
bgt _roll_loop
str r1,[DSP]
_roll_exit:
next

defcode "2DROP",5,,twodrop @ Core
@ ( x1 x2 -- )
@ Drop cell pair x1 x2 from the stack
add DSP,DSP,#8
next

defcode "2DUP",4,,twodup @ Core
@ ( x1 x2 -- x1 x2 x1 x2 )
@ Duplicate cell pair x1 x2.
ldmia DSP, {r0,r1}
push2dsp
next

defcode "2SWAP",5,,twoswap @ Core
@ ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
@ Exchange the top two cell pairs
ldmia DSP!,{r0-r3}
stmdb DSP!,{r0-r1}
stmdb DSP!,{r2-r3}
next

defcode "2OVER",5,,twoover @ Core
@ ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
@ Copy cell pair x1 x2 to the top of the stack. 
ldr r0,[DSP,#8]
ldr r1,[DSP,#12]
push2dsp
next

defcode "2ROT",4,,tworot @ Double Ext
@ ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
@ Rotates the top three cell pairs bringing
@ cell pair x1 x2  to the top of the stack.
ldmfd sp!,{r0,r1,r2,r3,r4,r5}
pushdsp r3
pushdsp r2
pushdsp r1
pushdsp r0
pushdsp r5
pushdsp r4
next

defcode "NIP",3,,nip @ Core Ext
@ ( n1 n2 -- n2 ) remove second item from stack
pop2dsp
pushdsp r0
next

defcode "TUCK",4,,tuck @ Core Ext
@ ( x1 x2 -- x2 x1 x2 )
@ Copy the top stack item below the second stack item
pop2dsp
pushdsp r0
pushdsp r1
pushdsp r0
next

defcode "=",1,,eq @ Core
@ ( x1 x2 -- flag)
@ flag is true iff x1 is bit-for-bit the same as x2
pop2dsp
cmp r0,r1
mvneq r0,#0
movne r0,#0
pushdsp r0
next

defcode "<>",2,,neq @ Core Ext
@ ( x1 x2 -- flag )
@ flag is true iff x1 is not bit-by-bit the same as x2.
pop2dsp
cmp r0,r1
mvnne r0,#0
moveq r0,#0
pushdsp r0
next

defcode "<",1,,lt @ Core
@ ( n1 n2 -- flag )
@ flag is true iff n1 is less than n2
pop2dsp
cmp r1,r0
mvnlt r0,#0
movge r0,#0
pushdsp r0
next

defcode ">",1,,gt @ Core
@ ( n1 n2 -- flag )
@ flag is true iff n1 is greater than n2
pop2dsp
cmp r1,r0
mvngt r0,#0
movle r0,#0
pushdsp r0
next

defcode "<=",2,,le
pop2dsp
cmp r1,r0
mvnle r0,#0
movgt r0,#0
pushdsp r0
next

defcode ">=",2,,ge
pop2dsp
cmp r1,r0
mvnge r0,#0
movlt r0,#0
pushdsp r0
next

defcode "U<",2,,ult @ Core
@ @ ( u1 u2 -- flag )
@ @ Return true if unsigned number u1 is less than u2
pop2dsp
cmp r0,r1
mvnhi r0,#0
movls r0,#0
pushdsp r0
next

defcode "U>",2,,ugt @ Core Ext
@ @ ( u1 u2 -- flag )
@ @ Return true if unsigned number u1 is greater than u2
pop2dsp
cmp r1,r0
mvnhi r0,#0
movls r0,#0
pushdsp r0
next

defcode "DU<",3,,dult @ Double Ext
@ ( ud1 ud2 -- flag )
@ flag is true iff ud1 is less than ud2
ldmia DSP!,{r0-r3} @ r0 = rhh, r1 = rhl, r2 = lhh, r3 = lhl
cmp r0,r2
mvnhi r4,#0
bhi _dultend
cmp r2,r0
movhi r4,#0
bhi _dultend
cmp r1,r3
mvnhi r4,#0
movls r4,#0
_dultend:
pushdsp r4
next

defword "WITHIN",6,,within @ Core Ext
@ ( n1|u1 n2|u2 n3|u3 -- flag)
@ Performs a comparison of a test value n1|u1 with a lower limit n2|u2 and an upper limit
@ n3|u3, returning true if either (n2|u2 < n3|u3 and (n2|u2 < n1|u2 and n1|u1 < n3|u3))
@ or (n2|u2 > n3|u3 and (n2|u2 <= n1|u1 or n1|u1 < n3|u3)) is true, returning false otherwise.
@ : WITHIN 
@   over - >r
@   - r> u<
@ ;
.int over,minus,tor
.int minus,rto,ult
.int exit

defcode "0=",2,,zeq @ Core
@ ( x -- flag )
@ flag is true iff x is equal to zero.
popdsp r0
cmp r0,#0
mvneq r0,#0
movne r0,#0
pushdsp r0
next

defcode "0<>",3,,zne @ Core Ext
@ ( x -- flag )
@ flag is true iff x is not equal to zero.
popdsp r0
cmp r0,#0
mvnne r0,#0
moveq r0,#0
pushdsp r0
next

defcode "0<",2,,zlt @ Core
@ ( n -- flag )
@ flag is true iff n is less than zero.
popdsp r0
cmp r0,#0
mvnlt r0,#0
movge r0,#0
pushdsp r0
next

defcode "0>",2,,zgt @ Core Ext
@ ( n -- flag )
@ flag is true iff n is greater than zero.
popdsp r0
cmp r0,#0
mvngt r0,#0
movle r0,#0
pushdsp r0
next

defcode "0<=",3,,zle
popdsp r0
cmp r0,#0
mvnle r0,#0
movgt r0,#0
pushdsp r0
next

defcode "0>=",3,,zge
popdsp r0
cmp r0,#0
mvnge r0,#0
movlt r0,#0
pushdsp r0
next

defcode "D<",2,,dlt @ Double
@ ( d1 d2 -- flag )
@ flag is true iff d1 is less than d2
ldmia DSP!,{r0-r3} @ r0 = rhh, r1 = rhl, r2 = lhh, r3 = lhl
cmp r0,r2
mvngt r4,#0
bgt _dltend
cmp r2,r0
movgt r4,#0
bgt _dltend
cmp r3,r1
movge r4,#0
mvnlt r4,#0
_dltend:
pushdsp r4
next

defword "D=",2,,deq @ Double
@ ( xd1 xd2 -- flag )
@ flag is ture iff xd1 is bit-for-bit the same as xd2
.int dminus,dzeq
.int exit

defword "D<>",3,,dne
.int deq,not
.int exit

defword "D0<",3,,dzlt @ Double
@ ( d -- flag )
@ flag is true iff d is less than zero.
.int nip,zlt
.int exit

defword "D0=",3,,dzeq @ Double
@ ( xd -- flag )
@ flag is true iff xd is eqal to zero.
.int or
.int zeq
.int exit

defword "NOT",3,,not    @ Syntactic sugaring for 0=
.int zeq
.int exit

defconst "0",1,,zero,0
defconst "1",1,,one,1
defconst "2",1,,two,2
defconst "3",1,,three,3
defconst "4",1,,four,4
defconst "-1",2,,minus1,-1
defconst "-2",2,,minus2,-2

defcode "ABS",3,,abs @ Core
@ ( n -- u )
@ u is the absolute value of n
popdsp r0
cmp r0,#0
rsblt r0,r0,#0
pushdsp r0
next

defword "DABS",4,,dabs @ Double
@ ( d -- ud )
@ ud is the absolute value of d
@ : DABS
@   dup 0<                  \ High order part less than zero?
@   if dnegate then         \ If so negate
@ ;
.int dup,zlt
.int zbranch,8
.int dnegate
.int exit


defcode "AND",3,,and @ Core
@ ( x1 x2 -- x3 )
@ x3 is the bit logical and of x1 and x2
pop2dsp
and r0,r0,r1
pushdsp r0
next

defcode "OR",2,,or @ Core
@ ( x1 x2 -- x3 )
@ x3 is the bit-by-bit inclusive or of x1 and x2.
pop2dsp
orr r0,r0,r1
pushdsp r0
next

defcode "XOR",3,,xor @ Core
@ ( x1 x2 -- x3 )
@ x3 is the bit-by-bit exclusive-or of x1 with x2.
pop2dsp
eor r0,r0,r1
pushdsp r0
next

defcode "MIN",3,,min @ Core
@ ( n1 n2 -- n3 )
@ n3 is the lesser of n1 and n2.
pop2dsp
cmp r0,r1
strlt r0,[DSP,#-4]!
strge r1,[DSP,#-4]!
next

defcode "MAX",3,,max @ Core
@ ( n1 n2 -- n3 )
@ n3 is the greater of n1 and n2.
pop2dsp
cmp r0,r1
strlt r1,[DSP,#-4]!
strge r0,[DSP,#-4]!
next

defword "DMAX",4,,dmax @ Double
@ ( d1 d2 -- d3 )
@ d3 is the greater of d1 and d2
@ : DMAX
@   2over 2over
@   d<
@   if 2swap then
@   2drop
@ ;
.int twoover,twoover
.int dlt
.int zbranch,8
.int twoswap
.int twodrop
.int exit

defword "DMIN",4,,dmin @ Double
@ ( d1 d2 -- d3 )
@ d3 is the lesser of d1 and d2
@ : DMIN
@   2over 2over
@   d< not
@   if 2swap then
@   2drop
@   then
@ ;
.int twoover,twoover
.int dlt,not
.int zbranch,8
.int twoswap
.int twodrop
.int exit


defword "INVERT",6,,invert @ Core
@ ( x1 -- x2 )
@ Inverts all bits of x2 giving the logical inverse x2
.int minus1
.int xor
.int exit

defword "NEGATE",6,,negate @ Core
@ ( n1 -- n2 )
@ Negate n1 giving the arithmetic inverse n2
.int invert
.int oneplus
.int exit

defword "DNEGATE",7,,dnegate @ Double
@ ( d1 -- d2 )
@ d2 is the negation of d1
@ : DNEGATE
@   invert swap             \ Invert high order and swap
@   invert                  \ Invert low order
@   1 u+                    \ unsigned add with carry
@   rot +                   \ Get high order back and add in carry
@   ;
.int invert,swap
.int invert,one,uplus
.int rot,plus
.int exit

defcode "RSHIFT",6,,rshift @ Core
@ (x1 u -- x2)
@ Perform a logical right shift of u bit-places on x1
@ giving x2. Put zeroes into the most significant bits
@ vacated by the shift.
pop2dsp
mov r0,r1,lsr r0
pushdsp r0
next

defcode "2/",2,,twoslash @ Core
@ ( x1 -- x2 )
@ x2 is the result of shifting x1 one bit toward the least-significant bit, leaving
@ the most-significant bit unchanged.
popdsp r0
mov r0,r0,asr#1
pushdsp r0
next

defcode "LSHIFT",6,,lshift @ Core
@ ( x1 u -- x2 )
@ Perform a logical left shift of u bit places
@ on x1 giving x2.
pop2dsp
mov r0,r1,lsl r0
pushdsp r0
next

defcode "+",1,,plus @ Core
@ ( n1|u1 n2|u2 -- n3|u3 )
@ Add n2|u2 to n1|u1 giving the sum n3|u3.
pop2dsp
add r0,r0,r1
pushdsp r0
next


.ltorg

defword "D+",2,,dplus @ Double
@ ( d1|ud1 d2|ud2 -- d3|ud3 )
@ Add d2|ud2 to d1|ud1, giving the sum d3|ud3
@ : D+
@   >r swap >r
@   u+
@   r> r>
@   + +
@ ;
.int tor,swap,tor           @ hive off high order words
.int uplus                  @ add lower order words
.int rto,rto                @ recover high order words
.int plus,plus              @ and add with carry
.int exit

defword "D-",2,,dminus @ Double
@ ( d1|ud1 d2|ud2 -- d3|ud3 )
@ Subtract d2|ud2 from d1|ud1 giving the difference d3|ud3.
.int dnegate
.int dplus
.int exit

defword "D2*",3,,dtwotimes @ Double
@ ( xd1 -- xd2 )
@ xd2 is the result of shifting xd1 one bit toward the most-significant bit, filling
@ the vacated least-significant bit with zero.
@ : D2*
@   swap dup
@   0< >r
@   2* swap 2*
@   r> -
@ ;
.int swap,dup
.int zlt,tor
.int twotimes
.int swap,twotimes
.int rto,minus
.int exit

defword "D2/",3,,dtwoslash @ Double
@ ( xd1 -- xd2 )
@ xd2 is the result of shifting xd1 one bit toward the least-significant bit, leaving
@ the most-significant bit unchanged.
@ : D2/
@   dup 0<
@   >r
@   2/ swap
@   2/ swap
@   r> -
@ ;
.int dup,zlt
.int tor
.int twoslash,swap
.int twoslash,swap
.int rto,minus
.int exit

defcode "1+",2,,oneplus @ Core
@ ( n1|u1 -- n2|u2 )
@ Add 1 to n1|u1 giving n2|u2
popdsp r0
add r0,r0,#1
pushdsp r0
next

defcode "1-",2,,oneminus @ Core
@ ( n1|u1 -- n2|u2 )
@ Subtract 1 from n1|u2 to give n2|u2
popdsp r0
sub r0,r0,#1
pushdsp r0
next

defword "M+",2,,mplus @ Double
@ ( d1|ud1 n -- d2|ud2 )
@ Add n1 to d1|ud1 giving the sum d2|ud2
.int stod
.int dplus
.int exit

@ Unsigned addition of u1 and u2 giving result u3 and 0 on tos
@ if no carry took place or 1 or a carry had taken place.
defcode "U+",2,,uplus       @ ( u1 u2 -- u3 0|1 )
pop2dsp
adds r0,r0,r1
movcc r1,#0
movcs r1,#1
pushdsp r0
pushdsp r1
next

defcode "-",1,,minus @ Core
@ ( n1|u1 n2|u2 -- n3|u3 )
@ Subtract n2|u2 from n1|u1 giving the difference n3|u3.
pop2dsp
sub r0,r1,r0
pushdsp r0
next

defcode "*",1,,times @ Core
@ ( n1|u1 n2|u2 -- n3|u3 )
@ Multiply n1|u1 by n2|u2 giving the product n3|u3
pop2dsp
mul r2,r1,r0
pushdsp r2
next

defcode "2*",2,,twotimes @ Core
@ ( x1 -- x2 )
@ x2 is the result of shifting x1 one bit towards the most-significant bit, filling
@ the vacated least-significant bit with zero.
popdsp r0
mov r0,r0,lsl#1
pushdsp r0
next

defword "/",1,,slash @ Code
@ ( n1 n2 -- n3 )
@ Divide n1 by n2 giving the single-cell quotient n3
.int smod,nip
.int exit

defword "MOD",3,,mod @ Core
@ ( n1 n2 -- n3 )
@ Divide n1 by n2 giving the remainder n3.
.int smod,drop
.int exit


defcode "UM*",3,,umtimes @ Core
@ ( u1 u2 -- ud )
@ Unsigned multiply of two single numbers to give an unsigned double number
popdsp r0
popdsp r1
umull r2,r3,r0,r1
pushdsp r2
pushdsp r3
next

@defword "UM*",3,,umtimes                @ ( u1 u2 -- ud )
@.int zero,swap
@.int lit,32,zero
@.int pdo
@.int dup,uplus
@.int tor,tor
@.int dup,uplus,rto
@.int plus,rto
@.int zbranch,24
@.int tor,over,uplus
@.int rto,plus
@.int ploop,-68
@.int rot,drop
@.int exit


defcode "M*",2,,mtimes @ Core
@ ( n1 n2 -- d )
@ Signed multiply of two single numbers to give an signed double number
popdsp r0
popdsp r1
smull r2,r3,r0,r1
pushdsp r2
pushdsp r3
next

@ defword "M*",2,,mtimes
@ @ ( n1 n2 -- d )
@ @ Signed multiply of two single numbers n1 and n2 to give a signed double d.
@ @ : M*
@ @   2dup xor >r         \ Store away sign of result
@ @   abs swap abs        \ Take absolute value of both multiplicands
@ @   um*                 \ Do unsigned multiply
@ @   r> 0<               \ Is result supposed to be negative?
@ @   if dnegate then     \ Negate it if it is
@ @ ;
@ .int twodup,xor
@ .int tor
@ .int abs,swap,abs
@ .int umtimes
@ .int rto
@ .int zlt
@ .int zbranch,8
@ .int dnegate
@ .int exit

defword "M*/",3,,mstarslash @ Double
@ ( d1 n1 +n2 -- d2 )
@ Performs multiply of double d1 by number n1 to give a
@ triple precision result t. Then divides t by +n2
@ to give a double number quotient d2
.int tor,stod
.int tor,abs,nrot
.int stod,rto
.int xor,rto
.int swap,tor,tor
.int dabs,rot,tuck
.int umtimes,twoswap,umtimes
.int swap,tor,zero
.int dplus,rto,nrot
.int rfetch,ummod
.int nrot,rto,ummod
.int nrot,rto
.int zbranch,32
.int zbranch,12
.int one,zero
.int dnegate
.int branch,8
.int drop
.int exit

defcode "U/MOD",5,,umod
pop2dsp         @ r0 = divisor, r1 = dividend    
udiv r2,r1,r0
mul r3,r2,r0    @ multiply quotient by divisor
sub r3,r1,r3
pushdsp r3
pushdsp r2
next

defword "UM/MOD",6,,ummod @ Core
@ ( ud u1 -- u2 u3 )
@ Unsigned division of a double number by a single number.
@ Leaves remainder u2 and quotient u3
@: UM/MOD           \ ( ud u1 -- u2 u3 )
@   DUP 0=                  \ Test for division by zero
@   IF -10 THROW THEN
@   2DUP U<                 \ If double number's high word is < divisor (both numbers treated
@                           \ as unsigned) we are good to go,
@                           \ otherwise we have an issue where the result would overflow
@                           \ a single number.
@   IF  
@		NEGATE 32 0	        \ Negate the divisor and set up a do loop
@		DO				     
@			>R DUP			\ store away divisor and dup high word
@			u+			    \ unsigned add with carry
@			>R >R			\ store away carry and result
@			DUP u+ R>		\ dup of low word, unsigned add with carry and bring back high result.
@			+ DUP			\ add low word carry and high word result
@			R> R@ SWAP		 
@			>R u+ R>		\ ( u4 u5 u6 0|1 0|1 ) ( R: -u1 )
@			OR			\ ( u4 u5 u6 flag ) ( R: -u1 )
@			IF
@				>R DROP		\ ( u4 ) ( R: -u1 u6 )
@				1+ R>		\ ( u7 u6 ) ( R: -u1 )
@			ELSE
@				DROP		\ ( u4 u5 ) ( R: -u1 )
@			THEN
@			R>
@		LOOP
@		DROP SWAP 
@   ELSE
@       -11 THROW
@   THEN
@ ;
.int dup,zeq
.int zbranch,16
.int lit,-10
.int throw
.int twodup,ult
.int zbranch,156
.int negate
.int lit,32,zero
.int pdo
.int tor,dup
.int uplus
.int tor,tor
.int dup,uplus,rto
.int plus,dup
.int rto,rfetch,swap
.int tor,uplus,rto
.int or
.int zbranch,28
.int tor,drop
.int oneplus,rto
.int branch,8
.int drop
.int rto
.int ploop,-112
.int drop,swap
.int branch,16
.int lit,-11
.int throw
.int exit

defword "FM/MOD",6,,fmmod @ Core 
@ ( d n1 -- n2 n3 )
@ Floored divide of d by n1, giving quotient n3 and remainder n2
@ : FM/MOD      ( d n1 -- n2 n3 )
@	DUP >R 2DUP				( Store away divisor, duplicate divisor and high part of double ) 
@	XOR >R >R				( xor divisor and high part, store away xor and divisor )
@   DUP 0<					( test if high part is negative )
@	IF DNEGATE THEN				( if it is, make it positive )
@   R@ ABS UM/MOD				( get back absolute value of divisor 
@   R> 0<                                   ( l h -- l h flag ) (R: n1 -- )
@   IF SWAP NEGATE SWAP THEN                ( l h -- -l h )
@   R> 0<                                   ( l h -- l h flag )
@   IF 
@       NEGATE OVER                     ( l h -- l h l )
@       IF
@           R@ ROT -                       (  )
@           SWAP 1-
@       THEN
@       R> DROP
@       0 OVER <
@       IF -11 THROW THEN
@       EXIT
@   THEN
@   R> DROP
@   DUP 0<
@   IF -11 THROW THEN
@ ;
.int dup,tor,twodup
.int xor,tor,tor
.int dup,zlt
.int zbranch,8
.int dnegate
.int rfetch,abs
.int ummod
.int rto,zlt
.int zbranch,16
.int swap,negate,swap
.int rto,zlt
.int zbranch,84
.int negate
.int over
.int zbranch,24
.int rfetch,rot,minus
.int swap,oneminus
.int rto,drop
.int zero,over,lt
.int zbranch,16
.int lit,-11
.int throw
.int exit
.int rto,drop
.int dup,zlt
.int zbranch,16
.int lit,-11
.int throw
.int exit

defword "SM/REM",6,,smrem @ Core
@ ( d1 n1 -- n2 n3 )
@ Symmetric divide of d by n1, giving quotient n3 and remainder n2
@ : SM/REM
@   2dup xor >r                \ Store away sign of result
@   over >r >r
@   dup 0<
@   if dnegate then
@   r> abs
@   um/mod
@   r> 0<
@   if swap negate swap then
@   r> 0<
@   if
@       negate 0 over
@       < 0= if exit then
@       -11 throw
@   then
@   dup 0<
@   if -11 throw then
@ ;
.int twodup,xor
.int tor,over,tor
.int tor,dup,zlt
.int zbranch,8
.int dnegate
.int rto,abs
.int ummod
.int rto,zlt
.int zbranch,16
.int swap,negate,swap
.int rto,zlt
.int zbranch,48
.int negate
.int zero,over
.int lt,zeq
.int zbranch,8
.int exit
.int lit,-11
.int throw
.int dup,zlt
.int zbranch,16
.int lit,-11
.int throw
.int exit

defword "*/MOD",5,,ssmod @ Core
@ ( n1 n2 n3 -- n4 n5 )
@ Multiply n1 by n2 giving a double intermediate result,
@ then divide by n3 to give single cell remainder n4
@ and quotient n5
@ : */MOD >R M* R> FM/MOD ;
.int tor
.int mtimes
.int rto
.int fmmod
.int exit

defword "*/",2,,sslash @ Core
@ ( n1 n2 n3 -- n4 )
@ Multiply n1 by n2 giving a double intermediate result
@ then divide by n3 giving the quotient n4
.int ssmod
.int nip
.int exit

defword "/MOD",4,,smod  @ Core
@ ( a b -- r q ) Gives remainder and quotient of a/b
.int tor,stod,rto
.int fmmod
.int exit

defword "U.",2,,udot @ Core
@ ( u -- )
@ display u in free field format.
.int zero,ddot
.int exit

defcode "S>D",3,,stod @ Core
@ ( n -- d )
@ Convert the number n to the double cell number d
@ with the same numerical value.
popdsp r0
cmp r0,#0
movge r1,#0
movlt r1,#-1
pushdsp r0
pushdsp r1
next

defword "D>S",3,,dtos @ Double
@ ( d -- n )
@ n is the equivalent of d.
.int drop
.int exit

defword "HOLD",4,,hold @ Core
@ ( char -- )
@ Within a <# #> delimited number conversion, HOLD adds 'char' to the beginning of the
@ pictured numeric output string.
@ : HOLD
@   hld @                       \ Get current position in pictured string
@   1 chars -                   \ Step back one char
@   dup hld !                   \ Update position
@   c!                          \ Store the character 
.int hld,fetch
.int one,chars
.int minus,dup
.int hld,store
.int cstore
.int exit

defword "HOLDS",5,,holds @ Core Ext
@ ( c-addr u -- )
@ Adds the string represented by c-addr u to the pictured output string
@ : HOLDS
@   hld @                   \ Get current pictured output string position
@   over -                  \ decrement pointer by length of string
@   dup hld !               \ Update pointer
@   swap move               \ Move string into pictured output area
@   ;
.int hld,fetch              @ ( c-addr u addr )
.int over,minus             @ ( c-addr u addr2 )
.int dup,hld,store
.int swap,move
.int exit

defword "<#",2,,lthash @ Core
@ ( -- )
@ Initialize the pictured numeric output conversion process
@ Note - pictured output is built by going back (from least
@ significant digit to most significant).
@ : <#
@   holdtop                 \ Pointer to end of pictured output conversion area
@   hld !                   \ Initialize pointer variable
@ ;
.int holdtop
.int hld,store
.int exit

defword "#",1,,hash @ Core
@ ( ud1 -- ud2 )
@ Divide ud1 by the number in BASE giving the quotient and the remainder n (n is the least significant
@ digit of ud1. Convert n to external form and add the resulting character to the beginning of the
@ pictured numeric output string.
@ : #                           ( ud1 -- ud2 )
@   0 base @                    ( ul uh 0 base )    Form a new unsigned double number from ud1 high cell
@   um/mod >r                   ( ul rem )          Put quotient on return stack
@   base @                      ( ul rem base )
@   um/mod swap                 ( quo rem )         
@   9 over <                    ( quo rem 0|-1 )    Check if remainder is digit in decimal
@   65 57                       ( quo rem 0|-1 65 57 ) 65 = 'A', 57 = '9'
@   1+ -                        ( quo rem 0|-1 7 )
@   and +                       ( quo n )
@   1+ -                        ( quo rem 0|-1 7 )
@   and +                       ( quo n )
@   48 +                        ( quo char ) 48 = '0'
@   hold                        ( quo ) Put char in pictured output string
@   r>                          ( ud ) Divided ud1, ready for further division
@   ;
.int zero,base,fetch
.int ummod,tor
.int base,fetch
.int ummod,swap
.int lit,9,over,lt
.int lit,65,lit,57              @ characters 'A' and '9'
.int oneplus,minus
.int and,plus
.int lit,48                     @ Character '0'
.int plus,hold
.int rto
.int exit

defword "#>",2,,hashgt @ Core
@ ( xd -- c-addr u )
@ Drop xd. Make the pictured numeric output string available as a character string.
@ : #>
@   2drop
@   hld @                   \ Get pointer to current position in pictured output string
@   holdtop over -          \ Leave c-addr u
@   chars                   \ and convert to length in chars
@ ;
.int twodrop
.int hld,fetch
.int holdtop,over,minus
.int chars
.int exit

defword "#S",2,,hashs @ Core
@ ( ud -- 0 0 )
@ Convert one digit of ud1 accroding to the rule for #. Continue conversion until the quotient is zero.
@ : #S
@   begin
@       #
@       2dup or 0=                  \ Have we got zero yet?
@   until
@ ;
.int hash
.int twodup
.int or,zeq
.int zbranch,-20
.int exit

defword ".",1,,dot @ Core
@ ( n -- )
@ Dispplay n in free field format.
.int stod,ddot
.int exit

defword "SIGN",4,,sign @ Core
@ ( n -- )
@ If n is negative, add a minus sign to the beginning of the pictured numeric output string.
@ : SIGN
@   0<
@   if [char] - hold then
@ ;
.int zlt
.int zbranch,16
.int lit,45,hold
.int exit

defword "(D.)",4,,pddot
@ The basic number conversion routine - used by other print routines.
@ : (D.)   ( d --  c-addr u )
@   swap over dup           ( high low high high )
@   0< if
@       dnegate             ( high ud )
@   then
@   <# #s                   ( high 0 0 )
@   rot                     ( 0 0 high )
@   sign                    \ add sign or not according to sign og high part of d
@ ;
.int swap,over,dup
.int zlt
.int zbranch,8
.int dnegate
.int lthash
.int hashs
.int rot
.int sign
.int hashgt
.int exit

defword "D.",2,,ddot @ Double
@ ( d -- )
@ Display d in free field format.
.int pddot
.int type
.int space
.int exit

defword "UD.",3,,uddot
.int lthash
.int hashs
.int hashgt
.int type
.int space
.int exit

defword ".S",2,,dots @ Tools
@ ( -- )
@ Copy and display the vaslues currently on the stack.
.int lits,1
.ascii "<"
.align 2
.int type
.int depth
.int stod,pddot,type
.int lits,1
.ascii ">"
.align 2
.int type
.int space
.int depth,qdup
.int zbranch,92
.int zero
.int pdo
.int sp0,fetch
.int i,oneplus
.int cells,minus
.int fetch
.int base,fetch
.int lit,10,eq
.int zbranch,16
.int dot
.int branch,8
.int udot
.int ploop,-76
.int exit

defword "(.R)",4,,pdotr
@ ( n d -- )
@ Display a double number d, right aligned in a field n characters wide
.int pddot
.int rot,over
.int minus,dup,zgt
.int zbranch,16
.int spaces
.int branch,8
.int drop
.int type
.int exit

defword ".R",2,,dotr @ Core Ext
@ ( n1 n2 -- )
@ Display n1 right aligned in a field n2 characters wide
@ : .R
@   swap
@   s>d
@   (.R)            \ Factored out useful word used to print double number in a field
@ ;
.int swap,stod
.int pdotr
.int exit

defword "U.R",3,,udotr @ Core Ext
@ ( u n -- )
@ Display u right-aligned in a field n characters wide.
.int swap,zero
.int pdotr
.int exit


defword "D.R",3,,ddotr @ Double
@ ( d n -- )
@ Display d right-aligned in a field n characters wide.
.int nrot
.int pdotr
.int exit


defcode "ALIGNED",7,,aligned @ Core
@ ( addr -- a-addr )
@ a-addr is the first aligned address greater than or equal to addr.
popdsp r0
add r0,r0,#3
and r0,r0,#~3
pushdsp r0
next

defword "ALIGN",5,,align @ Core
@ ( -- )
@ If the data space pointer is not aligned, reswerve enough space to align it.
.int here
.int aligned
.int pto
.int value_here
.int exit

defword "ABORTCORE",9,,abortcore @ Core
@ ( i*x -- ) ( R: j*x -- )
@ Empty the data stack and then perform quit (which also clears the return stack)
.int sp0,fetch,spstore
.int quit

defword "ABORT",5,,abort @ Exception Ext
@ ( i*x -- ) ( R: j*x -- )
@ As abortcore but perform -1 THROW
.int sp0,fetch,spstore
.int minus1
.int throw
.int exit

defword "ABORT\"",6,F_IMM | F_CON,abortquotecore @ Core
@ Compilation: ( "ccc<quote>" -- ) Parse ccc delimited by a "; append the run-time semantics below
@ Run-time: ( i*x x1 -- | i*x) ( R: j*x -- | j*x)
@   Take x1 from the stack if not zero display ccc and perform an implementation defined abort sequence
@   that includes the function of ABORT.
@ : ABORT"
@   POSTPONE IF 
@   [CHAR] "
@   PARSE
@   POSTPONE LITS
@   DUP ,
@   SWAP OVER HERE SWAP
@   MOVE
@   ALLOT ALIGN
@   POSTPONE TYPE 
@   POSTPONE ABORTCORE
@   POSTPONE THEN
@   ; IMMEDIATE COMPILE-ONLY
.int if
.int lit,34         @ ASCII for "
.int parse
.int lit,lits,comma
.int dup,comma
.int swap,over,here,swap
.int move,allot,align
.int lit,type
.int comma          @ Compile in type 
.int lit,abortcore,comma
.int then
.int exit

defword "ABORT\"",6,F_IMM | F_CON,abortquote @ Exception Ext
@ Compilation: ( "ccc<quote>" -- ) Parse ccc delimited by a "; append the run-time semantics below
@ Run-time: ( i*x x1 -- | i*x) ( R: j*x -- | j*x)
@   Take x1 from the stack if not zero display ccc and perform an implementation defined abort sequence
@   that includes the function of ABORT.
@ : ABORT"
@   POSTPONE IF 
@   [CHAR] "
@   PARSE
@   POSTPONE LITS
@   DUP ,
@   SWAP OVER HERE SWAP
@   MOVE
@   ALLOT ALIGN
@   POSTPONE THROWFRAME POSTPONE @
@   POSTPONE 0=
@   POSTPONE IF
@   POSTPONE TYPE 
@   POSTPONE THEN
@   POSTPONE -2
@   POSTPONE THROW
@   POSTPONE THEN
@   ; IMMEDIATE COMPILE-ONLY
.int if
.int lit,34         @ ASCII for "
.int parse
.int lit,lits,comma
.int dup,comma
.int swap,over,here,swap
.int move,allot,align
.int lit,throwframe,comma
.int lit,fetch,comma
.int lit,zeq,comma
.int if
.int lit,type,comma
.int then
.int lit,minus2,comma
.int lit,throw,comma
.int then
.int exit

defword "BAL+",4,,balplus
.int bal
.int oneplus
.int pto
.int value_bal
.int exit
 
defword "BAL-",4,,balminus
.int bal
.int oneminus
.int pto
.int value_bal
.int exit

defword "C\"",2,F_IMM | F_CON, cquote @ Core Ext
@ Compilation: ( "ccc<quote>" -- )
@   Parce ccc delimited by " and append the run-time semantics below to the current definition.
@ Run-time: ( -- c-addr )
@   Return c-addr, a counted string consisting of the characters 'ccc'.
@ : C"
@   [char] "                \ Delimiter is "
@   parse                   ( c-addr u ) \ Parse the characters
@   postpone lits           \ Compile in code to handle counter string at run-time
@   dup ,                   \ Compile in count
@   swap over               ( u c-addr u )
@   here swap               ( u c-addr here u ) \ Set up for move
@   move                    ( u )
@   allot align             \ Allocate space is data space and align
@   postpone drop           \ When the defined word is executed, lits will leave ( c-addr u ) so we drop u
@   postpone lit 4 ,        \ Code to push 4 onto stack when defined word is executed
@   postpone -              \ subtract a cell length from c-addr to give the address of the counted string
@ ; immediate compile-only
.int lit,34                 @ a quote (")
.int parse
.int lit,lits,comma
.int dup,comma
.int swap,over
.int here,swap
.int move
.int allot,align
.int lit,drop,comma
.int lit,lit,comma
.int lit,4,comma
.int lit,minus,comma
.int exit

defword "S\"",2,F_IMM,squote @ Core
@ Compilation ( "ccc<quote>" -- )
@   Parse ccc delimited by ". Append the run-time semantics given below to the current definition.
@ Run-time: ( -- c-addr u )
@   Return c-addr and u describing a string consisting of the characters ccc.
@ : S"
@   [char] " 
@   parse                       \ Parse string delimited by "
@   state @
@   if                          \ Compiling
@       postpone lits           \ Compile in string handler into current definition
@       dup ,                   \ Compile in length
@       swap over here swap     \ Set up for move
@       move
@       allot align             \ Allocate enough space and align
@   else                        \ Interpreting
@       tuck here swap          \ Set up for move
@       move
@       here swap               \ Leave c-addr u
@   then
@ ; immediate
.int lit
.ascii "\""
.align 2
.int parse   	    @ ( -- c-addr u )
.int state,fetch    @ Compiling or interpreting?
.int zbranch,56
.int lit,lits
.int comma          @ Compile lits xt
.int dup,comma      @ ( addr len ) Compile length
.int swap,over      @ ( len addr len )
.int here           @ ( len addr len addr2 )
.int swap,move      @ ( len )
.int allot
.int align
.int exit
.int tuck,here,swap  @ ( addr len -- len addr here len ) Store away at here
.int move
.int here,swap
.int exit

defword ".\"",2,F_IMM | F_CON,dotquote @ Core
@ Compilation: ( "ccc<quote>" -- )
@   Parse cc delimited by ". Append the run-time semantics given below to the current definition
@ Run-time: ( -- )
@   Display ccc
@ : ."
@   postpone S"
@   postpone type
@ ;  
.int squote
.int lit,type,comma
.int exit

defword ",\"",2,,commaquote
@ Compiles the following word (delimited by ") into the dictionary
@ : ,"
@   postpone s"
@   dup rot 2dup
@   dup cell+
@   rot move !
@   cell+ allot
@   ;       
.int squote
.int dup,rot,twodup
.int dup,cellplus
.int rot,move,store
.int cellplus,allot
.int exit

.ltorg

defcode "IMMEDIATE",9,,immediate @ Core
@ ( -- )
@ Make the most recent definition an immediate word.
ldr r0,=var_current
ldr r0,[r0]             @ Get address of current wordlist
ldr r0,[r0]             @ Get address of latest definition in that wordlist
ldr r1,[r0,#4]!         @ Get length byte
orr r1,r1,#F_IMM        @ Set immediate bit
str r1,[r0]             @ Store back in length byte
next

defcode "COMPILE-ONLY",12,,componly
ldr r0,=var_current
ldr r0,[r0]             @ Get address of current wordlist
ldr r0,[r0]             @ Get address of latest definition in that wordlist
ldr r1,[r0,#4]!         @ Get length byte
orr r1,r1,#F_CON        @ Set compile-only bit
str r1,[r0]             @ Store back in length word
next

defword "AHEAD",5,F_IMM | F_CON,ahead   @ Tools Ext
@ Compilation: ( C: -- orig ) ( -- 1 )
@   Begin an unconditional forward branch by putting the location of a new unresolved forward reference 'orig'
@   onto the (control) stack. Append the run-time semantics below to the current definition.
@   'orig' must be resolved, e.g. by THEN
@   Also leave 1 on the stack and update bal (for control-structure checking)
@ Run-time: ( -- )
@   Continue execution at the location specified by the resolution of 'orig'.
@ : AHEAD postpone branch here 0 , 1 bal+ ; immediate compile-only
.int lit,branch,comma
.int here
.int zero,comma
.int one                @ orig-type is 1
.int balplus
.int exit

defword "IF",2,F_IMM | F_CON,if @ Core
@ Compilation: ( C: -- orig ) ( -- 1 )
@   Begin a conditional forward branch by putting the location of a new unresolved forward reference 'orig'
@   onto the (control) stack. Append the run time semantics below to the current definition.
@   'orig' must be resolved, e.g. by a following THEN or ELSE.
@   Also leave 1 on the stack and update bal (for control-structure checking)
@ Run time:  ( x -- )
@   If all bits of x are zero, continue execution at the location specified by the resolution
@   of 'orig'. Otherwise continue execution.
@ : IF postpone 0branch here 0 , 1 bal+ ; immediate compile-only
.int lit,zbranch,comma  @( Compile in 0branch which, at runtime will branch off if item on top of stack is 0 )
.int here               @( Leave 'orig' on stack )
.int zero,comma
.int one
.int balplus
.int exit

defword "THEN",4,F_IMM | F_CON,then @ Core
@ Compilation: ( C: orig -- ) ( 1 -- )
@   Append the run-time semantics below to the current definition.
@   Resolve the forward reference orig using the location of the appended run-time semantics.
@ Run-time: ( -- )
@   Continue execution
@ : THEN 1- if -22 throw then
@       dup here    ( orig orig here )
@       swap -      ( orig offset )
@       swap !      \ Update forward reference placed by, for example, IF or ELSE to point to here.
@       bal-
@   ;
.int oneminus
.int zbranch,16
.int lit,-22
.int throw
.int dup,here
.int swap,minus,swap,store
.int balminus
.int exit

defword "ELSE",4,F_IMM | F_CON,else @ Core
@ Compilation: ( C: orig1 -- orig2 )
@   Put the location of a new unresolved forward reference orig2 onto the (control) stack.
@   Append the run-time semantics given below to the current definition. The semantics will be
@   incomplete until orig2 is resolved (e.g. by THEN).
@   Resolve the forward reference orig1 using the location following the appended run-time semantics.
@ Run-time: ( -- )
@   Continue execution at the location given by the resolution of orig2.
@ : ELSE postpone ahead 2swap postpone then ; immediate compile-only
.int ahead          @ ( orig1 1 orig2 1 )                
.int twoswap        @ ( orig2 1 orig1 1 )
.int then           @ ( orig2 1 ) \ Resolve orig1
.int exit

defword "CASE",4,F_IMM | F_CON,case @ Core Ext
@ Compilation: ( C: -- case-sys )
@   mark the beginning of a CASE...OF..ENDOF...ENDCASE structure.
@   Append the run-time semantics below to the current definition.
@ Run-time: ( -- )
@   Continue execution
@ : CASE
@   0           \ Stop marker for endcase fixup looping
@   2           \ For checking correct case structure
@   bal+        \ for checking complete definition
@ ; immediate compile-only
.int zero,two,balplus
.int exit

defword "OF",2,F_IMM | F_CON,of @ Core Ext
@ Compilation: ( C: -- of-sys )
@   Put of-sys onto the control stack.
@   Append the run-time semantics below to the current definition.
@   The semantics are incomplete until resolved by a consumer of of-sys such as ENDOF.
@ Run-time: ( x1 x2 -- | x1 )
@   If x1<>x2, discard discard x2 and continue at the location specified by the consumer of
@   of-sys (e.g. following the next ENDOF ). Otherwise discard both values and continue in line.
@ : OF
@   2 - if -22 throw then   \ Check control structure
@   postpone over
@   postpone =
@   postpone if
@   postpone drop
@   3                       \ For control structure checking
@ ; immediate compile-only
.int two,minus
.int zbranch,16
.int lit,-22
.int throw
.int lit,over,comma
.int lit,eq,comma
.int if
@.int lit,dots,comma
.int lit,drop,comma
.int three
.int exit

defword "ENDOF",5,F_IMM | F_CON,endof @ Core Ext
@ Compilation: ( C: case-sys1 of-sys -- case-sys2 )
@   Mark the end of the OF...ENDOF part of the case structure. The next location
@   for a transfer of control resovles the reference given by of-sys.
@   Append the run-time semantics below to the current definition.
@   Replace case-sys1 by case-sys2 to be resolved by ENDCASE.
@ Run-time: ( -- )
@   Continue execution at the location specified by the consumer of case-sys2.
@ : ENDOF
@   3 - if -22 throw then       \ Check control structure
@   postpone else               \ Fixup OF branch and lay down own branch orig
@   2                           \ For control structure checking
@ ; immediate compile-only
.int three,minus
.int zbranch,16
.int lit,-22
.int throw
.int else
.int two
.int exit

defword "ENDCASE",7,F_IMM | F_CON,endcase @ Core Ext
@ Compilation: ( C: case-sys -- )
@   Mark the end of CASE...OF...ENDOf...ENDCASE structure. Use case-sys to resolve
@   entire structure. Append the run-time semantics below to the current definition.
@ Run-time: ( x -- )
@   Discard the case selector x and continue execution
@ : ENDCASE
@   2 - if -22 throw then       \ Check control structure
@   bal-                        \ For definition completeness
@   postpone drop               \ In current definition remember to drop DUP'd case selector
@   begin                       \ Loop round fixing up ENDOF 'orig' branches
@       ?dup 0<>                \ while we haven't hit the CASE stop marker
@   while
@       postpone then           \ Fixup ENDCASE
@   repeat
@ ; immediate compile-only
.int two,minus
.int zbranch,16
.int lit,-22
.int throw
.int balminus
.int lit,drop,comma
.int qdup,zne
.int zbranch,16
.int then
.int branch,-24
.int exit

defword "BEGIN",5,F_IMM | F_CON,begin @ Core
@ Compilation: ( C: -- dest ) ( -- 0 )
@   Put the next location for a transfer of control, dest onto the (control) stack
@   Also put a flag (0) on the stack  for later control structure matching
@   Update balance for checking for premature compilation end.
@   Words like AGAIN or UNTIL will you the dest address to branch back to.
@   Run-time: continue execution
.int here
.int zero
.int balplus
.int exit

defword "AGAIN",5,F_IMM | F_CON,again @ Core
@ Compilation: ( C: dest 0 -- )
@   Append the run-time semantics given below to the current defintion, resolving the
@   backward reference dest. Also test the control structure flag placed by a previous BEGIN
@   Update balance for checking for premature compilation end.
@ Run-time: ( -- )
@   Continue execution at the location specified by 'dest'
@ : AGAIN if -22 throw then postpone branch here - , bal- ; immediate compile-only
.int zbranch,16
.int lit,-22
.int throw
.int lit,branch
.int comma
.int here              @ ( prevhere here )
.int minus
.int comma
.int balminus
.int exit

defword "UNTIL",5,F_IMM | F_CON,until @ Core
@ Compilation: ( C: dest 0 -- )
@   Append the run-time semantics given below to the current definition, resolving
@   the backward reference 'dest'.
@ Run-time: ( x -- )
@   If all bits of 'x' are zero, continue execution at the location specified by 'dest', otherwise
@   continue from the word after UNTIL.
@ : UNTIL if -22 throw then postpone 0branch here - , bal- ; immediate compile-only
.int zbranch,16
.int lit,-22
.int throw
.int lit,zbranch
.int comma
.int here
.int minus
.int comma
.int balminus
.int exit

defword "WHILE",5,F_IMM | F_CON,while @ Core
@ Compilation: ( C: dest -- orig dest ) ( 0 -- 0 1 )
@   Put the location of a new unresolved forward reference 'orig' onto the (control) stack,
@   under the existing 'dest'. Append the run-time semantics given below to the current definition.
@   Semantics are incomplete until 'orig' and 'dest' are resolved (e.g. by REPEAT)
@ Run-time: ( x -- )
@   If all bits of 'x' are zero continue execution at the location specified by the resolution of 'orig'.
@ : WHILE postpone if 2swap ; immediate compile-only 
.int if             @ \ Lay down orig
.int twoswap        @ \ place dest left by BEGIN on top
.int exit

defword "REPEAT",6,F_IMM | F_CON,repeat @ Core
@ Compilation:  @ ( C: orig dest -- ) ( 0 1 -- )
@   Append the run-time semantics below to the current definition, resolving the backward
@   reference 'dest'. Resolve the forward reference 'orig' using the location following
@   the appended run-time semantics.
@   Run-time: ( -- )
@   Continue execution at the location given by 'dest'.
@ : REPEAT postpone again postpone then ; immediate compile-only
.int again          @ \ Fix up branch back to BEGIN 'dest'
.int then           @ \ Fix up branch from 'orig'
.int exit

defword "(DO)",4,,pdo @ Internal
@ Routine compiled into definirion by DO
@ to implement DO functionality
.int tor                    @ ( limit start -- limit ) ( R: start )
.int maxneg,plus
.int rto,over,minus         @ ( limit+maxneg start-limit-maxneg) ( R: )
.int rto                    @ Get stacked FIP,swap and
.int swap,tor               @ put start on stack
.int swap,tor               @ Put limit on stack
.int tor                    @ Put back stacked FIP
.int exit

defword "?DO",3,F_IMM | F_CON,qdo @ Core Ext
@ Compilation: ( C: -- do-sys )
@   Put do-sys onto the control flow stack. Append the run-time semantics below to
@   the current definition.
@ Run-time: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
@   if n1|u1 is equal to to n2|u2, continue execution at the location given by the consumer
@   of do-sys. Otherwise set up loop control parameters with index n2|u2 and limit n1|u1
@   and continue executing immediately following ?DO.
@ : ?DO
@   postpone 2dup
@   postpone <>
@   postpone if
@   postpone do
@   nip 1 swap
@ ; immediate compile-only
.int lit,twodup,comma
.int lit,neq,comma
.int if
.int do
.int nip,one,swap           @ Replace 0 of do with orig-type of 1 for ?do
.int exit

defword "DO",2,F_IMM | F_CON,do @ Core
@ Compilation: ( C: -- do-sys )
@   Place do-sys onto the (control) stack. Append the run time semantics given below to
@   the current definition. The semantics are incomplete until resolved by a do-sys
@   consumer (such as LOOP)
@ Run-time: (n1|u1 n2|u2 -- ) ( R: -- loop-sys )
@   Set up loop control parameters with index n2|u2 and limit n1|u1.
@ Rationale: Typical use
@   : X ... limit first DO ... LOOP ;
@   : X ... limit first DO ... step +LOOP ;
@ Definition:
@ : DO 
@   postpone (do)
@   rakevar @
@   0 rakevar !
@   0                       @ For control structure matching
@   here                    @ Leave dest for branching back to
@   bal+                    @ for definition checking
@ ; immediate compile-only
.int lit,pdo,comma          @ Compile (do) into definition in which DO appears
.int rakevar,fetch          @ Keep current rakevar (Restored by LOOP)
.int zero,rakevar,store
.int zero                   @ At compile time leave orig-type of 0 (DO)
.int here                   @ and leave address of dictionary pointer
.int balplus
.int exit

defcode "(LOOP)",6,,ploop
ldr r0,[RSP,#4]                 @ Load index - remember this is index - limit -maxneg (somthing like 7FFFFFFE)
adds r0,r0,#1                   @ Increment index
str r0,[RSP,#4]                 @ Store it back in return stack
ldrvc r1,[FIP]                  @ If we didn't set overflow, load the offset back to DO
addvc FIP,FIP,r1                @ Add it to the FIP
addvs FIP,FIP,#4                @ Otherwise jump over offset to continue
addvs RSP,RSP,#8                @ and drop the index/limit by bumping up the return stack pointer
next

defword "LOOP",4,F_IMM | F_CON,loop @ Core
@ Compilation: ( C: do-sys -- )
@   Append the run-time semantics below to the current definition. Resolve the destination
@   of all unresolved occurrences of LEAVE between the location given by do-sys and the next
@   location for a transfer of control.
@ Run-time: ( -- ) ( R: loop-sys1 -- | loop-sys2 )
@   Add one to the loop index. If the loop index is then equal to the loop limit, discard the
@   loop parameters and continue execution immediately following the loop. Otherwise continue
@   execution at the beginning of the loop.
@ : LOOP
@   postpone (loop)
@   rake                        @ Rake up LEAVEs
@   rakevar !                   @ put back rakevar stacked by DO
@ ; immediate compile-only
.int lit,ploop,comma
.int rake
.int rakevar,store              @ Put back stacked rakevar
.int exit

defword "UNLOOP",6,F_CON,unloop @ Core
@ Execution: ( -- ) ( R: loop-sys -- )
@   Discard the loop-control parameters for the current nesting level. An UNLOOP
@   is required for each nesting level before the definition may be exited.
@ : UNLOOP
@   r> r> r>                    @ ( -- fip limit start )
@   2drop >r                    @ \ Discard limit and start, push back FIP onto return stack
@   ; compile-only
.int rto,rto,rto                @ Unstack FIP, limit and start
.int twodrop,tor                @ Get rid of all except FIP
.int exit

defcode "(+LOOP)",7,,pplusloop
popdsp r2                       @ Get the increment
ldr r0,[RSP,#4]                 @ Get the index
adds r0,r0,r2                   @ Add the index and increment
str r0,[RSP,#4]                 @ Put it back on return stack
ldrvc r1,[FIP]                  @ If there is no overflow (not hit limit yet) load the offset back to DO
addvc FIP,FIP,r1                @ by adding offset to FIP
addvs FIP,FIP,#4                @ Otherwise jump over offset to rest of code
addvs RSP,RSP,#8                @ and drop the index/limit from the return stack
next

defword "+LOOP",5,F_IMM | F_CON,plusloop @ Core
@ Compilation: ( C: do-sys -- )
@   Append the run-time semantics below to the ucrrent definition. Resolve the destination
@   of all unresolved occurrences of LEAVE between the location given by do-sys and the next
@   location for a transfer of control.
@ Run-time: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
@   Add n to the loop index. If the loop index did not cross the boundary between loop limit
@   minus one and the loop limit, continue execution at the beginning of the loop. Otherwise
@   discard the current loop control paramaters and continue execution immediately after the loop
@ : +LOOP
@   postpone (+loop)
@   rake                        @ Rake up leaves
@   rakevar !                   @ Put back rakevar that was stacked by DO
@ ; immediate compile-only
.int lit,pplusloop,comma
.int rake
.int rakevar,store              @ Put back stacked rakevar
.int exit

defword "LEAVE",5,F_IMM | F_CON,leave @ Core
@ Execution: ( -- ) ( R: loop-sys -- )
@   Discard the current loop control parameters. Continue execution immediately following the
@   innermost syntactically enclosing DO...LOOP or DO...+LOOP
@ : LEAVE
@   postpone unloop                     \ Compile UNLOOP into current definition
@   postpone branch                     \ Compile in a branch
@   here                                \ Leave the address of where we are in definition
@   rakevar dup @ ,                     \ Compile in rakevar contents (which links to previous leave or is 0)
@   !                                   \ Store address of where we are into rakevar, for next leave.
@ ; immediate compile-only
.int lit,unloop,comma
.int lit,branch,comma
.int here,rakevar
.int dup,fetch,comma
.int store
.int exit

defword "RAKE",4,F_CON,rake
@ Walk back down the chain of addresses pointed to be rakevar so we can fill in
@ the unconditional branch addresses left by LEAVE                         
@ : RAKE ( cs-check do-loc -- )
@   here - ,                ( cs-check ) \ Compile offset back to DO
@   rakevar @               ( cs-check rake-addr ) \ Get start of LEAVE linked list
@   begin
@       dup 0>              \ While not at end of list
@   while
@       dup @               \ ( leave-addr prev-leave-addr )
@       here rot            \ ( prev-leave-addr here leave-addr )
@       swap over -         \ ( prev-leave-addr leave-addr )
@       swap !              \ (
@   repeat
@   rakevar !               \ ( cs-check )
@   ?dup if
@       drop -rot
@       postpone else
@       postpone 2drop
@       postpone then
@   then
@   bal-
@ ; compile-only
.int here,minus,comma     @ ( 0 | 1 ) Compile in offset back to DO
.int rakevar,fetch          @ ( 0 | 1; addr )
.int dup,zgt                @ ( 0 | 1; addr flag )
.int zbranch,48
.int dup,fetch
.int here
.int rot
.int swap,over,minus
.int swap,store
.int branch,-56
.int rakevar,store          @ ( 0 | 1 )
.int qdup
.int zbranch,32
.int drop,nrot
.int else
.int lit,twodrop,comma
.int then
.int balminus
.int exit

defcode "I",1,F_CON,i @ Core
@ ( -- n|u ) ( r: loop-sys -- loops-sys )
@ n|u is a copy of current (innermost) loop index.
ldr r0,[RSP]
ldr r1,[RSP,#4]
add r0,r0,r1
pushdsp r0
next 

defcode "J",1,F_CON,j @ Core
@ ( -- n|u ) ( r: loop-sys1 loop-sys2 -- loops-sys1 loop-sys2 )
@ n|u is a copy of next (outer) loop index.
ldr r0,[RSP,#8]
ldr r1,[RSP,#12]
add r0,r0,r1
pushdsp r0
next

defword "COMPILE,",8,F_CON,compilecomma @ Core Ext
@ Execution: ( xt -- )
@   Append the execution semantics of the definition represented by 'xt' to the
@   execution semantics of the current definition.
.int comma
.int exit

defword "[COMPILE]",9,F_IMM | F_CON,bcompile @ Core Ext
@ ( "<spaces>name<space>"-- )
@ OBSOLETE - retained for backward compatibility
@ Parse name, delimited by spaces, compile into ongoing definition
@ Used to compile an otherwise immediate word
@ Use POSTPONE instead
.int parsename                  @ ( c-addr u )
.int twodup,errword,twostore
.int searchword                 @ ( c-addr u 0 | xt f 1 | xt f -1 )
.int qdup,zeq
.int zbranch,16
.int lit,-13
.int throw
.int twodrop
.int comma
.int exit

defword "POSTPONE",8,F_IMM | F_CON,postpone @ Core
@ Compilation ( "spaces<name>" -- )
@   Skip leading space delimiters. Parse name delimited by a space.
@   Find name. append the compilation semantics of name to the current definition.
@ Rationale:
@   Used when defining immediate words to delay the compilation of words until
@   the immediate word is actually being used in another definition.
@   For example POSTPONE 0branch has the effect of compiling
@   lit,0branch,comma into the immediate word so that when that immediate word
@   is used in a definition just 0branch is compiled in.
@   So IF can be defined as  : IF postpone 0branch here 0 , 1 bal+ ; immediate
@   When IF is used in another definition it compiles in a 0branch, pushes the
@   address (orig) which will be resolved by a following THEN or ELSE, compiles
@   in a dummy 0 orig address then pushes a 1 on the stack and updates bal:
@   both used for control structure matching.
.int ptick                  @ ( xt -1 | xt 1 )
.int zlt
.int zbranch,36             @ If non-immediate word
.int lit                    @ compile in xt for lit 
.int lit
.int comma
.int comma                  @ compile in xt
.int lit
.int comma
.int comma                  @ and compile in ,
.int exit
.int comma                  @ if immediate word just compile in xt
.int exit

.ltorg
.global main
.func main
main:
stmfd sp!,{lr}
ldr r0,=var_sp0
sub DSP,DSP,#4      @ Add a safety zone
str DSP,[r0]
ldr RSP,=rs_top
ldr r0,=var_rp0
str RSP,[r0]
ldr r0,=ds
ldr r1,=value_here
str r0,[r1]
ldr FIP,=cold_start
next

cold_start:
.int lits, 15
.ascii "PHORTH (c) 2021"
.align 2
.int type
.int cr
.int quit

defcode ">R",2,F_CON,tor @ Core
@ Execution: ( x -- ) ( R: -- x )
@   Move x to the return stack
popdsp r0
pushrsp r0
next

defcode "R>",2,F_CON,rto @ Core
@ Execution: ( -- x ) ( R: x -- )
@   Move x from the return stack to the data stack
poprsp r0
pushdsp r0
next

defcode "2>R",3,F_CON,twotor @ Core Ext
@ ( x1 x2 -- ) ( R: -- x1 x2 )
@ Transfer cell pair x1 x2 to the return stack.
popdsp r0
popdsp r1
pushrsp r1
pushrsp r0
next

defcode "2R>",3,F_CON,tworto @ Core
@ ( -- x1 x2 ) ( R: x1 x2 -- )
@ Transfer cell pair x1 x2 from the return stack
@ onto the data stack.
poprsp r0
poprsp r1
pushdsp r1
pushdsp r0
next

defcode "R@",2,,rfetch @ Core
@ ( -- x ) ( r: x -- x )
@ Copy x from the return stack to the data stack.
ldr r0,[RSP]
pushdsp r0
next

defcode "2R@",3,,tworfetch @ Core Ext
@ ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
@ Copy cell pair x1 x2 from the return stack
ldr r0,[RSP]
ldr r1,[RSP,#4]
pushdsp r1
pushdsp r0
next

.ltorg

defword "RECURSE",7,F_IMM | F_CON,recurse @ Core
@ Append the execution semantics of the current definition to he current definition.
@ : RECURSE
@   noname @ not
@   if
@       latest >cfa ,
@   then
@ ;
.int noname,fetch,not
.int zbranch,12
.int latest,tcfa
.int comma
.int exit

defword "QUIT",4,,quit @ Core
@( -- ) ( R: i*x -- )
@ Empty the return stack, store zero in source-id, make the user input device the input source
@ and enter interpretation state. Repeat the following:
@ * Accept a line from the input source into the input buffer, set >IN to zero and interpret.
@ * Display the system prompt if in interpretation state, all processing has been completed and
@   no ambiguous condition exists.
@ : QUIT
@   begin
@       rp0 @ rp!                       \ Reset return stack
@       false noname !                  \ Sets default for noname var (we are not defining a noname)
@       tib                             \ Input line buffer
@       512                             \ Line size
@       sourcevar 2!                   \ Set up input buffer
@       0 to source-id                  \ Set source-id to zero (terminal input)
@       0 to bal                        \ Set control structure checking variable to 0
@       postpone [                      \ Set to interpretation state
@       begin
@           refill                          \ Fill up input buffer from terminal
@           drop                            \ Drop the result flag
@           ['] interpret                   \ Get xt of interpret
@           catch                           \ Set up catch for any subsequent throws from what is interpret'd
@           ?dup 0=
@       while                               \ Everything OK
@           state @
@           if                          \ If compiling
@               ." >>> "                 \ Prompt for continuation of definition
@           else
@               ." ok" cr               \ Otherwise ok prompt
@           then
@       repeat                          \ There was an error
@       perrword                    \ Print the word in error
@       dup -58 0 within            \ Is it one of the known errors?
@       if
@           abs 1- 2* cells         \ Work out offset into error table
@           throwtable +
@           2@ type cr              \ and print error message
@       else
@           ." Unknown exception #"
@           .
@       then
@   again
@ ;
.int rp0,fetch,rpstore
.int false,noname,store
.int lit,line_buffer
.int lit,line_size
.int sourcevar,twostore
.int zero
.int pto
.int value_sourceid
.int zero
.int pto
.int value_bal
.int lbrac
.int refill,drop
.int lit,interpret
.int catch
.int qdup
.int zeq
.int zbranch,64
.int state,fetch
.int zbranch,24
.int lits,4
.ascii ">>> "
.align 2
.int branch,16
.int lits,3
.ascii "ok\n"
.align 2
.int type
.int branch,-92 @-96
.int perrword
.int dup
.int lit,-58
.int zero
.int within
.int zbranch,48
.int abs,oneminus
.int twotimes
.int cells
.int throwtable,plus
.int twofetch
.int type
.int cr
.int branch,40
.int lits,19
.ascii "Unknown exception #"
.align 2
.int type
.int dot
.int branch,-288 @-292

defword "PERRWORD",8,,perrword
.int lits,4
.ascii ">>> "
.align 2
.int type
.int errword,twofetch
.int type
.int lits,4
.ascii " <<<"
.align 2
.int type
.int space
.int exit

defvar "INTERP-VECTOR",13,,interpvector,comma
.int execute
.int numbercomma
.int number
.int execute
.int execute

@ : INTERPRET ( -- )
@   begin
@       sp?                     \ Check stack pointer is within limits
@       parse-name              ( c-addr u) \ Get the next word
@       dup                     \ Duplicate length
@   while                       \ While length is not zero
@       2dup                    \ Copy addr and len
@       errword 2!              \ Store away in case of error
@       search-word             ( c-addr u 0 | xt f -1 | xt f 1 ) \ Look for the word in the dictionary
@                               \ Return 0 if not found or xt followed by compile-only flag and immediate flag
@       dup
@       if                      \ If the word was found
@           swap not            \ Put compile only flag top of stack
@           state @             \ Are we compiling or interpreting?
@           or 0=               \ Are we trying a compile only word while interpreting?
@           if
@               -14 throw
@           then
@       then
@                               \ will reach here with -1 (normal word), 1 (immediate word) 
@                               \ or 0 (unknown word or possible number) top of stack
@       1+ 2*                   \ The next set of calculations work out an offset into the interp-vector table
@       state @                 \ Are we interpreting or compiling?
@       1+ + cells               
@       interp-vector
@       + @                     \ Get xt of correct word to execute
@       execute                 
@   repeat
@   2drop
@;
defword "INTERPRET",9,,interpret
.int spq
.int parsename
.int dup
.int zbranch,128
.int twodup
.int errword,twostore
.int searchword
.int dup
.int zbranch,48
.int swap,not
.int state,fetch
.int or,zeq
.int zbranch,16
.int lit,-14
.int throw
.int oneplus
.int twotimes
.int state,fetch
.int oneplus,plus
.int cells
.int interpvector
.int plus,fetch
.int execute
.int branch,-140
.int twodrop
.int exit

defword "EVALUATE",8,,evaluate @ Core
@ ( i*x c-addr u -- j*x )
@ Save the current input source specification. Store -1 in source-id. Make the string
@ described the input source and input buffer. Set >IN to zero then interpret. When the
@ parse area is empty, restore the prior input source specification.
.int saveinput
.int tor,tor,tor,tor,tor    @ Save away on return stack
.int lit,-1
.int pto                @ Set source-id to -1, indicating string evaluation
.int value_sourceid
.int sourcevar,twostore @ Set source to address and length of string
.int zero,toin,store    @ Set input pointer to zero
.int interpret
.int rto,rto,rto,rto,rto    @ get back save-input parameters
.int restoreinput
.int drop               @ Drop the flag
.int exit

defword "SMUDGE",6,,smudge
.int latest     		@ get lfa of newly created word
.int cellplus  			@ get to name length word address
.int dup
.int fetch,f_smudge,xor	@ smudge it so we don't find it (stops accidental recursion)
.int swap,store
.int exit

defvar "NONAME",6,,noname,0

defword ":NONAME",7,,colnoname @ Core Ext
.int bal
.int zbranch,16
.int lit,-29
.int throw
.int here
.int docol,comma
.int rbrac
.int zero,rakevar,store
.int one
.int pto,value_bal
.int true,noname,store
.int minus1
.int exit

defword ":",1,,colon @ Core
@ ( C: "<spaces>name" -- colon-sys )
@ Skip leading space delimiters. Parse 'name' delimited by a space. 
@ Create a definition for 'name', called a "colon definition". 
@ Enter compilation state and start the current definition, producing 'colon-sys'.
@ Append the initiation semantics given below to the current definition.
@ The execution semantics of 'name' will be determined by the words compiled into
@ the body of the definition. The current definition is not findable in the dictionary
@ until it is ended (e.g. by ';')
@ : :
@   bal if -29 throw                \ Control structure mismatch
@   create                          \ Create header with standard cfa
@   smudge                          \ xor smudge bit in the nfa so this current definition can't be found
@   -1 cells allot                  \ Step back to the cfa
@   docol ,                         \ So we can compile in cfa that performs colon execution
@   ]                               \ Enter compile state
@   0 rakevar !                     \ Initilaise rakevar for any DO/LEAVES in definition
@   1 to bal                        \ Bump up bal
@   false noname !                  \ This is not a :NONAME definition
@   -1                              \ For control structure matching
@ ;
.int bal
.int zbranch,16
.int lit,-29
.int throw
.int create
.int smudge
.int minus1,cells,allot
.int docol,comma
.int rbrac
.int zero,rakevar,store
.int one
.int pto
.int value_bal
.int false,noname,store
.int minus1
.int exit


defword ";",1,F_IMM | F_CON,semicolon @ Core
@ Compilation: ( C: colon-sys -- )
@   Append the run-time semantics below to the current definition. End the current
@   definition, allow it to be found in the dictionary and enter interpretation state,
@   consuming colon-sys. If the data space is not aligned, reserve enough space to align it.
@ Run-time: ( -- ) ( R: nest-sys -- )
@ Return to the calling definition specified by nest-sys.
@ : ; 
@   bal 1-                      \ Check control structure matching
@   if -22 throw then
@   1+
@   if -22 throw then
@   postpone exit               \ Compile exit into current definition
@   [                           \ Go back to interpretation state
@   noname @ not                \ If not a noname
@   if smudge then              \ Smudge name field so definition can be found
@   false noname !
@   0 to bal
@ ; immediate compile-only
.int bal,oneminus
.int zbranch,16
.int lit,-22
.int throw
.int oneplus
.int zbranch,16
.int lit,-22
.int throw
.int lit,exit,comma @ end with exit so new word will return
.int lbrac          @ back to interpret mode
.int noname,fetch,not
.int zbranch,8
.int smudge
.int false,noname,store
.int zero
.int pto
.int value_bal
.int exit

defword ">NUMBER",7,,tonumber @ Core
@ ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
@ Converts digits in string at c-addr1 of length u1 using current BASE
@ ud1 accumulates those digits and results in ud2.
@ Conversion continues until the first non-digit
@ u2 contains number of unconverted characters
@ c-addr2 is the location of the first unconverted character
@ or first char past string.
@ : >NUMBER
@   begin
@       dup
@   while                               \ If u1 not already 0
@       >r dup
@       >r c@
@       dup 97 122                      \ a - z
@       within
@       if 223 and then                 \ mask to convert to upper case
@       dup 58 65
@       within
@       if drop r> r> exit then         \ Found non-digit
@       48 - 9 over <
@       7 and -
@       dup 0
@       base @
@       within
@       if
@           swap base @
@           um* drop
@           rot base @
@           um* d+
@           r> r>
@           1 /string
@       else
@           drop r> r>
@           exit
@       then
@   repeat
@ ;
.int dup
.int zbranch,272
.int tor,dup
.int tor,cfetch
.int dup,lit,97                     @ a
.int lit,122                        @ z
.int within
.int zbranch,16
.int lit,223                        @ Hex DF to mask out bit to convert to upper case
.int and
.int dup,lit,58
.int lit,65,within
.int zbranch,20
.int drop,rto,rto
.int exit
.int lit,48,minus
.int lit,9
.int over,lt
.int lit,7                      @ ( char A char 9 1+ - )
.int and,minus
.int dup,zero
.int base,fetch
.int within
.int zbranch,68
.int swap,base,fetch
.int umtimes,drop
.int rot,base,fetch
.int umtimes
.int dplus
.int rto,rto,one,sstring
.int branch,20
.int drop,rto,rto,exit
.int branch,-276
.int exit

@ Converts a string at c-addr of length u into a number
@ Returns just zero if it cannot do so
@ Returns a single number and 1 if a single number found
@ Returns a double number and 2 if a valid number string followed
@ by a period is found.
defword "(NUMBER?)",9,,pnumberq        @ ( c-addr u -- 0 | n 1 | d 2 )
.int zero,dup
.int twoswap
.int over,cfetch
.int lit,45                     @ ( minus sign )
.int eq,dup,tor
.int zbranch,12
.int one,sstring
.int tonumber
.int qdup
.int zbranch,88
.int oneminus
.int swap,cfetch
.int lit,46                     @ ( period )
.int xor
.int or
.int zbranch,24
.int twodrop,rto
.int drop,zero,exit
.int rto
.int zbranch,8
.int dnegate
.int two
.int branch,28
.int twodrop,rto
.int zbranch,8
.int negate
.int one
.int exit

defdoes "NUMBER?",7,,numberq,defer_dodoes    @ Vectored to point to (number?)
.int pnumberq

defword "(NUMBER)",8,,pnumber
.int numberq
.int dup,zeq
.int zbranch,16
.int lit,-13
.int throw
.int exit

defword "NUMBER",6,,number
@ ( c-addr u -- n | d )
@ Attempt to convert a string specified by c-addr u into a number.
@ If valid punctuation is found return d otherwise return n.
.int twodup,errword,twostore
.int pnumber,drop
.int exit

defword "NUMBER,",7,,numbercomma
.int pnumber
.int oneminus
.int zbranch,12
.int swap
.int literal
.int literal
.int exit

.ltorg

defword "(')",3,,ptick
@ ( "<spaces>name" -- xt -1 | xt 1 )
@ Skip leading space delimiters. Parse name delimited by a space. Find name and
@ return xt, the execution for name and -1 for a normal word or 1 for an immediate word.
@ : (')
@   bl word
@   dup count
@   errword 2!
@   find ?dup
@   if exit then
@   -13 throw
@ ;
.int bl,word
.int dup,count
.int errword,twostore
.int find
.int qdup
.int zbranch,8
.int exit
.int lit,-13
.int throw

defword "'",1,,tick @ Core
@ ( "<spaces>name" -- xt )
@ Skip leading space delimiters. Parse name delimited by a space. Find name and
@ return xt, the execution for name.
@ : ' 
@   (')                 \ Return xt and immediacy flag
@   drop                \ Drop flag
@ ;
.int ptick,drop
.int exit

defword "[']",3,F_IMM | F_CON,btick @ Core
@ Compilation: ( "<spaces>name" -- )
@   Skip leading space delimiters. Parse name delimited by a space.
@   Append the run-time semantics given below to the current definition
@ Run-time: ( -- xt )
@   Place name's execution token on the stack.
@ : ['] ' postpone literal ;
.int tick
.int literal
.int exit

defword "FIND",4,,find @ Core @ Search
@ ( c-addr -- c-addr 0 | xt 1 | xt -1 )
@ Attempt to find the word whose name is at counted string c-addr.
@ If not found, return address and 0
@ If found return executon token xt and 1 for an immediate word or -1 for a normal word.
@ : FIND
@   dup count           \ Obtain string address and count
@   searchword          \ ready for search-word to search all the wordlists in the search order
@   ?dup if             \ Did we find it?
@       nip rot drop    \ Leave xt -1|1
@   else
@       2drop 0         \ Leave c-addr 0
@   then
@ ;
.int dup,count          @ ( c-addr c-addr2 u )
.int searchword         @ ( c-addr c-addr2 u 0 | c-addr xt f -1 | 1 )
.int qdup
.int zbranch,20
.int nip,rot,drop       @ ( xt -1 | 1 )
.int exit
.int twodrop,zero
.int exit

.ltorg

defword "(SEARCH-WORDLIST)",17,,psearchwordlist
@ (c-addr u wid -- 0 | xt f -1 | xt f 1 )
@ Search dictionary for word at c-addr of length u in wordlist wid
@ Return c-addr u 0 if not found. If found, return execution token xt, compile-only flag f
@ and -1 or 1 (immediate)
@ : (SEARCH-WORDLIST)
@   @                           \ Get lfa of current word from wid
@   >r 2dup r>                  \ Dup c-addr u under the lfa
@   begin
@       dup dup >r              \ dup lfa and stash one away
@   while                       \ while the lfa is not zero
@       cell+                   \ Go to nfa length field
@       count                   \ Get name and count field
@       f_immed f_cmponly       \ Get immediate and compile bit masks
@       or invert and           \ Mask out those bits from the count
@       cmpnc                   \ Compare the name and the string (ignoring case)
@       0= if                   \ If 0, they compared
@           2drop               \ drop c-addr and u
@           r>                  \ Get back lfa of found word
@           dup cell+ @ dup     \ Get lfa length field and dup it
@           f_immed and         \ and with immediate bit mask
@           30 rshift           \ Convert bit to 1 or -1
@           1- swap
@           f_cmponly and       \ and with compilation only bit mask
@           0<> swap            \ Set up compilation only flag in stack
@           rot >cfa -rot       \ Convert lfa to cfa (xt)
@           exit                \ and drop out of definition
@       then
@       2dup r> @               \ Set up with previous lfa ready to loop round again
@   repeat
@   r> 2drop 2drop 2drop        \ Didn't find it so tidy up
@   0                           \ and leave 0 on stack
@ ;
.int fetch                  @ ( --- c-addr u lfa )
.int tor,twodup,rto         @ ( -- c-addr u c-addr u lfa )
.int dup,dup,tor            @ ( --- c-addr u c-addr u lfa lfa ) (R: lfa )
.int zbranch,156            @ ( while lfa is not zero )
.int cellplus               @ ( --- c-addr u c-addr u lfa+4) Get length field of nfa
.int count                  @ ( --- c-addr u c-addr u nfa len )
.int f_immed,f_cmponly,or   
.int invert,and             @ ( -- c-addr u c-addr u nfa len2 ) mask out immediate and compile-only bits
.int cmpnc,zeq              @ ( --- c-addr u flag )
.int zbranch,92             @ \ Have we found it?
.int twodrop                @ ( --- ) drop the c-addr and u, we have found it
.int rto                    @ ( --- lfa ) retrieve lfa of found word
.int dup,cellplus,fetch,dup @ ( --- lfa len len )
.int f_immed,and            @ ( --- lfa immed ) 0 if normal
.int lit,30,rshift          @ convert immed flag to 1 if immed or -1 if normal
.int one,minus              @ ( --- lfa len -1 | 1 )
.int swap                   @ ( --- lfa -1 | 1 len )
.int f_cmponly,and
.int zne,swap               @ ( --- lfa flag -1 | 1 )
.int rot,tcfa,nrot          @ ( --- xt flag )
.int exit
.int twodup                 @ ( --- c-addr u c-addr u )
.int rto                    @ ( --- c-addr u c-addr u lfa ) retrieve the lfa
.int fetch                  @ ( --- c-addr u c-addr u prevlfa ) Get the previous lfa in the dictionary
.int branch,-168            @ keep searching
.int rto,twodrop
.int twodrop,twodrop    @ ( --- ) didn't find it 
.int zero
.int exit

defword "SEARCH-WORDLIST",15,,searchwordlist @ Search
@ ( c-addr u wid -- 0 | xt -1 | xt 1 )
@ find the definition identified by the string c-addr u in the word list identified by wid. 
@ If the definition is not found, return 0. If the defintion is found, return its execution
@ token xt and 1 if the definition is immediate or -1 otherwise.
@ : SEARCH-WORDLIST
@   (search-wordlist)               \ Perform search-wordlist internal routine
@   dup if                          \ If found nip the compile-only flag
@       nip
@   then
@   ;
.int psearchwordlist
.int dup
.int zbranch,8
.int nip
.int exit

defword "SEARCH-WORD",11,,searchword @ (c-addr u -- c-addr u 0 | xt f -1 | xt f 1 )
@ : SEARCH-WORD
@   #ORDER @ DUP            ( How many wids? )
@   0<> IF
@       0
@       DO                  ( For each wid )
@           2DUP            ( c-addr u c-addr u )
@           I CELLS         ( offset into #order of the ith wid )
@           #ORDER CELL+
@           + @             ( c-addr u c-addr u wid )
@           (SEARCH-WORDLIST) ( c-addr u c-addr u 0 | xt f -1 | xt f 1 )
@           ?DUP
@           0<>
@           IF
@               >R 2SWAP    ( xt f c-addr u )
@               2DROP
@               R>
@               UNLOOP      ( we've found the word so exit
@               EXIT
@           THEN
@       LOOP
@       0
@   THEN ;
.int hashorder,fetch,dup
.int zbranch,92
.int zero
.int pdo
.int twodup                     @ ( c-addr u c-addr u )
.int i,cells
.int hashorder
.int cellplus
.int plus,fetch                 @ ( c-addr u c-addr u wid )
.int psearchwordlist            @ ( c-addr u; 0 | xt f -1 | xt f 1 )
.int qdup
.int zbranch,28
.int tor,twoswap
.int twodrop,rto
.int unloop
.int exit
.int ploop,-72
.int zero
.int exit

.ltorg
defcode "SEARCH",6,,search @ String
@ ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
@ Search the string specified by c-addr1 u1 for
@ the string specified by c-addr2 u2. If the flag is true
@ then a match was found at c-addr3 with u3 characters remaining.
@ If flag is false, there was no match and c-addr3 is c-addr1
@ and u3 is u1.

@ Adjust a character string at c-addr1 by n characters
@popdsp r3       @ u2
@popdsp r1       @ c-addr2
@popdsp r2       @ u1
@popdsp r0       @ c-addr1
@
@mov r6,#0           @ Pointer into c-addr1
@mov r7,#0           @ Pointer into c-addr2
@ldrb r4,[r0,r6]
@ldrb r5,[r1,r7]
@cmp r4,r5
@add r6,#1
@addeq r7,#1
@cmp r7,r3
@beq search_end


defcode ",",1,,comma @ Core
@ ( x -- )
@ Reserve one cell of dataspace and store x in it.
popdsp r0
ldr r1,=value_here
ldr r2,[r1]                 @ Get address of here
str r0,[r2],#4              @ Place value and increment the address
str r2,[r1]                 @ update the value HERE
next

defcode "C,",2,,ccomma @ Core
@ ( char -- )
@ Reserve space for one character in data space 
@ and store char in that space
popdsp r0
ldr r1,=value_here
ldr r2,[r1]
strb r0,[r2],#1
str r2,[r1]
next

defcode "CMOVE",5,,cmove    @ String
@ ( c-addr1 c-addr2 u -- )
@ Copies u characters from address c-addr1 to c-addr2.
@ The copy proceeds from lower to higher addresses.
popdsp r2   @ len
popdsp r1   @ dest
popdsp r0   @ source
_cmove:
cmp r2,#0
ldrgtb r3,[r0],#1
strgtb r3,[r1],#1
subgt r2,r2,#1
bgt _cmove
next

defcode "CMOVE>",6,,cmoveto     @ String
@ ( c-addr1 c-addr2 u -- )
@ Copies u characters from address c-addr1 to c-addr2.
@ The copy proceeds from higher to lower addresses.
popdsp r2   @ len
popdsp r1   @ dest
popdsp r0   @ source
sub r3,r2,#1
add r0,r3   @ point to end of source
add r1,r3   @ point to end of dest
_cmoveto:
cmp r2,#0
ldrgtb r3,[r0],#-1
strgtb r3,[r1],#-1
subgt r2,r2,#1
bgt _cmoveto
next

defcode "MOVE",4,,move @ Core
@ ( addr1 addr2 u -- )
@ Copy the contents of u consecutive address units at addr1 to the u consecutive address units
@ at addr2
popdsp r2   @ u - len
popdsp r1   @ addr2 - dest
popdsp r0   @ addr1 - source
bl _move
next

@ On entry r0 = source address, r1 = destination address, r2 = number of bytes to move
_move:
stmfd sp!,{lr}
cmp r0,r1           @ Is source higher in memory than dest?
bgt _move_forward  @ if so copy forward
sub r3,r2,#1        @ otherwise copy backwards
add r0,r3           @ point to end of source
add r1,r3           @ point to end of dest

_move_backward:
cmp r2,#0
ble _move_end
ldrb r3,[r0],#-1
strb r3,[r1],#-1
sub r2,r2,#1
b _move_backward

_move_forward:
cmp r2,#0
ble _move_end
ldrb r3,[r0],#1
strb r3,[r1],#1
sub r2,r2,#1
b _move_forward

_move_end:
ldmfd sp!,{pc}

defword ">UPPER",6,,toupper
@ ( char -- )
.int dup
.int oneplus
.int lit,97
.int lit,122
.int within
.int zbranch,16
.int lit,32
.int minus
.int exit

defword "STRING>UPPER",12,,stringtoupper
@ ( c-addr1 u -- c-addr2 u )
@ Converts a string at c-addr1 of length u to
@ an uppercase string at c-addr2.
.int tuck
.int strb,swap,cmove
.int dup,strb,swap
.int zero
.int pdo
.int dup,cfetch
.int toupper
.int over,cstore,oneplus
.int ploop,-28
.int drop
.int strb,swap
.int exit

defcode "COMPARE",7,,compare    @ String
@ ( c-addr1 u1 c-addr2 u2 -- flag )
@ Compares two strings at c-addr1 and c-addr2 with lengths
@ u1 and u2 respectively.
@ If u1 and u2 are equal then if the string at c-addr1 is
@ equal to the string at c-addr2, return 0.
@ If the lengths differ, compare the strings up to the smaller
@ length. If identical return -1 if u1<u2 and 1 if u1>u2.
@ If not identical return -1 if the string at c-addr1 is
@ less than the string at c-addr2 for a length of the shorter
@ string, otherwise return 1.
popdsp r3       @ len2
popdsp r1       @ addr2
popdsp r2       @ len1
popdsp r0       @ addr1
cmp r2,r3
movge r4,r3
movlt r4,r2     @ Put shorter length in r4
_compare:
ldrb r5,[r0],#1
ldrb r6,[r1],#1
cmp r5,r6
bne _compare_ne
subs r4,r4,#1
bne _compare

cmp r2,r3
moveq r7,#0
_compare_ne:
mvnlt r7,#0
movgt r7,#1
pushdsp r7
next 

defcode "COMPARE-NOCASE",14,,cmpnc
@ ( c-addr1 u1 c-addr2 u2 -- flag )
@ Compares two strings at c-addr1 and c-addr2 with lengths
@ u1 and u2 respectively.
@ If u1 and u2 are equal (ignoring case) then if the string at c-addr1 is
@ equal to the string at c-addr2, return 0.
@ If the lengths differ, compare the strings (ignoring case) up to the smaller
@ length. If identical return -1 if u1<u2 and 1 if u1>u2.
@ If not identical return -1 if the string at c-addr1 (ignoring case) is
@ less than the string at c-addr2 for a length of the shorter
@ string, otherwise return 1.
popdsp r3       @ len2
popdsp r1       @ addr2
popdsp r2       @ len1
popdsp r0       @ addr1
cmp r2,r3
movge r4,r3
movlt r4,r2     @ Put shorter length in r4
_cmpnc:
ldrb r5,[r0],#1
orr r5,#32      @2_0100000
ldrb r6,[r1],#1
orr r6,#32      @2_0100000
cmp r5,r6
bne _cmpnc_ne
subs r4,r4,#1
bne _cmpnc

cmp r2,r3
moveq r7,#0
_cmpnc_ne:
mvnlt r7,#0
movgt r7,#1
pushdsp r7
next 

defcode ">CFA",4,,tcfa      @ ( lfa -- cfa )
popdsp r0
add r0,r0,#4            @ Skip link field
ldr r1,[r0],#4          @ load and skip length field
and r1,r1,#F_LEN        @ Keep only length bits
add r0,r0,r1            @ Skip name field
add r0,r0,#3            @ Find next 4 byte boundary
and r0,r0,#~3 
pushdsp r0
next

defword ">BODY",5,,tobody @ Core
@ ( xt -- a-addr )
@ a-addr is the data-field address corresponding to xt.
.int cellplus
.int exit

.ltorg

defcode "EXECUTE",7,,execute @ Core
@ ( i*x xt -- j*x )
@ Remove xt from the stack and perform the semantics identified by it.
popdsp r0
ldr r1,[r0]     @ Get address in the CFA
bx r1           @ and branch to it

defword "SP?",3,,spq
.int depth,zlt
.int zbranch,16
.int lit,-4
.int throw
.int exit

defcode "CR",2,,cr @ Core
@ ( -- )
@ Cause subsequent output to appear at the begining of
@ the next line.
mov r0,#'\n'
bl putchar
next

defcode "LIT",3,,lit
@ Routine to push the following cell in a defintion to the stack and bump up the FIP to
@ continue execution after that cell.
ldr r0,[FIP],#4
pushdsp r0
next

defword "LITERAL",7,F_IMM | F_CON,literal @ Core
@ Compilation: ( x -- )
@   Append the runtime semantics given below to the current definition
@ Run-time: ( -- x )
@   Place on the stack
@ : LITERAL
@   postpone lit
@   ,
@ ; immediate
.int lit,lit,comma
.int comma
.int exit

defword "2LITERAL",8,F_IMM | F_CON,twoliteral @ Double
@ Compilation:
@ ( x1 x2 -- )
@ Run-time:
@ ( -- x1 x2 )
@ Place cell pair x1 x2 on the stack.
.int swap
.int literal
.int literal
.int exit

defcode "LITS",4,,lits
ldr r0,[FIP],#4         @ Read and skip past length word
pushdsp FIP             @ Push address of string onto stack
pushdsp r0              @ Push length of string onto stack
add FIP,FIP,r0          @ skip string and go to next word boundary
add FIP,FIP,#3
and FIP,FIP,#~3
next

defcode "\\",1,F_IMM,backslash @ Core Ext
@ Compilation:
@   Perform the execution semantics given below
@ Execution: ( "ccc<eol>" -- )
@   Parse and discard the remainder of the parse area.
ldr r0,=var_toin
mov r1,#line_size
str r1,[r0]
next

defword "(",1,F_IMM,comment @ Core
@ Compilation:
@   Perform the execution semantics given below
@ Execution: ( "ccc<paren>" -- )
@   Parse ccc delimited by )
@ : (
@   [ char] )
@   parse 2drop
@ ; immediate
.int lit,41     @ ascii for )
.int parse
.int twodrop
.int exit

defword ".(",2,F_IMM,dotcomment @ Core Ext
@ ("ccc<paren> -- )
@ Parse  and display ccc delimited by )
.int lit,41     @ ascii for )
.int parse
.int type
.int exit

defcode "KEY",3,,key @ Core
@ ( -- char )
@ Receive one character char.
bl setattr
_rekey:
bl getchar
cmp r0,#127
bgt _rekey
pushdsp r0
bl resetattr
next

defcode "EMIT",4,,emit @ Core
@ ( x -- )
@ Display a character x on the display device
popdsp r0
bl putchar
next

defword "CHAR",4,,char @ Core
@ ( "<spaces>name" -- char )
@ Skip leading space delimiters.
@ Parse name delimited by a space.
@ Put the value of the first char on the stack.
.int parsename
.int drop
.int cfetch
.int exit

defword "[CHAR]",6,F_IMM | F_CON,bchar @ Core
@ Compilation: ( "<spaces>name" -- )
@   Skip leading space delimiters. Parse name delimited by space.
@   Append the run time semnatics below.
@ Run-time: ( -- char )
@   Place char, the first character of name, onto the stack.
.int char
.int literal
.int exit

defcode "BL",2,,bl @ Core
mov r0,#' '
pushdsp r0
next

defword "SPACE",5,,space @ Core
@ ( -- )
@ Display one space
.int bl,emit
.int exit

defword "SPACES",6,,spaces @ Core
@ ( n -- )
@ If n is greater than zero, display n spaces.
.int dup
.int zgt
.int zbranch
.int 28
.int zero
.int pdo
.int space
.int ploop
.int -8
.int exit
.int drop
.int exit
@.int qdup
@.int zbranch  @24
@.word 2f - . 
@.int zero
@1:
@.int pdo
@.int space
@.int ploop @-8
@.word 1b-.+4 
@2:
@.int exit

defcode "FILL",4,,fill @ Core
@ ( c-addr u char -- )
@ If u is greater than zero store char in each of u consecutive characters of memory
@ beginning at c-addr
popdsp r2       @ Char to fill memory with
popdsp r1       @ Number of these chars
popdsp r0       @ Address of memory to be filled
bl _fill
next

_fill:  @ On entry r0 = address, r1 = count, r2 = char
stmfd sp!,{lr}
add r3,r0,r1
_fill_loop:
cmp r3,r0
strgtb r2,[r3,#-1]!
bgt _fill_loop
ldmfd sp!,{pc}

defword "ERASE",5,,erase @ Core Ext
@ ( addr u -- ) 
@ If u is greater than 0, clear all bits of u consecutive address units of memory beginning at addr.
.int zero
.int fill
.int exit

defword "BLANK",5,,blank @ String
@ ( c-addr u )
@ if u is greater than 0, fill u address units of memory starting at c-addr with ASCII blank (hex 20)
.int bl
.int fill
.int exit

defword "/STRING",7,,sstring @ String
@ ( c-addr1 u1 n -- c-addr2 u2 )
@ Adjust a character string at c-addr1 by n characters
.int dup,tor
.int minus
.int swap,rto
.int chars,plus
.int swap
.int exit



defword "ENVIRONMENT?",12,,environmentq @ Core
@ ( c-addr u -- false | i*x true )
@ c-addr is the address of a string and u is its count.
@ The string should be a word from the set defined by defenv
@ in the environment wordlist or future optional word sets.
.int lit,e_wordlist
.int searchwordlist
.int dup,tor
.int zbranch,8
.int execute
.int rto
.int exit

defword "SOURCE",6,,source @ Core
@ ( -- c-addr u )
@ c-addr is the address of, and u is the number of characters in, the input buffer.
.int sourcevar,twofetch
.int exit


defword "REFILL",6,,refill @ Core Ext @ File Ext
@ ( -- flag )
@ Attempt to fill the input buffer from the input source, returning a true flag is successful.
@ When the input source is the user input device, attempt to receive input into the terminal
@ input buffer. If successful, set >In to zero and return true.
@ When the input source is a string from EVALUATE, return false and perform no other action.
@ When the input is from a text file, attempt to read the next line from from the text-input
@ file. If successful, make the result the current input buffer, set >IN to zero and return true.
@ Otherwise return false.
@ : REFILL
@   source-id
@   -1 =
@   if false exit then          \ If evaluating
@   source
@   2dup 0 fill                 \ zero out input buffer
@   source-id
@   0=
@   if                          \ Accept from terminal
@       accept drop
@   else                        \ Coming from a file
@       source-id
@       read-line
@       drop nip false
@       =
@       if
@           false exit
@       then
@   then
@   0 >in !
@   true
@ ;
.int sourceid
.int lit,-1,eq
.int zbranch,12
.int false
.int exit
.int source                     @ ( c-addr len )
.int twodup
.int lit
.int 0
.int fill
.int sourceid
.int zero,eq
.int zbranch,20
.int accept                     @ source-id = 0 so accept from terminal
.int drop
.int branch,44
.int sourceid                   @ coming from a file
.int read_line                  @ ( len flag ior )
.int drop,nip,false
.int eq
.int zbranch,12
.int false
.int exit
.int zero
.int toin
.int store
.int true
.int exit

@ : ACCEPT      ( c-addr +n1 -- +n2 )
@ defword "ACCEPT",6,,accept
@ .int tor,zero
@ .int dup,rfetch,lt
@ .int zbranch,240
@ .int key
@ .int dup,lfhash,eq
@ .int zbranch,44
@ .int drop,twodup,plus
@ .int zero,swap,cstore
@ .int nip,rto,drop
@ .int exit
@ .int dup,bsphash,eq
@ .int over,delhash,eq
@ .int or
@ .int zbranch,68
@ .int over,zne
@ .int zbranch,40
@ .int drop,delhash,emit
@ .int space
@ .int delhash,emit,oneminus
@ .int branch,8
@ .int drop
@ .int branch,68
@ .int dup,bl,lt
@ .int zbranch,12
@ .int drop,bl
@ .int dup,tor,emit
@ .int twodup,plus
@ .int rto,swap,cstore
@ .int oneplus
@ .int branch,-252
@ .int rto,drop
@ .int twodup,plus
@ .int zero,swap,cstore
@ .int nip
@ .int exit

defcode "ACCEPT",6,,accept @ Core
@ ( c-addr +n1 -- +n2 )
@ Receives a string of at most +n1 chars.
@ +n2 is the length of the string stored at c-addr
popdsp r6       @ Max count
popdsp r4       @ Address to move line into
bl term_input   @ Read a line from keyboard - r0 contains address of buffer that was allocated
mov r5,r0       @ Store buffer address away for later free
mov r2,#0       @ set up for count
_readline_count:
ldrb r1,[r0],#1 @ Start getting bytes and incrementing count until we hit 0x00 terminator
cmp r1,#0
beq _readline_end
strneb r1,[r4],#1 @ Store char
addne r2,r2,#1
cmp r6,r2       @ compare with max length
bgt _readline_count
_readline_end:
pushdsp r2      @ Push count onto stack
next

defcode "RP!",3,,rpstore
@ ( addr -- )
@ Sets the return stack pointer to addr.
popdsp RSP
next

defcode "@",1,,fetch @ Core
@ ( a-addr -- x )
@ x is the value stored at a-addr
popdsp r1
ldr r0,[r1]
pushdsp r0
next

defcode "2@",2,,twofetch @ Core
@ ( a-addr - x1 x2 )
@ Fetch the cell pair x1 x2 stored at a-addr. x2 is stored at a-addr and x1 at the next
@ consecutive cell.
popdsp r0           @ addr
ldr r1,[r0],#4      @ x2
ldr r2,[r0]         @ x1
pushdsp r2
pushdsp r1
next

defcode "!",1,,store @ Core
@ ( x a-addr -- )
@ Store x at a-addr
popdsp r0   @ addr
popdsp r1   @ value
str r1,[r0]
next

defcode "2!",2,,twostore @ Core
@ ( x1 x2 a-addr -- )
@ Store the cell pair x1 x2 at the address a-addr, with x2 at a-addr and x1 at the 
@ next consecutive cell.
popdsp r0   @addr
popdsp r1   @ x2
popdsp r2   @ x1
str r1,[r0],#4
str r2,[r0]
next

defcode "+!",2,,plusstore @ Core
@ ( n|u a-addr -- )
@ Add n|u to the single-cell number at a-addr
popdsp r0           @ addr
popdsp r1           @ n
ldr r2,[r0]         @ Contents of addr
add r2,r2,r1        @ Contents + n
str r2,[r0]         @ Store result back in addr
next

defcode "C@",2,,cfetch @ Core
@ ( c-addr -- char )
popdsp r1
ldrb r0,[r1]
pushdsp r0
next

defcode "C!",2,,cstore @ Core
@ ( x c-addr )
@ Store low-order bits (corresponding to the char size)
@ of x at c-addr
popdsp r0   @ addr
popdsp r1   @ value
strb r1,[r0]
next

defcode "SP!",3,,spstore
@ ( addr -- )
@ Sets the stack pointer to addr.
popdsp r1
mov DSP,r1
next

defcode "BRANCH",6,,branch
ldr r1,[FIP]
add FIP,FIP,r1
next

defcode "0BRANCH",7,,zbranch
popdsp r0
cmp r0,#0
ldreq r1,[FIP]
addeq FIP,FIP,r1
addne FIP,FIP,#4
next

defword "ALLOT",5,,allot @ Core
@ ( n -- )
@ If n>0 reserve n address units of data space
@ if n<0 release |n| address units of data space.
.int here,plus
.int pto
.int value_here
.int exit

defword "BUFFER:",7,,buffercreate @ Core Ext
@ ( u "<spaces>name" -- )
@ Skip leading space delimiters. Parse name delimited by a space.
@ Create a definition for name with the execution semantics below.
@ Reserve u address units at an aligned address.
@ name execution: ( -- a-addr )
@ a-addr is the address of the space reserved by BUFFER:
.int create
.int allot
.int exit

defword "[",1,F_IMM,lbrac @ Core
@ Compilation: perform the exececution semantics below
@ Execution: ( -- )
@   Enter interpretation state
.int zero
.int state,store
.int exit

defword "]",1,,rbrac
.int true
.int state,store
.int exit

_dodoes:
@ r0 contains cfa of word to execute
@ Link register has address of what immediately follows bl to _dodoes
add r1,r0,#4            @ Point to pfa
pushdsp r1              @ and push onto stack
pushrsp FIP             @ Save away 
mov FIP,lr              @ Put address of high level routine into FIP
b _next

@ Dummy for debugging
defcode "DONOWT",6,,donowt
next

defword "(DOES>)",7,,bdoes
.int rto                @ Pop address of bl to dodoes that was compiled into defining word
.int noname,fetch,not
.int zbranch,12
.int latest,tcfa        @ Get cfa of word being defined
.int store              @ and store the address into cfa
.int exit


defword "DOES>",5,F_IMM | F_CON,doesto  @ Core
@ Compilation:
@ ( C: colon-sys1 -- colon-sys2 )
@ Append the run time semantics below
@ ( -- ) ( R: nest-sys1 )
.int lit,bdoes,comma    @ Compile bdoes into new defining word
.int lit,0xeb000000     @ bl opcode
.int lit,_dodoes        @ Address of _dodoes routine
.int here               @ Get where we are in compilation of defining word
.int minus              @ Now work out offset of _dodoes from here
.int lit,8              @ Adjust for pc offset
.int minus
.int four               @ Divide by 4 as bl address is word aligned
.int slash
.int lit,0x00ffffff     @ Mask out opcode bits (in case it's a negative offset)
.int and
.int or                 @ combine opcode and offset
.int comma              @ and compile into defining word
.int exit

defword "CREATE",6,,create @ Core
@ ( "<spaces>name" -- )
@ Skip leading space delimiters. Parse name delimited by a space. Create a definition
@ for 'name' with the execution semantics below. If the data space pointer is not aligned,
@ reserve enough space to align it. The new data space pointer defines 'name's data field.
@ Create does not allocate any space in 'name's data field.
@ 'name' execution ( -- a-addr )
@   'a-addr' is the address of 'name's data field. The execution semantics of name may be
@   extended by using DOES>
@ : CREATE
@   align latest        \ Align the dataspace pointer and get the lfa of the latest definition
@   here !              \ Store into new words lfa
@   here current @ !    \ Update current wordlist
@   1 cells allot       \ and allocate space for lfa
@   parse-name          \ Parse the word leaving c-addr u
@   dup ,               \ Compile in length part of the name field
@   tuck                ( u c-addr u )
@   here swap           ( u c-addr-from c-addr-to u ) \ Set up for move
@   move                \ move the string into dataspace
@   allot align         \ Reserve space for the moved string in the name field
@   (dodoes) ,          \ Compile in code to to a basic DOES (i.e. put pfa onto stack)
@ ;
.int align
.int latest
.int here
.int store              
.int here
.int current,fetch,store 
.int one,cells,allot    
.int parsename          
.int dup,comma         
.int tuck             
.int here,swap       
.int move           
.int allot
.int align
.int bdodoes
.int comma
.int exit

defword "CHAR+",5,,charplus          @ Core
@ ( c-addr1 -- c-addr2 )
.int oneplus
.int exit

defword "CHARS",5,,chars @ Core
@ ( n1 -- n2 )
@ Returns the size in address-units of n1 chars
.int exit

defword "CELL+",5,,cellplus @ Core
.int four,plus
.int exit

defword "CELLS",5,,cells @ Core
.int four
.int times
.int exit

defconst "(DODOES)",8,,bdodoes,var_dodoes
defconst "_DODOES",7,,udodoes,_dodoes

defword "VARIABLE",8,,variable @ Core
@ ( "<spaces>name" -- )
@ Skip leading space delimiters. Parse name delimited by a space. Create a definition
@ for name with the execution semantics defined below. Reserve one cell of data space at an
@ aligned address.
@ name Execution: ( -- a-addr )
@   a-addr is the address of the reserved cell.
@ : VARIABLE
@   create 1 cells allot                  \ Create the definition and reserve space
@   does>
@ ;
.int create
.int one,cells,allot
.int bdoes
var_dodoes:
bl _dodoes
.int exit

defword "2VARIABLE",9,,twovariable @ Double
@ ( "<spaces>"name" -- )
@ Skip leading space delimiters. Parse name delimited by a space.
@ Create a definition for name with the execution semantics
@ defined below.
@ name Execution:
@ ( -- a-addr )
@ a-addr is the address of the first (lowest address) cell
@ of two consecutive cells in data space.
.int create
.int lit,8
.int allot
.int exit

defword "CONSTANT",8,,constant @ Core
@ ( x "<spaces>name" -- )
@ Skip leading spaces. Parse name delimited by a space.
@ Create a definition for name  with exection semantics
@ ( -- x )
.int create,comma
.int bdoes
const_dodoes:
bl _dodoes
.int fetch
.int exit

defword "2CONSTANT",9,,twoconstant @ Double
@ ( x1 x2 "<spaces>name" -- )
@ Skip leading spaces. Parse name delimited by a space.
@ Create a definition for name with ther execution semantics below.
@ name Execution:
@ ( -- x1 x2 )
@ Place cell pair x1 x2 on the stack.
.int create
.int comma,comma
.int bdoes
bl _dodoes
.int twofetch
.int exit

defword "VALUE",5,,value @ Core Ext
@ ( x "<spaces>name" -- )
@ Skip leading space delimiters. Parse name delimited by a space. Create a definition for name
@ with the execution semantics defined below, with an inital value equal to x.
@ name Execution: ( -- x )
@   Place x on the stack. The value of x is that given when name was created, until the phrase
@   x TO name is executed, causing a new value of x to be assigned to name.
@ TO name Run-time: ( x -- )
@   Assign the value x to name.
@ : value
@   create ,                    \ Create definition and compile in initial value
@   does>                       \ Upon execution
@       @                       \ Fetch the current
@ ;
.int create
.int comma
.int bdoes
val_dodoes:
bl _dodoes
.int fetch
.int exit

defcode "(TO)",4,,pto
@ This will have been compiled into a definition by TO, looking something like
@ : test 1 to bal ;
@ This definitiion will have a structure like:
@ +----------------------------+-----+---+-------------+-------------+------+
@ | header for test definition | lit | 1 | xt for (TO) | pfa for bal | exit |
@ +----------------------------+-----+---+-------------+-------------+------+
@ When execution hits the xt for (TO), 1 will be on the stack and the FIP will
@ point to next cell which contains the pfa of BAL (where the value will be stored).
@ This address will be loaded into r1 and the FIP incremented to jump past the pfa.
@ Finally the value is stored into the pfa of bal. 
popdsp r0
ldr r1,[FIP],#4
str r0,[r1]
next

defword "TO",2,F_IMM,to @ Core Ext
@ Interpretation: ( i*x "<spaces>name" -- )
@   Skip leading spaces and parse name delimited by a space. Perform "TO name run-time"
@   semantics given in the definition for the defining word of name.
@ Compilation: ( "<spaces>name" -- )
@   Skip leading spaces and parse name delimited by a space. Append the "TO name run-time"
@   semantics given in the definition for the defining word of name to the current definition.
@ : TO
@   '                       \ Parse the name
@   dup @                   ( xt cfa) \ dups xt and leaves the address of the code pointed to by that xt
@   doval                   \ Address of the dovalue code
@   <> if -32 throw then    \ If cfa and doval aren't the same it wasn't created bu VALUE
@   state @
@   if                      \ If compiling
@       postpone (TO)       \ Compile in code to handle TO in definition (see (TO))
@       cell+ ,             \ Compile in pfa of the VALUE
@   else
@       cell+ !             \ If interpreting, directly store value in the VALUE.
@   then
@ ;
.int tick,dup,fetch
.int doval
.int neq
.int zbranch,16
.int lit,-32
.int throw
.int state,fetch
.int zbranch,32
.int lit,pto,comma
.int cellplus,comma
.int branch,12
.int cellplus,store
.int exit

defcode "TYPE",4,,type @ Core
@ ( c-addr u -- )
@ If u is greater than zero, display the string specified
@ by c-addr and u.
popdsp r1       @ length
popdsp r0       @ address
bl _type
next

_type:
stmfd sp!,{r4-r5,lr}
mov r4,r0       @address
mov r5,r1       @length
_type1:
subs r5,r5,#1
ldrgeb r0,[r4],#1
blge putchar
bge _type1
ldmfd sp!,{r4-r5,pc}  

defword "MS",2,,ms              @ ( +n -- )
.int lit,1000
.int times
.int microsec
.int exit

defcode "MICROSEC",8,,microsec  @ ( +n -- )
popdsp r0
bl usleep
next

@ defcode "PARSE",5,,parse @ Core Ext
@ @ ( char "ccc<char>" -- c-addr u )
@ popdsp r0       @ delimiter
@ bl _parse
@ pushdsp r0      @ address
@ pushdsp r1      @ length
@ next

defword "PARSE",5,,parse @ Core Ext
@ ( char "ccc<char>" -- c-addr u )
@ Parse ccc delimited by char
@ Parses following word in input stream, delimited by char
@ Returns address and length in temp storage
.int source,toin,fetch,sstring          @ \ Adjust string start by >in
.int over,tor                           @ \ Stash away of start of word
.int rot,tor                            @ \ Store away delimiter char
.int dup                                @  
.int zbranch,44
.int over,cfetch,rfetch
.int neq
.int zbranch,20
.int one,sstring
.int branch,-48
.int rto,drop
.int drop,rto
.int tuck,minus
.int dup,toin
.int fetch,plus,oneplus
.int toin,store
.int exit

defword "ISSPACE?",8,,isspaceq
@ ( c -- f )
@ \ Is it a whitespace delimiter (anything with ascii code of space or less
.int bl,oneplus,ult
.int exit

defword "ISNOTSPACE?",11,,isnotspaceq
@ ( c -- f)
@ \ Is it not a whitespace delimiter
.int isspaceq
.int zeq
.int exit

defword "XT-SKIP",7,,xtskip
@ ( addr1 u2 xt -- addr2 u2)
@ \ Skip all characters satisfying xt ( c -- f )
.int tor
.int dup
.int zbranch,44
.int over,cfetch
.int rfetch,execute
.int zbranch,20
.int one,sstring
.int branch,-48
.int rto,drop
.int exit

defword "PARSE-NAME",10,,parsename @ Core Ext
@ (" <spaces>name<space>" -- c-addr u )
@ Skip leading spaces and parse name delimited by a space
@ c-addr is the address within the input buffer and u it's length
@ If parse area is empty of contain only spaces, u is zero
.int source,toin,fetch,sstring
.int lit,isspaceq
.int xtskip,over,tor
.int lit,isnotspaceq,xtskip
.int twodup,one,min,plus
.int source,drop,minus,toin,store
.int drop,rto,tuck,minus
.int exit

@ defcode "PARSE-NAME",10,,parsename
@ @ (" <spaces>name<space>" -- c-addr u )
@ @ Skip leading spaces and parse name delimited by a space
@ @ c-addr is the address within the input buffer and u it's length
@ @ If parse area is empty of contain only spaces, u is zero
@ mov r0,#32          @ Delimiter is space
@ bl _skipleading
@ bl _parse
@ pushdsp r0
@ pushdsp r1
@ next

@ The purpose of this function is to skip leading delimiters
@ in the input source (offset by >IN), updating >IN to point
@ to the first non-delimiter or to the terminating null.
@ On entry: r0 = the delimiter
@ On exit:  >IN is updated to point to the first non-delimiter or
@           terminating null
_skipleading:
stmfd sp!,{r4,r5,r6,lr}
@ work out where in line buffer we are starting from
ldr r1,=var_sourcevar
ldr r2,[r1],#4       @ Load source length
ldr r3,[r1]          @ Load address of source
add r4,r2,r3         @ address of end of input
ldr r1,=var_toin
ldr r5,[r1]
add r5,r5,r3         @ r5 now points to current position in input buffer
ldrb r6,[r5],#1
_skiploop:
cmp r6,r0
ldreqb r6,[r5],#1
beq _skiploop
@ r5 now holds position of first non-delimiter
subs r5,r5,r3       @ Now r5 holds the new >IN offset
subs r5,r5,#1
str r5,[r1]
ldmfd sp!,{r4,r5,r6,pc}

@ The purpose of this functions is to scan the input source
@ starting at >IN for a word delimited by the character in r0
@ On entry: r0 = the delimiter
@ On exit:  r0 = address of input storage holding word
@           r1 = count of chars in word
_parse:
stmfd sp!,{r4,r5,r6,r7,r8,r9,lr}
@ work out where in line buffer we are starting from
ldr r1,=var_sourcevar
ldr r2,[r1],#4      @ Load source length
ldr r3,[r1]         @ Load address of source
add r4,r2,r3        @ address of end of input
ldr r1,=var_toin
ldr r5,[r1]
add r5,r5,r3    @ r5 now points to current position in input buffer
mov r6,r5       @ Store this away
mov r8,#0       @ Set up for a count of characters
cmp r5,r4
bgt _parse_eol
ldrb r7,[r5],#1
_parse_loop:
cmp r7,r0
cmpne r7,#0
addne r8,#1
ldrneb r7,[r5],#1
bne _parse_loop

ldr r9,[r1]         @ Update >in
add r9,r9,r8
cmp r7,#0
addne r9,r9,#1		@ Point to char after delimiter
str r9,[r1]
_parse_eol:
mov r0,r6
mov r1,r8

ldmfd sp!,{r4,r5,r6,r7,r8,r9,pc}

defcode "WORD",4,,word @ Core
@ ( char "<chars>ccc<char>" -- c-addr )
@ Skip leading delimiters. Parse characters ccc delimited by char.
@ Returns c-addr, the address of a transient region containing the parsed
@ word as a counted string.
popdsp r0
bl _skipleading     @ Updates >IN
bl _parse           @ Returns r0 = address of word, r1 = count
mov r2,r1           @ Set up for move
ldr r1,=word_buffer @ Destination address
mov r4,r1           @ Keep destination address
str r2,[r1],#4      @ Store count
bl _move
pushdsp r4
next    

defword "COUNT",5,,count @ Core
@ ( c-addr1 -- c-addr2 u )
@ Takes a counted string stored at c-addr2 and returns
@ the address of the first char after c-addr1 and the
@ length of the string, u, stored at c-addr1.
.int dup               @ ( --- c-addr c-addr )
.int cellplus          @ ( --- c-addr nfa )
.int swap
.int fetch
.int exit

@ Converts a string and count into a C-style null terminated string
@ On entry r0 = address of string, r1 = length of string
@ On exit r0 = address of null terminated string
_cstring:
stmfd sp!,{r4,r5,lr}
mov r2,r1
mov r4,r2
ldr r3,=hold_start
mov r5,r3
mov r1,r3
bl _move
mov r3,r5
add r3,r3,r4
mov r0,#0
str r0,[r3]
mov r0,r5
ldmfd sp!,{r4,r5,pc}

defcode "ASCIIZ",6,,asciiz
popdsp r1
popdsp r0
bl _cstring
pushdsp r0
next

defcode "OPEN-FILE",9,,openfile @ File
@ ( c-addr u fam -- fileid ior )
@ Open the file named by c-addr u with the file access method
@ specified by fam.
popdsp r2           @ file access method
mov r4,r2           @ Save away
popdsp r1           @ length
popdsp r0           @ address of string
bl _cstring         @ Convert to a C-style 0x00 delimited string
mov r1,r4           @ Put back file access method
bl fopen
pushdsp r0
mov r1,#0
cmp r0,#0
bleq __errno_location
ldreq r1,[r0]
pushdsp r1
next

defcode "CLOSE-FILE",10,,closefile @ File
@ ( fileid -- ior )
@ Close the file identified by fileid. ior is the
@ impementation defined I/O result code.
popdsp r0
bl fclose
mov r1,#0
cmp r0,#0
blne __errno_location
ldrne r1,[r0]
pushdsp r1
next

defcode "READ-FILE",9,,readfile     @ ( c-addr u1 fileid -- u2 ior )
@ Read u1 consecutive characters from file identified by fileid
@ storing them at c-addr.
@ If successful return ior = 0 and u2 = number of chars read
@ If unsuccessful ior is not-zero
popdsp r3           @ file id
popdsp r2           @ count
popdsp r0           @ buffer
mov r1,#1
bl fread
pushdsp r0
bl __errno_location
ldr r1,[r0]
pushdsp r1
next

defcode "GETLINE",7,,getline        @ ( c-addr u1 fileid -- u2 ior )
bl __errno_location @ Reset errno to zero
mov r1,#0
str r1,[r0]
popdsp r2           @ fileid
popdsp r1           @ length
add r1,r1,#1        @ Add one as fgets reads up to one less than given length
popdsp r0           @ address of buffer
mov r4,r0           @ Save address
mov r5,r2           @ Save fileid 
mov r0,r2
bl fileno
cmp r0,#-1
beq _getline_err
mov r2,r5
mov r0,r4
bl fgets
cmp r0,#0           @ 0 indicates end-of-file or error
bleq __errno_location
ldreq r1,[r0]
moveq r2,#0
beq _getline_exit
mov r2,#'\n'
mov r0,r4           @ Restore address
_findcr:
ldrb r3,[r0],#1
cmp r2,r3
cmpne r3,#0         @ null? That is we have reached 'len' bytes without hitting eol
bne _findcr
@ r0 now points to one after cr (or null)
sub r2,r0,r4        @ work out length of input
sub r2,r2,#1         @ but ignore cr itself
mov r3,#0
str r3,[r0,#-1]     @ Overwrite cr with null
mov r0,r4
_getline_replacews: @ Replace whitespace with spaces
ldrb r3,[r0],#1
cmp r3,#0
beq _getline_exit
cmp r3,#32
movlt r5,#32
strltb r5,[r0,#-1]
b _getline_replacews
_getline_exit:
pushdsp r2
pushdsp r1
next
_getline_err:
pushdsp r0
bl __errno_location
ldr r1,[r0]
pushdsp r1
next

.ltorg

defword "READ-LINE",9,,read_line @ File
@ ( c-addr u1 fileid -- u2 flag ior )
@ Read one line of text from the file given by fileid
@ Read up to u1 consecutive characters storing them in c-addr
@ Terminate the read when a carriage return character is found
@ If no exception encountered ior = 0 and flag = true
@ and u2 contains the number of characters read
@ if file position = file size before executing read-line,
@ flag is false, ior = 0 and u2 = 0
.int dup,dup,filesize               @ ( fileid ud ior )
.int twodrop                          @ ( filesize )
.int swap,fileposition              @ ( ud ior )
.int twodrop
.int eq
.int zbranch,28
.int twodrop,drop
.int zero,zero,zero
.int exit
.int getline                        @ ( u2 ior )
.int dup                            @ ( u2 ior ior )
.int zbranch,16                     @ ior = 0, no error
.int false
.int swap                       
.int exit
.int true
.int swap
.int exit

defcode "SAVE-INPUT",10,,saveinput @ Core Ext
@ ( -- xn ... x1 n )
@ x1 through xn describe the current state of the input
@ source specification for later use by RESTORE-INPUT.
@ In this implementation we have 
@ ( -- source-addr source-len >in-addr source-id 4 )
ldr r0,=var_sourcevar
ldr r1,[r0],#4  @ Load source length
ldr r0,[r0]  @ Load address of source
pushdsp r0
pushdsp r1
ldr r0,=var_toin
ldr r0,[r0]
pushdsp r0
ldr r0,=value_sourceid
ldr r0,[r0]
pushdsp r0
mov r0, #4
pushdsp r0
next

defcode "RESTORE-INPUT",13,,restoreinput @ Core Ext
@ ( xn ... x1 n -- flag )
@ Attempt to restore the input spurce specification to the state described
@ by x1 thtough xn. flag is true if the input source cannot be so restored.
popdsp r0   @ Drop count on stack
popdsp r0
ldr r1,=value_sourceid
str r0,[r1]
popdsp r1
ldr r0,=var_toin
str r1,[r0]
popdsp r1
popdsp r2
ldr r0,=var_sourcevar
str r1,[r0],#4
str r2,[r0]
mov r0,#0
pushdsp r0
next

defword "INCLUDE-FILE",12,,includefile @ File
@ ( i*x file-id -- j*x)
@ Remove fileid from the stack. Save the current input
@ source specification including the value of source id.
@ Make the file specified by fileid the input source.
@ Repeat until end of file: read a line from the file,
@ fill the input buffer from that line, set >IN to zero
@ When the end of file is reached, close the file and
@ restore the input source specification.
@ and interpret.
.int saveinput              @ ( -- source-addr source-len >in-addr source-id 4 )
.int tor,tor,tor,tor,tor
.int pto
.int value_sourceid         @ Set source id to file id
.int fib,lit,fb_size        @ Redirect source to file input buffer
.int sourcevar,twostore
.int refill
.int zbranch,16
.int interpret
.int branch,-20
.int sourceid,closefile
.int drop
.int rto,rto,rto,rto,rto
.int restoreinput
.int drop                   @ Drop flag
.int exit

defword "INCLUDED",8,,included @ File
@ ( i*x c-addr u -- j*x )
@ Remove c-addr u from the stack. Save the current input
@ source specification including the value of source id.
@ Open the file specified by c-addr u obtaining a fileid.
@ Make the file specified by fileid the input source.
@ Repeat until end of file: read a line from the file,
@ fill the input buffer from that line, set >IN to zero
@ When the end of file is reached, close the file and
@ restore the input source specification.
@ and interpret.
.int lits,1
.ascii "r"
.align 2
.int drop
.int openfile
.int zbranch,16
.int lit,-38
.int throw
.int includefile
.int exit

defword "INCLUDE",7,,include @ File Ext
@ ( i*x "name" -- j*x )
@ Skip leading whitespace and parse name delimited by a
@ white space character.
@ Push the address and length of name on the stack and
@ perform the function of INCLUDED.
.int bl,word @ Core Ext
.int count
.int twodup,errword,twostore
.int included
.int exit

defcode "FILE-POSITION",13,,fileposition @ File
@ ( fileid -- ud ior )
@ ud is the current file position for the file identified
@ by fileid. ior is the implementation defined I/O result.
popdsp r0
mov r4,r0
bl fileno
cmp r0,#-1
movne r0,r4
blne ftell
pushdsp r0
mov r1,#0
pushdsp r1
bl __errno_location
ldr r1,[r0]
pushdsp r1
next

defcode "REPOSITION-FILE",15,,repositionfile @ File
@ ( ud fileid -- ior )
@ Reposition the file identified by fileid to ud. ior is the
@ implementation defined I/O result code.
popdsp r0           @ file id
popdsp r1           @ Discard high portion
popdsp r1           @ New position
mov r2,#0           @ SEEK_SET
mov r4,r0
bl fileno
cmp r0,#-1
movne r0,r4
blne fseek
bl __errno_location
ldr r1,[r0]
pushdsp r1
next

defcode "FILE-SIZE",9,,filesize @ File
@ ( fileid -- ud ior )
@ ud is the size, in characters, of the file identified by fileid.
@ ior is the implementation defined I/O result code.
popdsp r0           @ file id
mov r4,r0           @ Save fileid 
bl fileno
cmp r0,#-1
beq _filesize_err
mov r0,r4
bl ftell            @ Get current position
mov r5,r0           @ Save position
mov r0,r4           @ Restore fileid
mov r1,#0           @ cell offset
mov r2,#2           @ SEEK_END
bl fseek            @ Set to end of file
mov r0,r4           @ Restore file id
bl ftell
pushdsp r0
mov r1,#0
pushdsp r1
mov r0,r4           @ Restore fileid
mov r1,r5           @ Restore position
mov r2,#0
bl fseek            @ Reset file position
b _filesize_ok
_filesize_err:
pushdsp r0
pushdsp r0
_filesize_ok:
bl __errno_location
ldr r1,[r0]
pushdsp r1
next

defword "UNDEFERRED",10,,undeferred
.int lits,3
.ascii ">>>"
.align 2
.int type
.int errword,twofetch,type
.int lits,29
.ascii "<<< Uninitialized DEFER word "
.align 2
.int type
.int exit

defword "DEFER",5,,defer @ Core Ext
@ ( "<spaces>name" -- )
@   Skip leading space delimiters. Parse 'name' delimited by a space.
@   Create a defintion for 'name' with the execustion semantics below.
@ name Execution: ( i*x -- j*x )
@   Execute the xt that 'name' is set to execute.
@ : DEFER
@   create
@   postpone undeferred
@   does>
@       @ execute
@   ;
.int create
.int lit,undeferred,comma
.int bdoes
defer_dodoes:
bl _dodoes
.int fetch
.int execute
.int exit

defword "DEFER@",6,,deferfetch @ Core Ext
@ ( xt1 -- xt2 )
@ xt2 is the execution token that xt1 is set to execute
@ Should only be used on DEFERred words
.int tobody
.int fetch
.int exit

defword "DEFER!",6,,deferstore @ Core Ext
@ ( xt2 xt1 -- )
@ Set the word xt1 to execute xt2.
@ xt1 should be a DEFER word
.int tobody
.int store
.int exit

defword "ACTION-OF",9,F_IMM,actionof @ Core Ext
@ Interpretation: ( "<spaces>name" -- xt )
@   Skip leading spaces and parse name. xt is the execution token that name is set to execute.
@   Ambiguous if name was not defined by DEFER
@ Compilation: ( "<spaces>name" -- )
@   Skip leading spaces and parse name. 
@ Run-time: ( xt -- )
@   xt is the execution token that name is set to execute.
.int state,fetch
.int zbranch,28
.int btick
.int lit,deferfetch,comma
.int branch,12
.int tick,deferfetch
.int exit

defword "IS",2,F_IMM,is @ Core Ext
@ Interpretation: (xt "<spaces>name" -- )
@   Skip leading space and parse name. Set name to execute xt.
@   name should be a DEFER word
@ Compilation: ( "<spaces>name" -- )
@   Skip leading spaces and parse name. Append the run time semantics given below
@ Run-time: ( xt -- )
@   Set name to execute xt
@ : IS
@   state @
@   if                      \ If we're compiling
@       postpone [']
@       postpone defer!
@   else                    \ If we're interpreting
@       ' defer!
@   then
@ ; immediate
.int state,fetch
.int zbranch,28
.int btick
.int lit,deferstore,comma
.int branch,12
.int tick,deferstore
.int exit

defword "WORDLIST",8,,wordlist @ Search
@ Sets up a wordlist structure at here and returns that address
@ Structure is 2 cells containing link to lfa of latest word in current vocabulary
@ followed by a link to the lfa of the current vocabulary
@ : WORDLIST    ( -- wid )
@   HERE        \ Current address in the dictionary area
@   0 , 0 ,     \ Initialise the structure
@ ;
.int here,zero,comma
.int zero,comma
.int exit

defword "SET-CURRENT",11,,setcurrent
@ ( wid -- )
@ Set the compilation word list to the word list identified by wid.
.int current,store
.int exit

defword "GET-CURRENT",11,,getcurrent @ Search
@ ( -- wid )
@ Gets wid, the identifier of the compilation word list.
.int current,fetch
.int exit

defword "GET-ORDER",9,,getorder @ Search
@ ( -- widn ... wid1 n )
@ Return the number of word lists n in the search order and the word list identifiers
@ widn ... wid1. wid1 identifies the word list searched first and widn the word list
@ searched last.
@ : GET-ORDER
@   0 #order @ 1-               \ set up limit and index
@   do
@       i cells
@       #order cell+
@       + @
@       -1
@   +loop
@   #order @
@ ;
.int zero,hashorder,fetch,oneminus
.int pdo
.int i,cells
.int hashorder,cellplus
.int plus,fetch
.int minus1
.int pplusloop,-32
.int hashorder,fetch
.int exit

defword ">ORDER",6,,toorder
@ Push the wid onto the search order list so it is search first
@ ( wid -- )
@ : >ORDER
@   >r                          \ Store away wid
@   get-order                   ( widn ... wid1 n ) \ Get current order
@   r> swap 1+                  ( widn ... wid1 wid n+1 ) Get set for set-order
@   set-order
@ ;
.int tor,getorder
.int rto,swap,oneplus
.int setorder
.int exit

defword "SET-ORDER",9,,setorder @ Search
@ ( widn ... wid1 n -- )
@ Set the search order to be the wordlists identified by widn ... wid1. Subsequently
@ wid1 will be searched first and widn last. If n is zero, empty the search order. If n
@ is negative, set the search order to the implementation defined minimum search order.
@ : SET-ORDER
@   dup 8 >
@   if -49 throw then
@   dup 0>
@   if
@       dup #order !        \ Set new count
@       0 do                \ Set up a do loop to store each wid
@           i cells
@           #order cell+ +  \ Point to storage area for wid'n'
@           !
@       loop
@       exit
@   then
@   dup 0=
@   if                  \ empty the search order
@       0 #order !
@   then
@   0< if               \ Set to default
@       only
@   then
@ ;   
.int dup,lit,8,gt
.int zbranch,16
.int lit,-49,throw
.int dup,zgt
.int zbranch,60
.int dup,hashorder,store
.int zero
.int pdo
.int i,cells
.int hashorder,cellplus
.int plus,store
.int ploop,-28
.int exit
.int dup,zeq
.int zbranch,16
.int zero,hashorder,store
.int zlt,zbranch,8
.int only
.int exit

defword "VOCABULARY",10,,vocabulary
@ Create a wordlist <name>. Subsequent execution of name replaces
@ the first word list in the search order with  <name>.
@ Basically a common usage word that wraps the ANS stanaard wordlist
@ Structure of a vocabulary word:
@   +--------+--------------------------+------------------------+-----------------+
@   | Header | cfa containing does code | addess of new wordlist | vocabulary link |
@   +--------+--------------------------+------------------------+-----------------+
@                                               |
@                                             +-+
@                                             |  
@                                             V
@                                       +-------------------------------------+---------------------------+
@                                       | lfa of latest word in this wordlist | link to previous wordlist |
@                                       +-------------------------------------+---------------------------+
@                                         
@ : VOCABULARY ( "name" -- )
@   wordlist dup        \ ( -- wid wid ) creates a new wid and dups it 
@   create ,            \ ( wid wid -- wid ) create a vocabulary and compile the wid into the pfa
@   cell+               \ ( wid -- wid + 4 ) point to the cell after the wid
@   latest              \ ( wid+4 -- wid+4 lfa ) get the lfa of this new vocabulary word
@   swap !              \ ( wid+4 lfa -- ) \ store the lfa of the newly created vocab into the wid structure
@   voc-link @          \ ( -- voc )  gets address of a field in vocabulary created before this one
@   ,                   \ compile into dictionary
@   latest              \ ( -- lfa )   get the lfa of the new vocab word
@   cell+               \ ( lfa -- nfa )  get name field address
@   name>body           \ ( nfa -- pfa )  get pfa of latest
@   cell+               \ ( pfa -- pfa+4 )  get address of voclink area in new vocab
@   voc-link !          \ update voclink to point to voclink area of this new vocabulary
@   does>               \ execution time behaviour
@   @                   \ ( pfa -- wid ) gets the wid
@   #order              \ ( wid -- wid pfa ) gets address of #order variable which contains wid order list
@   cell+ !             \ store address of wid into first #order slot
@   ;
.int wordlist,dup
.int create
.int comma
.int cellplus,latest
.int swap,store
.int voclink,fetch
.int comma
.int latest,cellplus
.int nametobody,cellplus
.int voclink,store
.int bdoes
vocab_dodoes:
bl _dodoes
.int fetch
.int hashorder
.int cellplus,store
.int exit

defword "CONTEXT",7,,context
@ ( -- a-addr )
@ Returns an a-addr, the address of a cell containing
@ a pointer to the first word list in the search order
.int hashorder
.int cellplus,fetch
.int exit

defword "DEFINITIONS",11,,definitions @ Search
@ ( -- )
@ Make the compilation word list the same as the first wordlist in the search order.
@ Subsequent definition will be placed in the compilation word list.
@ : DEFINITIONS
@   context current !
@ ;
.int context,current,store
.int exit

defword "PREVIOUS",8,,previous  @ Search Ext
@ ( -- )
@ Transform the search order widn .. wid2, wid1 (where wid1 is searched first) into
@ widn .. wid2.
@ : PREVIOUS
@   get-order
@   nip 1-              \ Remove wid1 and decrement count
@   set-order
@ ;
.int getorder
.int nip,oneminus
.int setorder
.int exit

defword "ALSO",4,,also  @ Search Ext
@ ( -- )
@ Transform the search order widn ... wid2, wid1 (where wid1 is searched first) into
@ widn ... wid2, wid1, wid1.
@ : ALSO
@   get-order
@   over swap 1+                \ Duplicate wid1 and incrmeent count
@   set-order
@ ;
.int getorder
.int over,swap,oneplus
.int setorder
.int exit

defdoes "FORTH",5,,forth,vocab_dodoes @ Search Ext
@ FORTH is a vocabulary word which holds the address of the forth wordlist
@ followed by a link to the next vocabulary (starts off with zero)
.int f_wordlist
.int 0              @ ( voc-link )

defword "ONLY",4,,only @ Search Ext
@ ( -- )
@ Set the search order to the implementation defined search order (in this case FORTH).
@ : ONLY
@   forth-wordlist 1 set-order
@ ;
.int forth_wordlist
.int one,setorder
.int exit

defword "TRAVERSE-WORDLIST",17,,traverse_wordlist   @ Tools Ext
@ Given an execution token and a wid, execute xt once for each word in the wordlist wid,
@ passing the name token nt of the word to xt, until the wordlist exhausted or xt returns false.
@ : TRAVERSE-WORDLIST ( i*x xt wid -- j*x)
@   begin
@       @                       \ Get the lfa of word in wid
@       ?dup 0<>                \ Is it end of word list chain
@   while
@       dup >r                  \ Stash wid away
@       cell+ swap              \ get name field address and swap with xt
@       dup >r                  \ Stash away xt
@       execute                 \ Execute the word identified by the xt ( for example .name)
@       r> r>                   \ Pull back xt and wid
@       rot not                 \ Rotate flag to top and if flag is false, finish the loop
@       if
@           2drop exit          \ Drop wid and xt and leave
@       then
@   repeat
@   drop                        \ Drop xt
;
.int fetch
.int qdup,zne
.int zbranch,72
.int dup,tor
.int cellplus,swap
.int dup,tor
.int execute
.int rto,rto
.int rot,not
.int zbranch,12
.int twodrop,exit
.int branch,-84
.int drop
.int exit

defword "XT>LINK",7,,xtolink
@ : XT>LINK
@       LATEST
@       BEGIN
@           ?DUP
@       WHILE
@           2DUP SWAP
@           <
@           IF
@               NIP EXIT
@           THEN
@           @
@       REPEAT
@       DROP
@       0
@   ;
.int latest
.int qdup
.int zbranch,44
.int twodup,swap
.int lt
.int zbranch,12
.int nip
.int exit
.int fetch
.int branch,-48
.int drop
.int zero
.int exit

defword "XT>DATA",7,,xtodata
.int cellplus
.int exit

defword "LINK>NAME",9,,linktoname
.int cellplus
.int exit

defword "NAME>STRING",11,,nametostring @ Tools Ext
.int count
.int f_immed,f_cmponly,or
.int invert,and
.int exit

defword "NAME>BODY",9,,nametobody
.int nametostring
.int plus,aligned
.int cellplus
.int exit

defword ".NAME",5,,dotname
.int nametostring
.int type,space
.int true
.int exit

defword "WORDS",5,,words
.int lit,dotname
.int context
.int traverse_wordlist
.int exit

defword "ORDER",5,,order @ Search Ext
@ ( -- )
@ Display the word lists in the search order in their search order sequence, from first searched to
@ last searched. Also display the word list into which definitions will be placed.
@ : ORDER
@   ." context: "
@   #order @ 0                  \ Get number of wordlists and set up for DO loop
@   do
@       #order cell+
@       i cells + @             \ Get address of wordlist
@       cell+                   \ Advance to vocabulary link
@       @                       \ Get lfa of vocabulary
@       ?dup 0=                 \ Is it zero - which means it's an unnamed wordlist, not a vocabulary
@       if
@           ." UNNAMED "
@       else
@           cell+               \ advance to vocabulary name field address
@           .name drop          \ Print name
@       then
@   loop
@   4 spaces
@   ." current: "
@   current @                   \ Get compilation wordlist
@   cell+ @                     \ Advance to vocabulary link part of wid and get lfa of vocabulary
@   ?dup 0=                     \ If zero, it's just a wid, not a vocabulary
@   if
@       ." UNNAMED "
@   else
@       cell+                   \ NFA of vocab
@       .name drop              \ Print the name
@   then
@ ;
.int lits,9
.ascii "context: "
.align 2
.int type
.int hashorder,fetch,zero       @ Get count of wordlists in #order
.int pdo                        @ Loop round for each wid in search order
.int hashorder,cellplus         @ Step over #order count
.int i,cells,plus
.int fetch,cellplus             @ Get address of wordlist and advance to vocabulary link
.int fetch                      @ Fetch lfa of vocabulary
.int qdup,zeq                   @ Is this wordlist connected to a vocabulary?
.int zbranch,32
.int lits,8
.ascii "UNNAMED "
.align 2
.int type
.int branch,16
.int cellplus                   @ Advance to name field address
.int dotname,drop               @ Print name
.int ploop,-92
.int four,spaces                
.int lits,9
.ascii "current: "
.align 2
.int type
.int current,fetch              @ Get compilation (current) wordlist
.int cellplus,fetch             @ Advance to vocabulary link part of wid and fetch lfa of vocabulary
.int qdup,zeq                   @ Is this wordlist connected to a vocabulary?
.int zbranch,32
.int lits,8
.ascii "UNNAMED "
.align 2
.int type
.int branch,16 
.int cellplus                   @ NFA of vocabulary
.int dotname,drop
.int exit

defword "VOCTRIM",7,,voctrim
@ ( lfa of marker word -- )
@ Used by MARKER to trim vocabularies created after the marker.
@ : VOCTRIM
@   >r  
@   voc-link @                  \ Get vocabulary link
.int tor
.int voclink
.int fetch,qdup
.int zbranch,68
.int dup,minus1,cells,plus
.int fetch,dup
.int fetch,dup
.int rfetch,lt
.int zbranch,-20
.int swap,store
.int branch,-76
.int voclink
.int fetch,dup
.int rfetch,lt
.int zbranch,-20
.int voclink,store
.int rto,drop
.int exit

defword "MARKER",6,,marker @ Core Ext
@ ( "<spaces>name" -- )
@ Skip leading space delimiters. Parse 'name' delimited by a space.
@ Create a definition of name with the execution semantics defined below.
@ 'name' Execution: ( -- )
@   Restore all dictionary allocation and search order pointers to the state they had
@   just prior to the definition of 'name'. Remove the definition of 'name' and all
@   subsequent definitions.
@ On creation of the marker word, its PFA will look like:
@ +---------------------+-------+-------+-----+-------+--------+---------+-------------------+
@ | search order length | wid 1 | wid 2 | ... | wid n | latest | current | lfa before marker |
@ +---------------------+-------+-------+-----+-------+--------+---------+-------------------+
@ : MARKER
@   create
@   #order @            	\ Get length of search order
@   dup ,               	\ Compile in length
@   0 do                	\ Loop n times
@       #order cell+    	\ Go to first wid pointer in list
@       i cells +       	\ Go to nth wid pointer
@       @ ,             	\ Compile in the wid
@   loop
@   latest ,                \ Store link to latest definition (i.e. the marker word itself)
@   current @ ,             \ Store link to current wid
@   latest @ ,              \ Store lfa of latest definition before this one
@   does>
@       dup @               \ Get old search-order count
@       #order !            \ Store in search-order list
@       dup @ 0             \ Set up for do loop
@       do
@           cell+ dup @     \ Get first wid pointer
@           #order cell+    \ Address in #order
@           i cells + !     \ and store in old wid
@       loop
@       cell+ dup @         \ Get stored link to lfa of myself (which was old here )
@       dup >r to here      \ Reset dictionary pointer
@       cell+ dup @
@       current !           \ Reset current wid
@       cell+ @
@       current @ !         \ Reset latest to definition before marker
@       r> voctrim          \ Get back lfa of myself and trim the vocabulary
@ ;
.int create
.int hashorder,fetch
.int dup,comma
.int zero
.int pdo
.int hashorder,cellplus
.int i,cells,plus,fetch
.int comma
.int ploop,-32
.int latest,comma
.int current,fetch,comma
.int latest,fetch,comma
.int bdoes
bl _dodoes
.int dup,fetch
.int hashorder,store
.int dup,fetch,zero
.int pdo
.int cellplus,dup
.int fetch,hashorder,cellplus
.int i,cells,plus,store
.int ploop,-40
.int cellplus,dup,fetch
.int dup,tor
.int pto,value_here
.int cellplus,dup,fetch
.int current,store
.int cellplus,fetch
.int current,fetch,store
.int rto
.int voctrim
.int exit

defcode "OPENMEM",7,,openmem
ldr r0,=mem_file
ldr r1,memflags
bl open
pushdsp r0
next
mem_file:
.asciz "/dev/mem"
memflags:
.word 1052674

defcode "CLOSEMEM",8,,closemem
popdsp r0
bl close
pushdsp r0
next

defcode "ALLOCATE",8,,allocate          @ ( u -- a-aadr ior )
popdsp r0
bl malloc
cmp r0,#0
moveq r1,#-1
movne r1,#0
pushdsp r0
pushdsp r1
next

defcode "FREE",4,,myfree
popdsp r0
bl free
mov r0,#0
pushdsp r0
next

@ Maps I/O address space addr2 onto allocated user space memory a-addr
@ of size n1, using open dev/mem file descriptor fd
defcode "MEMMAP",6,,memmap              @ ( a-addr n1 fd a-addr2 )
popdsp r3       @ a-addr2
popdsp r2       @ fd
popdsp r1       @ n1
popdsp r0       @ a-addr
pushdsp r3      @ 6th parameter to mmap
pushdsp r2      @ fd is 5th param
mov r3,#1       @ flags
mov r2,#3       @ prot
bl mmap
add sp,sp,#8
pushdsp r0
next

defword "CATCH",5,,catch @ Exception
@ ( i*x xt -- j*x 0|i*x n)
@ Pushes an exception frame onto the exception stack
@ and then executes the execution token xt in such
@ a way that control can be transferred to a point
@ just after catch if THROW is executed during the 
@ execution of xt.
@ : CATCH
@   SP@ >R                  ( Store away address of top of stack )
@   THROWFRAME @ >R         ( Store away value of throwframe )
@   RP@ THROWFRAME !        ( Store away address of top of return stack in throwframe )
@   EXECUTE                 ( execute returns if no THROW )
@   R> THROWFRAME !         ( Restore previous throwframe )
@   R> DROP                 ( Discard the saved stack pointer )
@   0                       ( Indicate normal completion )
@   ;
.int spfetch,tor
.int throwframe,fetch,tor
.int rpfetch,throwframe,store
.int execute
.int rto,throwframe,store
.int rto,drop
.int zero
.int exit

defword "THROW",5,,throw @ Exception
@ ( k*x n -- k*x|i*x n )
@ 
.int qdup
.int zbranch,116
.int dup
.int minus1
.int eq
.int zbranch,8
.int abortcore
.int dup
.int minus2
.int eq
.int zbranch,24
.int type
.int sp0,fetch,spstore
.int quit
.int throwframe,fetch,rpstore
.int rto,throwframe,store
.int rto,swap,tor,spstore
.int drop,rto
.int exit

.bss
.align 5
.set data_size,0x1C000
ds:
.space data_size
ds_top:

.bss
.align 5
.set word_size,0x200
word_buffer:
.space word_size
wb_top:

.bss
.align 5
string_buffer:
.space word_size
stb_top:

.bss 
.align 5
.set line_size,0x200
line_buffer:
.space line_size
lb_top:

.bss
.align 5
.set fb_size,0x200
file_buffer:
.space fb_size
fb_top:

.bss
.align 5
.set rs_size,0x200
rs:
.space rs_size
rs_top:

.bss
.align 5
.set pad_size,0x200
pad_start:
.space pad_size
pad_top:
.space 4

.bss
.align 5
.set hold_size,0x200
hold_start:
.space hold_size
hold_top:
.space 4

