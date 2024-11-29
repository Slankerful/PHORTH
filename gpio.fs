base @
decimal

: bin. base @ 2 base ! swap u. base ! ;


\ Stores the memory mapped address of the gpio system
\ in the value gpio
: gpio_map		( -- a-addr )
	openmem
	0 swap
	4096 swap
	gpio_base
	memmap
	dup -1 =
	abort" Unable to map memory - have you used 'sudo phorth'?"
;

gpio_map value gpio

\ Calculate the required GPIO select register
\ given the pin number
: gpiosel		( +n -- a-addr )
	10 /
	cells
	gpio +
;

: gpioset		( +n -- )
	gpio 28 +
	over 32 /
	+
	1 rot lshift
	swap !
;

: gpioclear		( +n -- )
	gpio 40 +
	over 32 /
	+
	1 rot lshift
	swap !
;
	
\ Set a port as input
: gpio_in		( +n -- )
	dup gpiosel	( n a-addr )
	dup @		( n a-addr x )
	rot		( a-aadr x n )
	10 mod		( a-addr x n )
	3 *		( a-addr x n )
	7 swap		( a-addr x 7 n )
	lshift
	invert
	and
	swap !
;

\ Set a port as output 
: gpio_out		( +n -- )
	dup gpiosel	( n a-addr )
	dup @
	rot
	10 mod		( a-addr x n )
	3 * 
	1 swap
	lshift
	or
	swap !
;

: pin_on		( +n -- )
	dup gpio_in
	dup gpio_out
	gpioset
;

: pin_off		( +n -- )
	dup gpio_in
	dup gpio_out
	gpioclear
;

create pins 7 , 8 , 9 , 10 , 11 , 17 , 18 , 27 , 22 , 23 , 24 , 25 ,

: pin 			( n -- pinno )
	cells
	pins +
	@
;

: all_off 12 0 do i pin pin_off loop ;
: all_on 12 0 do i pin pin_on loop ;

base !
