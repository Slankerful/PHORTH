phorth: phorth.S setattr.o resetattr.o term_input.o
	gcc -g -o phorth  phorth.S setattr.o term_input.o resetattr.o -lreadline

setattr.o: setattr.c
	gcc -c setattr.c

resetattr.o: resetattr.c
	gcc -c resetattr.c

term_input.o: term_input.c
	gcc -c term_input.c

