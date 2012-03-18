all : prog1 prog2 prog3 prog4 prog5 prog6 prog7 prog8 prog9

prog1 : prog1.c
	gcc ${CFLAGS} -g -o prog1 prog1.c

prog2 : prog2.c
	gcc ${CFLAGS} -g -o prog2 prog2.c

prog3 : prog3.c
	gcc ${CFLAGS} -g -o prog3 prog3.c

prog4 : prog4.c
	gcc ${CFLAGS} -g -o prog4 prog4.c

prog5 : prog5.c
	gcc ${CFLAGS} -g -o prog5 prog5.c

prog6 : prog6.c
	gcc ${CFLAGS} -g -o prog6 prog6.c

prog7 : prog7.c
	gcc ${CFLAGS} -g -o prog7 prog7.c

prog8 : prog8.c
	gcc ${CFLAGS} -g -o prog8 prog8.c -lpthread

prog9 : prog9.c
	gcc ${CFLAGS} -g -o prog9 prog9.c -lpthread -lm

clean :
	/bin/rm -f *.o prog[1-8] core* tags
