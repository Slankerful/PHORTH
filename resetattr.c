#include <termios.h>

extern struct termios cooked;

void resetattr() {
tcsetattr(0, TCSANOW, &cooked); /* set immediately */
}
