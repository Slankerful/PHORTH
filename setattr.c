#include <termios.h>

struct termios cooked;

void setattr() {
struct termios info;
tcgetattr(0, &info);          /* get current terminal attirbutes; 0 is the file descriptor for stdin */

tcgetattr(0, &cooked);

info.c_lflag &= ~(ICANON | ECHO);      /* disable canonical mode and echo */
info.c_cc[VMIN] = 1;          /* wait until at least one keystroke available */
info.c_cc[VTIME] = 0;         /* no timeout */
tcsetattr(0, TCSANOW, &info); /* set immediately */
}
