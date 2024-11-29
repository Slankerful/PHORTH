#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

static char *buffer = (char *) NULL;

char * term_input() {
        if (buffer) {
            free(buffer);
            buffer = (char *) NULL;
        }
        buffer = readline(NULL);
        if (buffer && *buffer)
            add_history(buffer);
        return buffer;
}
