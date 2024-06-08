#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <locale.h>
#include <time.h>

#include "urclos_fs.h"

#ifdef _WIN32
    #include <conio.h>
    #define INIT_TERM 0
#else
    #include <termios.h>
    struct termios orig_termios;
    void reset_term() {
        tcsetattr(0,TCSAFLUSH,&orig_termios);
    }

    void init_term() {
        tcgetattr(0,&orig_termios);
        atexit(reset_term);
        struct termios raw = orig_termios;
        raw.c_lflag &= ~(ECHO | ICANON);
        tcsetattr(0, TCSAFLUSH, &raw);
    }

    #define INIT_TERM init_term()
    #define getch getchar
#endif

extern uint64_t urcl_main();
typedef uint16_t urcl_t;

union WordAndFloat {
    urcl_t w;
    float f;
};

urcl_t address = 0;

urcl_t urcl_in(urcl_t port) {
    switch (port) {
        case 1: {
            return getchar();
        }
        case 32: {
            return address;
        }
        case 33: {
            return fs[address];
        }
        case 40: {
            return (urcl_t) rand();
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%u was read\n", port);
            return 0;
        }
    }
}

void urcl_out(urcl_t port, urcl_t data) {
    switch (port) {
        case 1: {
            putchar(data);
            break;
        }
        case 16: {
            putchar(data & 0xFF);
            break;
        }
        case 19: {
            putchar(data & 0x7F);
            break;
        }
        case 2:
        case 25: {
            printf("%u", data);
            break;
        }
        case 24: {
            printf("%i", (int32_t) data);
            break;
        }
        case 27: {
            printf("%04x", data);
            break;
        }
        case 28: {
            union WordAndFloat f = { .w = data };
            printf("%f", f.f);
            break;
        }
        case 32: {
            address = data;
            break;
        }
        case 33: {
            fs[address] = data;
            break;
        }
        case 40: {
            srand(data);
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%u was written to with %u\n", port, data);
            break;
        }
    }
}

int main() {
    INIT_TERM;

    (void)urcl_main();

    printf("\n\x1b[1;32mI:\x1b[0m URCL-OS halted\n");
    return 0;
}
