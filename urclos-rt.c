#include <errno.h>
#include <locale.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

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

urcl_t* fs;
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
            return fs[(int) address];
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
            printf("%lc", data);
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
            fs[(int) address] = data;
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

bool is_little_endian() {
    volatile uint32_t i = 0x01234567;
    return (*((uint8_t*) (&i))) == 0x67;
}

void swap_bytes(uint16_t* buf, int size) {
    for (int i = 0; i < size; i++) {
        buf[i] = ((buf[i] & 0xff) << 8) | ((buf[i] >> 8) & 0xff);
    }
}

int main(int argc, char* argv[]) {
    setlocale(LC_ALL, "");
    if (argc < 2) {
        printf("Usage: %s <fs>\n", argv[0]);
        return 1;
    }

    FILE* fs_file = fopen(argv[1], "r+b");
    if (fs_file == NULL) {
        perror("Loading filesystem failed");
        return 2;
    }

    fseek(fs_file, 0, SEEK_END);
    int size = ftell(fs_file);
    rewind(fs_file);
    fs = (urcl_t*) malloc(size);
    if (fread(fs, 1, size, fs_file) != (size_t) size) {
        perror("Loading filesystem failed");
        return 3;
    }

    if (is_little_endian()) {
        swap_bytes(fs, size / 2);
    }

    INIT_TERM;

    (void) urcl_main();

    printf("\n\x1b[1;32mI:\x1b[0m URCL-OS halted\n");

    if (is_little_endian()) {
        swap_bytes(fs, size / 2);
    }

    rewind(fs_file);
    fwrite(fs, size, 1, fs_file);
    fflush(fs_file);
    fclose(fs_file);

    free(fs);
    return 0;
}
