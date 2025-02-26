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

typedef uint16_t urcl_t;
extern size_t urcl_main();
void urcl_in(urcl_t *ret, uint8_t port);
void urcl_out(uint8_t port, urcl_t *data);

union WordAndFloat {
    size_t w;
    float f;
};

urcl_t* fs;
urcl_t address = 0;

void urcl_in(urcl_t *ret, uint8_t port) {
    switch (port) {
        case 1: {
            *ret = getchar();
            break;
        }
        case 32: {
            *ret = address;
            break;
        }
        case 33: {
            *ret = fs[address];
            break;
        }
        case 40: {
            *ret = (urcl_t) rand();
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%hhu was read\n", port);
            break;
        }
    }
}

void urcl_out(uint8_t port, urcl_t *data) {
    switch (port) {
        case 1: {
            printf("%lc", (uint32_t) *data);
            break;
        }
        case 16: {
            putchar(*data & 0xFF);
            break;
        }
        case 19: {
            putchar(*data & 0x7F);
            break;
        }
        case 2:
        case 25: {
            printf("%hu", *data);
            break;
        }
        case 24: {
            printf("%hi", *data);
            break;
        }
        case 27: {
            printf("%04hx", *data);
            break;
        }
        case 28: {
            union WordAndFloat f = { .w = *data };
            printf("%f", f.f);
            break;
        }
        case 32: {
            address = *data;
            break;
        }
        case 33: {
            fs[address] = *data;
            break;
        }
        case 40: {
            srand(*data);
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%hhu was written to with %hu\n", port, *data);
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
    fs = (uint16_t*) malloc(size);
    if ((int) fread(fs, 1, size, fs_file) != size) {
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
