#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

extern uint32_t urcl_main();

uint32_t urcl_in(uint32_t port) {
    switch (port) {
        case 40: {
            return (uint32_t) rand();
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%u was read\n", port);
            return 0;
        }
    }
}

void urcl_out(uint32_t port, uint32_t data) {
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
            printf("%x", data);
            break;
        }
        case 28: {
            printf("%f", *((float*) &data));
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
    uint32_t inst = urcl_main();
    printf("\n\x1b[32mI:\x1b[0m ran %u instructions\n", inst);
    return 0;
}
