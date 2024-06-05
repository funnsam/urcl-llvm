#include <stdio.h>
#include <stdint.h>

extern uint32_t urcl_main();

uint32_t urcl_in(uint32_t port) {
}

void urcl_out(uint32_t port, uint32_t data) {
    switch (port) {
        case 1: {
            putchar(data);
            break;
        }
        default: {
            printf("\nW: unknown port %%%u was written with %u\n", port, data);
            break;
        }
    }
}

int main() {
    uint32_t inst = urcl_main();
    printf("\nI: ran %u instructions\n", inst);
    return 0;
}
