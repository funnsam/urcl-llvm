#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <locale.h>
#include <time.h>

typedef _BitInt(URCL_BITS) urcl_t;
extern size_t urcl_main();
void urcl_in(urcl_t *ret, uint8_t port);
void urcl_out(uint8_t port, urcl_t *data);

union WordAndFloat {
    urcl_t w;
    float f;
};

void urcl_in(urcl_t *ret, uint8_t port) {
    switch (port) {
        case 40: {
            *ret = (urcl_t) rand();
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%lu was read\n", port);
            *ret = 0;
            break;
        }
    }

    return;
}

void urcl_out(uint8_t port, urcl_t *data) {
    switch (port) {
        case 1: {
            printf("%lc", (uint32_t) *data);
            break;
        }
        case 16: {
            putchar((uint64_t) data & 0xFF);
            break;
        }
        case 19: {
            putchar((uint64_t) data & 0x7F);
            break;
        }
        case 2:
        case 25: {
            printf("%lu", (uint64_t) *data);
            break;
        }
        case 24: {
            printf("%li", (int64_t) *data);
            break;
        }
        case 27: {
            printf("%lx", (uint64_t) *data);
            break;
        }
        case 28: {
            union WordAndFloat f = { .w = *data };
            printf("%f", f.f);
            break;
        }
        case 40: {
            srand(*data);
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%hhu was written to with %lu\n", port, (uint64_t) *data);
            break;
        }
    }
}

void memory_oob(urcl_t *addr) {
    printf("\n\x1b[1;31mE:\x1b[0m out of bounds memory address 0x%lx was indexed\n", *addr);
}

int main() {
    setlocale(LC_ALL, "");
    struct timespec start, end;
    clock_gettime(CLOCK_REALTIME, &start);
    srand(start.tv_nsec);

    size_t inst = urcl_main();

    clock_gettime(CLOCK_REALTIME, &end);

    double time = (double)(end.tv_sec - start.tv_sec) + (double)(end.tv_nsec - start.tv_nsec) * 1e-9;

    printf("\n\x1b[1;32mI:\x1b[0m ran %'zu instructions in %'.1fs (%'.0f Hz)\n", inst, time, ((double) inst) / time);
    return 0;
}
