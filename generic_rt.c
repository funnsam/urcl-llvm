#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <locale.h>
#include <time.h>

extern size_t urcl_main();

union WordAndFloat {
    size_t w;
    float f;
};

size_t urcl_in(size_t port) {
    switch (port) {
        case 40: {
            return (size_t) rand();
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%lu was read\n", port);
            return 0;
        }
    }
}

void urcl_out(size_t port, size_t data) {
    switch (port) {
        case 1: {
            printf("%lc", (uint32_t) data);
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
            printf("%lu", data);
            break;
        }
        case 24: {
            printf("%li", data);
            break;
        }
        case 27: {
            printf("%lx", data);
            break;
        }
        case 28: {
            union WordAndFloat f = { .w = data };
            printf("%f", f.f);
            break;
        }
        case 40: {
            srand(data);
            break;
        }
        default: {
            printf("\n\x1b[1;33mW:\x1b[0m unknown port %%%lu was written to with %lu\n", port, data);
            break;
        }
    }
}

int main() {
    setlocale(LC_ALL, "");
    struct timespec start, end;
    clock_gettime(CLOCK_REALTIME, &start);
    srand(start.tv_nsec);

    size_t inst = urcl_main();

    clock_gettime(CLOCK_REALTIME, &end);

    double time = (double)(end.tv_sec - start.tv_sec) + (double)(end.tv_nsec - start.tv_nsec) * 1e-9;

    printf("\n\x1b[1;32mI:\x1b[0m ran %'lu instructions in %'.1fs (%'.0f Hz)\n", inst, time, ((double) inst) / time);
    return 0;
}
