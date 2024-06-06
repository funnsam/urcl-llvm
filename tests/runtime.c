#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

extern uint64_t urcl_main();

uint8_t urcl_in(uint8_t port) {
    switch (port) {
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%u was read\n", port);
            return 0;
        }
    }
}

int test_no = 1;
int t_case = 0;
int failed = 0;

void urcl_out(uint8_t port, uint8_t data) {
    switch (port) {
        case 1: {
            test_no += 1;
            t_case = 0;
            break;
        }
        case 2: {
            t_case += 1;

            if (data != 0) {
                printf("\x1b[1;31mE:\x1b[0m test %i case %i failed (returned %u)\n", test_no, t_case, data);
                failed += 1;
            }

            break;
        }
        case 24: {
            if (data != t_case) {
                printf("\x1b[1;31mE:\x1b[0m test %i failed (expected %u cases, ran %i instead)\n", test_no, data, t_case);
                failed += 1;
            }
            break;
        }
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%u was written to with %u\n", port, data);
            break;
        }
    }
}

int main() {
    (void) urcl_main();
    printf("\x1b[32mI:\x1b[0m %i cases failed\n", failed);
    return failed;
}
