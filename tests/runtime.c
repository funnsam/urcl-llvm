#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

extern size_t urcl_main();

size_t urcl_in(size_t port) {
    switch (port) {
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%lu was read\n", port);
            return 0;
        }
    }
}

int total_t = 0;
int test_no = 1;
int t_case = 0;
int failed = 0;

void urcl_out(size_t port, size_t data) {
    switch (port) {
        case 1: {
            test_no += 1;
            t_case = 0;
            break;
        }
        case 2: {
            t_case += 1;

            if (data != 0) {
                printf("\x1b[1;31mE:\x1b[0m test %i case %i failed (returned %lu)\n", test_no, t_case, data);
                failed += 1;
            }

            break;
        }
        case 24: {
            if ((int) data != t_case) {
                printf("\x1b[1;31mE:\x1b[0m test %i failed (expected %lu cases, ran %i instead)\n", test_no, data, t_case);
                failed += 1;
            }
            break;
        }
        case 25: {
            total_t = (int) data;
            break;
        }
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%lu was written to with %lu\n", port, data);
            break;
        }
    }
}

int main() {
    (void) urcl_main();
    printf("\x1b[1;32mI:\x1b[0m %i cases failed\n", failed);

    bool fails = (total_t != test_no - 1) || (total_t == 0);
    if (fails) {
        printf("\x1b[1;31mE:\x1b[0m expected to run %i tests, ran %i instead\n", total_t, test_no - 1);
    }

    return failed + (int) fails;
}
