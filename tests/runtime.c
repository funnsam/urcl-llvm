#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "../api.h"

int total_t = 0;
int test_no = 1;
int t_case = 0;
int failed = 0;

void urcl_in(urcl_t *ret, uint8_t port) {
    switch (port) {
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%hhu was read\n", port);
            *ret = 0;
            return;
        }
    }
}

void urcl_out(uint8_t port, urcl_t *data) {
    switch (port) {
        case 1: {
            test_no += 1;
            t_case = 0;
            break;
        }
        case 2: {
            t_case += 1;

            if (*data != 0) {
                printf("\x1b[1;31mE:\x1b[0m test %i case %i failed (off by %li)\n", test_no, t_case, (int64_t) -((surcl_t) *data));
                failed += 1;
            }

            break;
        }
        case 24: {
            if ((int) *data != t_case) {
                printf("\x1b[1;31mE:\x1b[0m test %i failed (expected %lu cases, ran %i instead)\n", test_no, (uint64_t) *data, t_case);
                failed += 1;
            }
            break;
        }
        case 25: {
            total_t = (int) *data;
            break;
        }
        default: {
            printf("\x1b[1;33mW:\x1b[0m unknown port %%%hhu was written to with %lu\n", port, (uint64_t) *data);
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
