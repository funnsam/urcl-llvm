CC_FLAGS=-Wpedantic -Wall -Wextra -std=gnu11 -lrt -O3
CC=gcc
LD_FLAGS=-O3
LD=gcc

test: tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex
	for f in `echo "$^"`; do \
		./$$f; \
	done

tests/runtime.o: tests/runtime.c
	$(CC) $^ -c -o $@ $(CC_FLAGS)

generic_rt.o: generic_rt.c
	$(CC) $^ -c -o $@ $(CC_FLAGS)

urclos_rt.o: urclos_rt.c
	$(CC) $^ -c -o $@ $(CC_FLAGS)

%.o: %.urcl
	cargo r -r -- $< -O3 -o $@ --emit-ir --native-addr

benchmarks/%: generic_rt.o benchmarks/%.o
	$(LD) $^ -o $@ $(LD_FLAGS)

examples/%: generic_rt.o examples/%.o
	$(LD) $^ -o $@ $(LD_FLAGS)

tests/%: tests/runtime.o tests/%.o
	$(LD) $^ -o $@ $(LD_FLAGS)

urclos: urclos_rt.o urcl-os/urclos3.o
	$(LD) $^ -o $@ $(LD_FLAGS)

clean:
	- find . -name *.o -delete
	- rm tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex benchmarks/mandelbrot urclos

clean_tests:
	- rm tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex tests/*.o

.PHONY: test clean clean_tests
