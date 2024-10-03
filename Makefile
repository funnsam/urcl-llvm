CC_FLAGS=-Wpedantic -Wall -Wextra -std=gnu11 -lrt -O3
CC=gcc
LD_FLAGS=-O3
LD=gcc

test: tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex
	for f in `echo "$^"`; do \
		./$$f; \
	done

tests/runtime.o:
	$(CC) tests/runtime.c -c -o tests/runtime.o $(CC_FLAGS)

generic_rt.o:
	$(CC) generic_rt.c -c -o generic_rt.o $(CC_FLAGS)

urclos_rt.o:
	$(CC) urclos_rt.c -c -o urclos_rt.o $(CC_FLAGS)

%.o: %.urcl
	urcl-llvm $< -O3 -o $@

tests/%: tests/runtime.o tests/%.o
	$(LD) $^ -o $@ $(LD_FLAGS)

benchmarks/%: generic_rt.o benchmarks/%.o
	$(LD) $^ -o $@ $(LD_FLAGS)

urclos: urclos_rt.o urcl-os/urclos3.o
	$(LD) $^ -o $@ $(LD_FLAGS)

clean:
	- rm *.o

.PHONY: test clean
