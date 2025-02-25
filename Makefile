CC_FLAGS = -Wpedantic -Wall -Wextra -std=gnu23 -lrt -O3
CC = clang
LD_FLAGS = -O3
LD = gcc

temp_bits := $(shell mktemp -u)

test: tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex
	for f in `echo "$^"`; do \
		./$$f; \
	done

tests/runtime.o: tests/runtime.c
	$(CC) $^ -c -o $@ $(CC_FLAGS)

# generic_rt.o: generic_rt.c
# 	$(CC) $^ -c -o $@ $(CC_FLAGS)

generic_rt_%.o: generic_rt.c
	$(CC) $^ -c -o $@ $(CC_FLAGS) -DURCL_BITS=$*

urclos_rt.o: urclos_rt.c
	$(CC) $^ -c -o $@ $(CC_FLAGS)

%.o: %.urcl
	cargo r -r -- $< -O3 -o $@ --emit-ir --native-addr --output-target-data $(temp_bits)

benchmarks/%: benchmarks/%.o
	$(eval RT_OUT = generic_rt_$(shell cat $(temp_bits)).o)
	make $(RT_OUT)
	$(LD) $^ $(RT_OUT) -o $@ $(LD_FLAGS)

examples/%: examples/%.o
	$(eval RT_OUT = generic_rt_$(shell cat $(temp_bits)).o)
	make $(RT_OUT)
	$(LD) $^ $(RT_OUT) -o $@ $(LD_FLAGS)

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
