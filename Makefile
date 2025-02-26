CC_FLAGS = -Wpedantic -Wall -Wextra -std=gnu23 -lrt -O3
CC = gcc
LD_FLAGS = -O3
LD = gcc

temp_bits := $(shell mktemp -u)

test: tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex
	for f in `echo "$^"`; do \
		./$$f; \
	done

tests/runtime_%.o: tests/runtime.c
	$(CC) $^ -c -o $@ $(CC_FLAGS) -DURCL_BITS=$*

generic_rt_%.o: generic_rt.c
	$(CC) $^ -c -o $@ $(CC_FLAGS) -DURCL_BITS=$*

urclos_rt.o: urclos_rt.c
	if [[ "$$(cat $(temp_bits))" != "16" ]]; then \
		echo assert failed: urclos is expected to be 16 bits; \
		exit 1; \
	fi
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

tests/%: tests/%.o
	$(eval RT_OUT = tests/runtime_$(shell cat $(temp_bits)).o)
	make $(RT_OUT)
	$(LD) $^ $(RT_OUT) -o $@ $(LD_FLAGS)

urclos: urcl-os/urclos3.o urclos_rt.o
	$(LD) $^ -o $@ $(LD_FLAGS)

clean:
	- find . -name "*.o" -delete
	- rm tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex benchmarks/mandelbrot urclos

clean_tests:
	- rm tests/core tests/basic1 tests/basic2 tests/special_regs tests/complex tests/*.o

.PHONY: test clean clean_tests
