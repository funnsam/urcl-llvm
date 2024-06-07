CC_FLAGS=-Wpedantic -Wall -Wextra
CC=gcc
LD_FLAGS=-O3
LD=gcc

test: test_runtime.o core basic special_regs complex

test_runtime.o:
	$(CC) tests/runtime.c -c -o test_runtime.o $(CC_FLAGS)

core: test_runtime.o
	cargo r -r -- tests/core.urcl
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

basic: test_runtime.o
	cargo r -r -- tests/basic.urcl
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

special_regs: test_runtime.o
	cargo r -r -- tests/special_regs.urcl
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

complex: test_runtime.o
	cargo r -r -- tests/complex.urcl
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

runtime.o:
	$(CC) stdlib-rt.c -c -o runtime.o $(CC_FLAGS)

mandelbrot: runtime.o
	cargo r -r -- benchmarks/mandelbrot-fortran.urcl
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

clean:
	- rm urcl.o runtime.o test_runtime.o
