CC_FLAGS=-Wpedantic -Wall -Wextra -std=gnu11 -lrt -O3
CC=gcc
LD_FLAGS=-O3
LD=gcc

test: test_runtime.o core basic1 basic2 special_regs complex

test_runtime.o:
	$(CC) tests/runtime.c -c -o test_runtime.o $(CC_FLAGS)

core: test_runtime.o
	cargo r -r -- tests/core.urcl -O3
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

basic1: test_runtime.o
	cargo r -r -- tests/basic1.urcl -O3
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

basic2: test_runtime.o
	cargo r -r -- tests/basic2.urcl -O3
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

special_regs: test_runtime.o
	cargo r -r -- tests/special_regs.urcl -O3
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

complex: test_runtime.o
	cargo r -r -- tests/complex.urcl -O3
	$(LD) urcl.o test_runtime.o $(LD_FLAGS)
	./a.out

runtime.o:
	$(CC) generic-rt.c -c -o runtime.o $(CC_FLAGS)

mandelbrot: runtime.o
	cargo r -r -- benchmarks/mandelbrot-fortran.urcl -O3
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

urclos_fs.h:
	python3 gen_urclos_fs.py urcl-os/fs.bin

urclos_rt.o: urclos_fs.h
	$(CC) urclos-rt.c -c -o urclos_rt.o $(CC_FLAGS)

urclos: urclos_rt.o
	cargo r -r -- urcl-os/urclos3.urcl -O3 --max-heap 16386 --max-stack 128
	$(LD) urcl.o urclos_rt.o $(LD_FLAGS)
	./a.out

clean:
	- rm *.o
