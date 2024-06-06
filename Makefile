CC_FLAGS=-Wpedantic -Wall -Wextra
CC=clang
LD_FLAGS=-O3
LD=clang

test: runtime.o core basic special_regs complex

runtime.o:
	$(CC) tests/runtime.c -c -o runtime.o $(CC_FLAGS)

core: runtime.o
	cargo r -- tests/core.urcl
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

basic: runtime.o
	cargo r -- tests/basic.urcl
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

special_regs: runtime.o
	cargo r -- tests/special_regs.urcl
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

complex: runtime.o
	cargo r -- tests/complex.urcl
	$(LD) urcl.o runtime.o $(LD_FLAGS)
	./a.out

clean:
	rm urcl.o runtime.o
