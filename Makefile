test:
	cargo r -- tests/core.urcl && gcc urcl.o tests/runtime.c && ./a.out
	cargo r -- tests/basic.urcl && gcc urcl.o tests/runtime.c && ./a.out
	cargo r -- tests/complex.urcl && gcc urcl.o tests/runtime.c && ./a.out
