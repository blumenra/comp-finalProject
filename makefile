
all: schemetest

schemetest: scheme.o
	#ld -g -m64 scheme.o -o schemetest
	gcc -m64 scheme.o -o schemetest

scheme.o: scheme.s
	nasm -g -w+all  -f elf64  scheme.s -o scheme.o
	
.PHONY:
	clean

clean:
	rm ./*.o schemetest


