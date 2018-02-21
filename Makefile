
all: run

run: 1.o
	#ld -g -m64 scheme.o -o schemetest
# 	gcc -m64 scheme.o -o schemetest
	gcc -m64 1.o -o run

# scheme.o: scheme.s
# 	nasm -g -w+all  -f elf64  scheme.s -o scheme.o
	
1.o: 1.s
	nasm -g -w+all  -f elf64  1.s -o 1.o
	
.PHONY:
	clean

clean:
# 	rm ./*.o schemetest
	rm ./*.o run


