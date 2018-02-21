
%:
	echo '(load "compiler.scm")' '(compile-scheme-file "$(MAKECMDGOALS).scm" "1.s")' | scheme -q
# 	cat $(MAKECMDGOALS).scm > $(MAKECMDGOALS).tmp.scm
# 	cat scheme.s > $(MAKECMDGOALS) scm
# 	cat $(MAKECMDGOALS).tmp.scm >> $(MAKECMDGOALS).scm
# 	rm -f $(MAKECMDGOALS).tmp.scm
	nasm -g -f elf64 -w+all -o 1.o 1.s
	gcc -m64 -g -Wall -o run 1.o
	
	
.PHONY:
	clean
clean:
	rm -f ./run