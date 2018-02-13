all: 

%: 
	echo '(load "compiler.scm" ) (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q

	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -o $(MAKECMDGOALS) $(MAKECMDGOALS).o

clean:
	rm -f a.s a.o a