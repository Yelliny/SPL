all: 

%: 
	echo '(load "pattern-matcher.scm") (load "pc.scm") (load "qq.scm") (load "project.scm") (load "sexpr-parser.scm") (load "tag-parser.scm") (load "semantic-analyzer.scm") (load "code-gen.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q

	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -o $(MAKECMDGOALS) $(MAKECMDGOALS).o

clean:
	rm -f a.o a.s a

#  nasm -f elf64 a.s -o a.o && gcc -m64 -o a a.o