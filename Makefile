all: 

%: 
	echo '(load "project/pattern-matcher.scm") (load "project/pc.scm") (load "project/qq.scm") (load "project/project.scm") (load "project/sexpr-parser.scm") (load "project/tag-parser.scm") (load "project/semantic-analyzer.scm") (load "project/code-gen.scm") (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q

	nasm -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc -m64 -o $(MAKECMDGOALS) $(MAKECMDGOALS).o