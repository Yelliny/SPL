
(define true-labl "L3")

(define false-labl "L4")

(define void-labl "L1")

(define nil-labl "L2")

(set! asm-counter 0)

(define get-inc-counter
	(lambda ()
		(let ((curr asm-counter))
			(set! asm-counter (1+ asm-counter))
			curr)))

(define overall-code-gen
	(lambda (pe)
		;(display constants-table) (newline)
		(let* (
			(prolog-code (gen-prolog))
			(global-labels-code (gen-global-table-labels -1))
			(emtsalog (gen-emtsalog))
			(runtime-code (gen-runtime 0))
			(epilog (gen-epilog))
			(the-magnificant-code (fold-left (lambda (init rest)
				(string-append init (code-gen rest -1) epilog)) "" pe))
			(consts-code (gen-const-table-asm))
 			(symbol-table-code (gen-symbol-table))
 			)
		(string-append
			prolog-code
			consts-code 
			"\n"
			global-labels-code
			symbol-table-code
			emtsalog
			runtime-code
			the-magnificant-code
			))))

(define gen-epilog
	(lambda ()
		"\npush qword rax\ncall write_sob_if_not_void\nadd rsp, 1*8\n"))

(define gen-emtsalog
	(lambda ()
	"\nsection .text\nglobal main\nmain:\n"))

(define gen-prolog
	(lambda ()
		(string-append
		"%include \"project/scheme.s\"\n\n" 
		"\nsection .data\n"
			"msg: db \"MOR TUR\", 0\n"
			"format_s: db \"%s\", 10, 0\n"
			"format_d: db \"%d\", 10, 0\n\n"
			"format_p: db \"%p\", 10, 0\n\n")))

(define code-gen
	(lambda (pe depth)
		(let ((type (car pe)))
			;(display `(code-gen pe ,pe)) (newline)
		(cond
			((equal? type 'const)
				(gen-const (cadr pe) depth))
			((equal? type 'if3)
				(gen-if (cdr pe) depth))
			((equal? type 'or)
				(gen-or (cadr pe) depth))
			((equal? type 'lambda-simple)
				(gen-lambda-simple (cdr pe) (+ 1 depth)))
			((equal? type 'lambda-opt)
				(gen-lambda-opt (cdr pe) (+ 1 depth)))
			((equal? type 'applic)
				(gen-applic (cadr pe) (caddr pe) depth))
			((equal? type 'tc-applic)
				(gen-tc-applic (cadr pe) (caddr pe) depth))
			((equal? type 'pvar)
				(gen-pvar (cdr pe) depth))
			((equal? type 'bvar)
				(gen-bvar (cdr pe) depth))
			((equal? type 'define)
				(gen-define (cdr pe) depth))
			((equal? type 'fvar)
				(gen-fvar (cdr pe) depth))
			((equal? type 'seq)
				(gen-seq (cadr pe) depth))
			((equal? type 'set)
				(gen-set (cdr pe) depth))
			((equal? type 'box)
				(gen-box (cadr pe) depth))
			((equal? type 'box-set)
				(gen-box-set (cdr pe) depth))
			((equal? type 'box-get)
				(gen-box-get (cadr pe) depth))
			(else
				(display `(code-gen not recognize pe ,pe)) (newline)
				"code-gen does not recognize you\n")))))

(define gen-symbol-table
	(lambda ()
		(fold-left (lambda (init curr)
						(string-append init
							"mov rdi, 8\n"
							"call malloc\n"
							;; address to symbol link
							"mov r8, rax\n"
							;; high 30 bits points to symbol string
							"mov r10, " (cadr curr) "\n"
							"sub r10, start_of_data\n"
							"shl r10, 34\n"
							;; 4 low bits are T_LINK
							"or r10, T_LINK\n"
							
							"mov [r8], r10\n"
							;; r11 - 30 bits pointer to symbol link
							"mov r11, r8\n"
							"sub r11, start_of_data\n"
							"shl r11, 34\n"
							"shr r11, 30\n"
							"or [r12], r11\n"
							"mov r12, r8\n"
							))
		"\nsection .data\nsymbol_table: dq 0\nsection .text\nmov r12, symbol_table\n" symbol-table))) 

(define gen-box-get
	(lambda (pe depth)
		(if (equal? (car pe) 'bvar)
			(let ((loc-lambda (number->string (caddr pe)))
				(loc-param (number->string (cadddr pe))))
				(string-append
					"# ----- gen-box-get bvar " loc-lambda " " loc-param "-----\n"
					"box_get_" (number->string (get-inc-counter)) ":\n"
					"push r8\n"
					"mov r8, [rbp + 2*8]\n"
					"mov r8, [r8 + 8*" loc-lambda"]\n"
					"mov r8, [r8 + 8*" loc-param "]\n"
					"mov rax, [r8]\n"
					"pop r8\n\n"
					))
			(let ((loc (number->string (caddr pe))))
				(string-append
					"# ----- gen-box-get pvar " loc " -----\n"
					"box_get_" (number->string (get-inc-counter)) ":\n"
					"push r8\n"
					"mov r8, [rbp + 4*8 + 8*" loc "]\n"
					"mov rax, [r8]\n"
					"pop r8\n\n"
					))
		)))

(define gen-box-set
	(lambda (pe depth)
		(if (equal? (caar pe) 'bvar)
			(let ((loc-lambda (number->string (caddar pe)))
				(loc-param (number->string (car (cdddar pe))))
				(val (cadr pe)))
				(string-append
					"# ----- gen-box-set -----\n"
					(code-gen val depth)
					"box_set_" (number->string (get-inc-counter)) ":\n"
					"push r8\n"
					"mov r8, [rbp + 2*8]\n"
					"mov r8, [r8 + 8*" loc-lambda"]\n"
					"mov r8, [r8 + 8*" loc-param "]\n"
					"mov [r8], rax\n"
					"pop r8\n"
					"mov rax, [" void-labl "]\n\n"
					))
			(let ((loc (number->string (caddar pe)))
				(val (cadr pe)))
				(string-append
					"# ----- gen-box-set -----\n"
					(code-gen val depth)
					"box_set_" (number->string (get-inc-counter)) ":\n"
					"push r8\n"
					"mov r8, [rbp + 4*8 + 8*" loc "]\n"
					"mov [r8], rax\n"
					"mov rax, [" void-labl "]\n\n"
					"pop r8\n"
					))
			)))

(define gen-box
	(lambda (pe depth)
		(string-append
			(code-gen pe depth)
			"# ----- gen-box -----\n"
			"gen_box_" (number->string (get-inc-counter)) ":\n"
			"push r8\n"
			"push rdi\n"
			"mov r8, rax\n"
			"mov rdi, 8\n"
			"call malloc\n"
			"mov [rax], r8\n"
			"pop rdi\n"
			"pop r8\n\n\n"
			)))

(define gen-define
	(lambda (pe depth)
		(if (> depth -1)
			(ERROR "define inside a function")
			(let* ((var-name (get-fvar-label (cadar pe)))
					(val (cadr pe)))
				(string-append 
					(code-gen val depth) 
					"mov [" var-name "], rax\n"
					"mov rax, [" void-labl "]\n\n")))))

(define ERROR
	(lambda (msg)
		(display (string-append "ERROR: " msg "\n"))
		(exit)))

(define gen-set
	(lambda (pe depth)
		(let* ((var-type (caar pe))
				(var-name (symbol->string (cadar pe)))
				(val (cadr pe)))
		(cond
			((equal? var-type 'fvar)
				(string-append 
					(code-gen val depth) 
					"mov [" var-name "], rax\n"
					"mov rax, [" void-labl "]\n\n"))
			((equal? var-type 'pvar)
				(let ((loc (number->string (caddar pe))))
				(string-append
					(code-gen val depth)
					"mov [rsp + 4*8 + 8*" loc "], rax\n"
					"mov rax, [" void-labl "]\n\n")))
			((equal? var-type 'bvar)
				(let ((loc-lambda (car (caddar pe)))
					(loc-param (cadr (caddar pe))))
				(string-append
					(code-gen val depth)
					"push r8\n"
					"mov r8, [rsp + 2*8 + 1*8]\n"
					"mov r8, [r8 + 8*" loc-lambda"]\n"
					"mov [r8 + 8*" loc-param "], rax\n"
					"pop r8\n"
					"mov rax, [" void-labl "]\n\n")))
			))))

(define gen-seq
	(lambda (pe depth)
		(string-append "# ----- gen-seq -----\n"
		(fold-left (lambda (init curr)
						(string-append init (code-gen curr depth)))
		"" pe))))


(define gen-const-table-asm
	(lambda ()
		(string-append "section .data\n"
			(fold-left string-append ""
				(map (lambda (const-entry)
						(let ((label (car const-entry))
							  (asm_code (gen-const-asm-code (list-ref const-entry 2))))
						(string-append label ":\n\t" asm_code "\n"))) constants-table)))))

(define gen-global-table-labels
	(lambda (depth)
			(fold-left 
				(lambda (init curr) (string-append init (car curr) ": dq 0\n"))
					"" global-table))) 


(define gen-runtime
	(lambda (depth)
			(string-append 
				(gen-plus depth) "mov [plus], rax\n"
				(gen-minus depth) "mov [minus], rax\n"
				(gen-cons depth) "mov [cons], rax\n"
				(gen-mult depth) "mov [mult], rax\n"
				(gen-divi depth) "mov [divi], rax\n"
				(gen-car depth) "mov [car], rax\n"
				(gen-cdr depth) "mov [cdr], rax\n"
				(gen-lower depth) "mov [lower], rax\n"
				(gen-greater depth) "mov [greater], rax\n"
				(gen-equali depth) "mov [equali], rax\n"
				(gen-boolean? depth) "mov [boolean?], rax\n"
				(gen-char? depth) "mov [char?], rax\n"
				(gen-integer? depth) "mov [integer?], rax\n"
				(gen-pair? depth) "mov [pair?], rax\n"
				(gen-number? depth) "mov [number?], rax\n"
				(gen-procedure? depth) "mov [procedure?], rax\n"
				(gen-string? depth) "mov [string?], rax\n"
				(gen-symbol? depth) "mov [symbol?], rax\n"
				(gen-vector? depth) "mov [vector?], rax\n"
				(gen-null? depth) "mov [null?], rax\n"
				(gen-char->integer depth) "mov [char_to_integer], rax\n"
				(gen-denominator depth) "mov [denominator], rax\n"
				(gen-integer->char depth) "mov [integer_to_char], rax\n"
				(gen-make-string depth) "mov [make_string], rax\n"
				(gen-make-vector depth) "mov [make_vector], rax\n"
				(gen-numerator depth) "mov [numerator], rax\n"
				(gen-string-length depth) "mov [string_length], rax\n"
				(gen-vector-length depth) "mov [vector_length], rax\n"
				(gen-string-ref depth) "mov [string_ref], rax\n"
				(gen-vector-ref depth) "mov [vector_ref], rax\n"
				(gen-not depth) "mov [not], rax\n"
				(gen-remainder depth) "mov [remainder], rax\n"
				(gen-set-car! depth) "mov [set_car], rax\n"
				(gen-set-cdr! depth) "mov [set_cdr], rax\n"
				(gen-string-set! depth) "mov [string_set], rax\n"
				(gen-vector-set! depth) "mov [vector_set], rax\n"
				(gen-apply depth) "mov [apply], rax\n"
				(gen-eq depth) "mov [eq?], rax\n"
				(gen-vector depth) "mov [vector], rax\n"
				(gen-string-symbol depth) "mov [string_symbol], rax\n"
				(gen-symbol-string depth) "mov [symbol_string], rax\n"
				)))

(define gen-symbol-string
	(lambda (depth)
		(let* ((code-label (string-append "symbol_string_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_symbol_string\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-string-symbol
	(lambda (depth)
		(let* ((code-label (string-append "string_symbol_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_string_symbol\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-vector
	(lambda (depth)
		(let* ((code-label (string-append "vector_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_vector\n"
			 code-label "_end:\n\n")))
		str)))


(define gen-eq
	(lambda (depth)
		(let* ((code-label (string-append "eq_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_eq\n"
			 code-label "_end:\n\n")))
		str)))		


(define gen-apply
	(lambda (depth)
		(let* ((code-label (string-append "apply_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_apply\n"
			 code-label "_end:\n\n")))
		str)))		
				
(define gen-vector-set!
	(lambda (depth)
		(let* ((code-label (string-append "vector_set_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_vector_set\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-string-set!
	(lambda (depth)
		(let* ((code-label (string-append "string_set_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_string_set\n"
			 code-label "_end:\n\n")))
		str)))					
				
(define gen-vector-ref
	(lambda (depth)
		(let* ((code-label (string-append "vector_ref_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_vector_ref\n"
			 code-label "_end:\n\n")))
		str)))				

(define gen-remainder
	(lambda (depth)
		(let* ((code-label (string-append "remainder_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_remainder\n"
			 code-label "_end:\n\n")))
		str)))	

(define gen-set-car!
	(lambda (depth)
		(let* ((code-label (string-append "set_car_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_set_car\n"
			 code-label "_end:\n\n")))
		str)))	
		
(define gen-set-cdr!
	(lambda (depth)
		(let* ((code-label (string-append "set_cdr_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_set_cdr\n"
			 code-label "_end:\n\n")))
		str)))		
				
(define gen-not
	(lambda (depth)
		(let* ((code-label (string-append "not_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_not\n"
			 code-label "_end:\n\n")))
		str)))	

(define gen-make-string
	(lambda (depth)
		(let* ((code-label (string-append "make_string_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_make_string\n"
			 code-label "_end:\n\n")))
		str)))					
				
(define gen-integer->char
	(lambda (depth)
		(let* ((code-label (string-append "integer_to_char_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_integer_to_char\n"
			 code-label "_end:\n\n")))
		str)))							
				
(define gen-denominator
	(lambda (depth)
		(let* ((code-label (string-append "denominator_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_denominator\n"
			 code-label "_end:\n\n")))
		str)))				
										
(define gen-char->integer
	(lambda (depth)
		(let* ((code-label (string-append "char_to_integer_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_char_to_integer\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-string-ref
	(lambda (depth)
		(let* ((code-label (string-append "string_ref_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_string_ref\n"
			 code-label "_end:\n\n")))
		str)))					
			
(define gen-vector-length
	(lambda (depth)
		(let* ((code-label (string-append "vector_length_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_length T_VECTOR\n"
			 code-label "_end:\n\n")))
		str)))				
			
(define gen-string-length
	(lambda (depth)
		(let* ((code-label (string-append "string_length_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_length T_STRING\n"
			 code-label "_end:\n\n")))
		str)))					
				
(define gen-numerator
	(lambda (depth)
		(let* ((code-label (string-append "numerator_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_numerator\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-make-vector
	(lambda (depth)
		(let* ((code-label (string-append "make_vector_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_make_vector\n"
			 code-label "_end:\n\n")))
		str)))	
				
(define gen-make-string
	(lambda (depth)
		(let* ((code-label (string-append "make_string_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_make_string\n"
			 code-label "_end:\n\n")))
		str)))					
				
(define gen-integer->char
	(lambda (depth)
		(let* ((code-label (string-append "integer_to_char_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_integer_to_char\n"
			 code-label "_end:\n\n")))
		str)))							
				
(define gen-denominator
	(lambda (depth)
		(let* ((code-label (string-append "denominator_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_denominator\n"
			 code-label "_end:\n\n")))
		str)))				
										
(define gen-char->integer
	(lambda (depth)
		(let* ((code-label (string-append "char_to_integer_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_char_to_integer\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-null?
	(lambda (depth)
		(let* ((code-label (string-append "null?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_NIL\n"
			 code-label "_end:\n\n")))
		str)))	
						
				
(define gen-procedure?
	(lambda (depth)
		(let* ((code-label (string-append "procedure?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_CLOSURE\n"
			 code-label "_end:\n\n")))
		str)))	
		
(define gen-string?
	(lambda (depth)
		(let* ((code-label (string-append "string?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_STRING\n"
			 code-label "_end:\n\n")))
		str)))	
		
(define gen-symbol?
	(lambda (depth)
		(let* ((code-label (string-append "symbol?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_SYMBOL\n"
			 code-label "_end:\n\n")))
		str)))	
		
(define gen-vector?
	(lambda (depth)
		(let* ((code-label (string-append "vector?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_VECTOR\n"
			 code-label "_end:\n\n")))
		str)))			
				
(define gen-number?
	(lambda (depth)
		(let* ((code-label (string-append "number?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_number?\n"
			 code-label "_end:\n\n")))
		str)))	
				
(define gen-pair?
	(lambda (depth)
		(let* ((code-label (string-append "pair?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_PAIR\n"
			 code-label "_end:\n\n")))
		str)))					
				
(define gen-integer?
	(lambda (depth)
		(let* ((code-label (string-append "integer?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_INTEGER\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-char?
	(lambda (depth)
		(let* ((code-label (string-append "char?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_CHAR\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-boolean?
	(lambda (depth)
		(let* ((code-label (string-append "boolean?_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_pred? T_BOOL\n"
			 code-label "_end:\n\n")))
		str)))						
				
(define gen-equali
	(lambda (depth)
		(let* ((code-label (string-append "equali_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_equali\n"
			 code-label "_end:\n\n")))
		str)))				
				
(define gen-greater
	(lambda (depth)
		(let* ((code-label (string-append "greater_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_greater\n"
			 code-label "_end:\n\n")))
		str)))
				
(define gen-lower
	(lambda (depth)
		(let* ((code-label (string-append "lower_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_lower\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-cdr
	(lambda (depth)
		(let* ((code-label (string-append "cdr_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_cdr\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-car
	(lambda (depth)
		(let* ((code-label (string-append "car_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_car\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-cons
	(lambda (depth)
		(let* ((code-label (string-append "cons_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_cons\n"
			 code-label "_end:\n\n")))
		str)))
		
		
(define gen-divi
    (lambda (depth)
        (let* ((code-label (string-append "divi_" (number->string (get-inc-counter))))
        (str (string-append
            (gen-closure-code depth code-label)
            "jmp " code-label "_end\n"
            "\n" code-label ":\n"
            "our_divi\n"
			 code-label "_end:\n\n"
			 )))
        str)))
		

(define gen-mult
    (lambda (depth)
        (let* ((code-label (string-append "mult_" (number->string (get-inc-counter))))
        (str (string-append
            (gen-closure-code depth code-label)
            "jmp " code-label "_end\n"
            "\n" code-label ":\n"
            "our_mult\n"
			 code-label "_end:\n\n"
			 )))
        str)))



(define gen-minus
	(lambda (depth)
		(let* ((code-label (string-append "minus_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_minus    \n\n"
			 code-label "_end:\n\n")))
		str)))

(define gen-plus
	(lambda (depth)
		(let* ((code-label (string-append "plus_" (number->string (get-inc-counter))))
		(str (string-append
			(gen-closure-code depth code-label)
			"jmp " code-label "_end\n"
			"\n" code-label ":\n"
			"our_plus\n"
			 code-label "_end:\n\n")))
		str)))


(define get-fvar-label
	(lambda (fvar-name)
		(let* ((vars (map car global-table))
			(tags (map caddr global-table)))
				(if (member (symbol->string fvar-name) vars)
					(symbol->string fvar-name)
					(list-ref vars (index-of tags fvar-name))))))

(define gen-fvar
	(lambda (pe depth)
		(string-append 
			"mov rax, [" (get-fvar-label (car pe)) "]\n")))

(define gen-bvar
	(lambda (pe depth)
		(let*
			((loc-lambda (number->string (cadr pe)))
			(loc-param (number->string (caddr pe)))
			(var (symbol->string (car pe)))
			)
			(string-append
				"# ----- gen-bvar " var " " loc-lambda " " loc-param " -----\n"  
				"push r8\n"

				; r8 - pointer to env
				"mov r8, [rbp + 2*8]\n"
			
				; r8 - pointer to loc-lambda frame
				"mov r8, [r8 + 8*" loc-lambda"]\n"

				; rax - loc-param in loc-lambda frame
				"mov rax, [r8 + 8*" loc-param "]\n"

				"pop r8\n\n"
				))))

(define gen-pvar
	(lambda (pe depth)
		(let ((loc (number->string (cadr pe)))
			(var (symbol->string (car pe))))
		(string-append 
			"# ----- gen-pvar " var " " loc " -----\n"  
			"mov rax, [rbp + 4*8 + 8*" loc "]\n\n"
			))))


(define gen-if
	(lambda (pe depth)
		(let* ((counter (number->string (get-inc-counter)))
			(test-pe (car pe))
		 (then-pe (cadr pe))
		  (else-pe (caddr pe))
		  (test-gen (code-gen test-pe depth))
		  (then-gen (code-gen then-pe depth))
		  (else-gen (code-gen else-pe depth)))
				(string-append
					"# ----- gen-if -----\n"  
					test-gen "cmp rax, [" false-labl "]\n"
					"je else_" counter "\n"
					"then_" counter ":\n" then-gen
					"jmp if_exit_" counter "\n"
					"else_" counter ":\n" else-gen
					"if_exit_" counter ":\n\n"))
					))

(define gen-or
	(lambda (pe depth)
		(let* ((counter (number->string (get-inc-counter))))
			(string-append
				"# ----- gen-or -----\n"   
				(fold-left 
					(lambda (init curr)
						(string-append init (code-gen curr depth) 
							"cmp rax, [" false-labl "]\n"
							"jne or_exit_" counter "\n"))
					"" pe)
				 "or_exit_" counter ":\n\n"))))


(define gen-tc-applic
	(lambda (rator rands depth)
		(let* ((rator-code (code-gen rator depth))
			(num-of-params (number->string (length rands)))
			(counter (number->string (get-inc-counter)))
			(push-rands-code (string-append "push qword 0\n"
										(fold-left (lambda (init curr)
											(string-append 
												(code-gen curr depth)
												"push rax\n" init)) "" rands))))
		;	(display `(--- new-rands ,new-rands ---)) (newline)
			(string-append
				"# ----- gen-tc-applic " num-of-params " -----\n" 
				 rator-code

				 "tc_applic_" counter ":\n"
				; put the pointer to rator's closure SOB in r8
				"mov r8, rax\n"
				"mov r9, r8\n"

				; r8 = closure code, r9 = closure env
				"CLOSURE_CODE r8\n"
				"CLOSURE_ENV r9\n"
				; r10 - old return address
				"mov r10, qword [rbp + 1*8]\n"
				; r11 - old rbp
				"mov r11, qword [rbp]\n"

				; r12 - counter to old frame
				"mov r12, qword [rbp + 3*8]\n"
				"add r12, 4\n"

				; push rands
				push-rands-code

				; push num-of-params
				"push " num-of-params "\n"

				; push env
				"push r9\n"
				; push old return address
				"push r10\n"
				; r13 - counter to new frame
				"mov r13, qword [rsp + 2*8]\n"
				"add r13, 3\n"

				

				"\n.loop:\n"
				"cmp r13, -1\n"
				"je .end_loop\n\n"
				"mov r14, [rsp + 8*r13]\n"
				"mov [rbp + 8*r12], r14\n"
				"dec r12\n"
				"dec r13\n"
				"jmp .loop\n"

				"\n.end_loop:\n"
				"mov rsp, rbp\n"
				"mov rax, 8\n"
				"inc r12\n"
				"mul r12\n"
				"add rsp, rax\n"
				"mov r12, [rsp]\n"
				"mov rbp, r11\n"
				"jmp r8\n\n"

				))))

(define gen-applic
	(lambda (rator rands depth)
		(let* ((rator-code (code-gen rator depth))
			(num-of-params (number->string (length rands)))
			(counter (number->string (get-inc-counter)))
			(push-rands-code (string-append "push qword 0\n"
										(fold-left (lambda (init curr)
											(string-append 
												(code-gen curr depth)
												"push rax\n" init)) "" rands))))
		;	(display `(--- new-rands ,new-rands ---)) (newline)
			(string-append
				"# ----- gen-applic " num-of-params " -----\n" 
				 rator-code
				 "pushall\n"
				; put the pointer to rator's closure SOB in r8
				"mov r8, rax\n"
				"mov r9, r8\n"

				; r8 = closure code, r9 = closure env
				"CLOSURE_CODE r8\n"
				"CLOSURE_ENV r9\n"
				
				; push rands
				push-rands-code

				; push num-of-params
				"push " num-of-params "\n"

				; push env
				"push r9\n"

				"call r8\n"

				"mov r10, [rsp + 1*8]\n"
				; pop env, pop n, pop params
				"add rsp, 8*2\n"
				"mov r11, rax\n"
				"mov rax, 8\n"
				"mul r10\n"
				"add rsp, rax\n"
				"mov rax, r11\n"
				; pop 0 
				"add rsp, 8\n"
				"popall\n"
				))))

(define gen-lambda-opt
	(lambda (pe depth)
		(let* ((params `(,@(car pe)))
			(num-of-reg-params (number->string (length params)))
			(body (caddr pe))
			(lambda-pe (list params body))
			(fix-stack-code (string-append 
									"pushall\n"
									; r9 - counter
									"mov r9, " num-of-reg-params "\n"							
									
									"mov r8, qword [rbp + 4*8 + r9*8]\n"
									
									;if r8 == 0 then there are no elements in lambda opt's param list
									"cmp r8, 0\n"
									"je .push_nil\n"
									
									".search_list_end:\n"
									"inc r9\n"
									"cmp qword [rbp + 4*8 + r9*8], 0\n"
									"jne .search_list_end\n"

									; rbx - size of shift up
									"mov rbx, r9\n"
									"mov rcx, " num-of-reg-params "\n"
									"sub rbx, rcx\n"
									

									; rbp + 4*8 +r9*8 points to the end of the list (nil)
									; r11 - nil
									"mov r11, [" nil-labl "]\n"

									; r12 holds prev pair (nil at the beginning)
									"mov rdi, 8\n"
									"call malloc\n"
									"mov r12, rax\n"
									"mov [r12], r11\n"

									"dec r9\n"

									".build_list:"
									; if r9 == num-of-reg-params, building the list is done
									"cmp r9, " num-of-reg-params "-1 \n"
									"je .push_list\n"

									; r10 - address of curr list element
									"mov rdi, 8\n"
									"call malloc\n"
									"mov r10, rax\n"
									"mov r11, qword [rbp + 4*8 + r9*8]\n"
									"mov [r10], r11\n"

									; r11 - pointer to new pair
									"mov rdi, 8\n"
									"call malloc\n"
									"mov r11, rax\n"
									"make_lit_pair_runtime r11, r10, r12\n"

									; move the pointer to the new pair to r12 and countinue looping
									"mov r12, r11\n"

									"dec r9\n"
									"jmp .build_list\n"

									".push_list:\n"
									; r13 - the list
									"mov r13, qword [r12]\n"
									"mov r15, " num-of-reg-params "\n"

									"mov [rbp + 4*8 + r15*8], r13\n"
									; in case that there is only 1 extra param, do not shift the stack
									"cmp rbx, 1\n"
									"jg .shift_stack_up\n"

									"jmp .done\n"

									".shift_stack_up:\n"
									; rbx - size of shift
									"dec rbx\n"
									; r15 - pointer to stack element to shift
									"add r15, 4\n"

									".shift_up_loop:\n"
									"cmp r15, 0\n"
									"jl .shift_up_done\n"
									; r14 - element to shift
									"mov r14, qword [rbp + r15*8]\n"
									; shift
									"mov rdx, r15\n"
									"add rdx, rbx\n"
									"mov [rbp + rdx*8], r14\n"
									"dec r15\n"
									"jmp .shift_up_loop\n"

									".shift_up_done:\n"
									; shift up rsb,rbp as well
									"mov rax, 8\n"
									"mul rbx\n"
									"add rbp, rax\n"
									"add rsp, rax\n"

									"jmp .done\n"


									".push_nil:\n"

									; r8 - pointer of top element in stack to be shifted
									"mov r8, " num-of-reg-params "\n"
									"add r8, 3\n"
									; r9 - counter
									"mov r9, 0\n"

									".shift_stack_down:\n"
									"cmp r9, r8\n"
									"jg .shift_down_done\n"
									; r10 - current element to be shifted down
									"mov r10, [rbp + r9*8]\n"
									; r11 - stack pointer to the shifted location
									"mov r11, r9\n"
									"dec r11\n"
									"mov [rbp + r11*8], r10\n"

									"inc r9\n"
									"jmp .shift_stack_down\n"

									".shift_down_done:\n"
									"mov r10, [" nil-labl "]\n"
									"mov [rbp + r8*8], r10\n"
									; shift rsp,rbp as well
									"sub rsp, 8\n"
									"sub rbp, 8\n"

									".done:\n"

									; inc num of params in the stack by 1 
									"mov r15, " num-of-reg-params "\n"
									"inc r15\n"
									"mov [rbp + 3*8], r15\n"

									"popall\n"
									)))
		;(display `(asIF: ,lambda-pe num-of-params: ,num-of-reg-params)) (newline)
		(gen-lambda lambda-pe depth #t fix-stack-code))))

(define gen-lambda-simple
	(lambda (pe depth)
		(gen-lambda pe depth #f "")))

(define gen-lambda 
	(lambda (pe depth is-lambda-opt fix-stack-code)
		(let* ((code-label (string-append "lambda_code_label_" (number->string (get-inc-counter))))
				(str (string-append 
					(gen-closure-code depth code-label)

					"jmp " code-label "_end\n"
					"\n" code-label ":\n"
					"push rbp\n"
					"mov rbp, rsp\n"
					(if is-lambda-opt fix-stack-code "")
					(code-gen (cadr pe) depth)
					"leave\n"
					"ret\n\n"
					 code-label "_end:\n\n"
					)))
		str)))

(define gen-closure-code
	(lambda (depth code-label)
		(let* ((env_size (number->string (* 8 depth)))
			(n_offset (number->string (* 8 3)))
			(counter (number->string (get-inc-counter)))
			(params_offset (number->string (* 8 4)))
			(prev_env_offset (number->string (* 8 2)))
			(str-depth (number->string depth))
		(str (string-append
			"# ----- gen-lambda-" counter " depth: " str-depth " -----\n"
			"closure_" counter ":\n\n"
			"gen_closure " env_size ", " str-depth ", " n_offset ", " params_offset ", " prev_env_offset ", " code-label "\n"
			)))
		str)))

(define gen-const-asm-code
	(lambda (details)
		(let ((type (symbol->string (car details))))
			  (cond ((equal? type "T_UNDEFINED")
			  			"dq SOB_UNDEFINED")
		  			((equal? type "T_VOID")
		  				"dq SOB_VOID")
		  			((equal? type "T_BOOLEAN")
		  				(if (equal? (cadr details) 0)
		  					"dq SOB_FALSE"
		  					"dq SOB_TRUE"))
		  			((equal? type "T_NIL")
		  				"dq SOB_NIL")
		  			((equal? type "T_INTEGER")
		  				(string-append "dq MAKE_LITERAL(T_INTEGER, " (number->string (cadr details)) ")"))
		  			((equal? type "T_CHAR")
		  				(string-append "dq MAKE_LITERAL(T_CHAR, " (number->string (cadr details)) ")"))
		  			((equal? type "T_PAIR")
		  				(string-append "dq MAKE_LITERAL_PAIR(" (list-ref details 1) ", " (list-ref details 2) ")"))
		  			((equal? type "T_FRACTION")
		  				;(display (list-ref details 1)) (newline) (display (list-ref details 2)) (newline)
		  				(string-append "dq MAKE_LITERAL_FRACTION(" (number->string (list-ref details 1)) " ," (number->string (list-ref details 2)) ")"))
		  			((equal? type "T_VECTOR")
                        (if (equal? (cadr details) 0)
                            "dq MAKE_LITERAL(T_VECTOR, 0)"
                            (fold-left (lambda (init rest)
                                (string-append init ", " rest)) 
                                (string-append "MAKE_LITERAL_VECTOR " (list-ref details 2))
                                (cdddr details))))
		  			((equal? type "T_STRING")
		  				(string-append "MAKE_LITERAL_STRING \"" (caddr details) "\""))
		  			((equal? type "T_SYMBOL")
                        (string-append "MAKE_LITERAL_SYMBOL \"" (symbol->string (cadr details)) "\""))
			  ))))

(define get-const-label
	(lambda (val)
		(let* ((vals (map cadr constants-table))
			(labels (map car constants-table))
			(index (index-of vals val))
			(label (list-ref labels index)))
		label
		)))

(define gen-const
	(lambda (val depth)	
		(let ((label (get-const-label val)))
			(string-append "mov rax, [" label "]\n"))))
