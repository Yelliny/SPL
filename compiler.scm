(load "sexpr-parser.scm")
(load "tag-parser.scm")
(load "semantic-analyzer.scm")
(load "code-gen.scm")

(set! constants-table 
	`(
		("L0" 'undefined (T_UNDEFINED) 0)
		("L1" ,(void) (T_VOID) 1)
		("L2" () (T_NIL) 2)
		("L3" #t (T_BOOLEAN 1) 3)
		("L4" #f (T_BOOLEAN 0) 5)
		))
(set! constants-index 7)

(set! counter-label 5)

(set! global-table 
	'(
		("plus" NULL +)
		("cons" NULL cons)
		("minus" NULL -)
        ("null?" NULL null?)
        ("mult" NULL *)
        ("divi" NULL /)
        ("car" NULL car)
        ("cdr" NULL cdr)
        ("lower" NULL <)
        ("greater" NULL >)
        ("equali" NULL =)
        ("boolean?" NULL boolean?)
        ("char?" NULL char?)
        ("integer?" NULL integer?)
        ("pair?" NULL pair?)
        ("number?" NULL number?)
        ("procedure?" NULL procedure?)
        ("string?" NULL string?)
        ("symbol?" NULL symbol?)
        ("vector?" NULL vector?)
        ("char_to_integer" NULL char->integer)
        ("denominator" NULL denominator)
        ("integer_to_char" NULL integer->char)
        ("make_string" NULL make-string)
        ("make_vector" NULL make-vector)
        ("numerator" NULL numerator)
        ("string_length" NULL string-length)
        ("vector_length" NULL vector-length)
        ("string_ref" NULL string-ref)
        ("vector_ref" NULL vector-ref)
        ("not" NULL not)
        ("set_car" NULL set-car!)
        ("set_cdr" NULL set-cdr!)
        ("remainder" NULL remainder)
        ("string_set" NULL string-set!)
        ("vector_set" NULL vector-set!)
        ("apply" NULL apply)
        ("eq?" NULL eq?)
        ("vector" NULL vector)
		))
(set! symbol-table 
	'(
	))


(define runtime-support
	'(append apply not < = > + / * -
	  boolean? car cdr char->integer
	  char? cons denominator eq?
	  integer? integer->char list
      make-string make-vector map
      not null? number? numerator
	  pair? procedure? rational?
	  remainder set-car! set-cdr!
	  string-length string-ref
	  string-set! string->symbol
	  string? symbol? symbol->string
	  vector vector-length vector-ref
	  vector-set! vector? zero?
))

(define pipeline
	(lambda (s)
		((star <sexpr>) s
			(lambda (m r)
			(map (lambda (e)
			(annotate-tc
			(pe->lex-pe
			(box-set
			(remove-applic-lambda-nil
			(parse e))))))
			m))
			(lambda (f) 'fail))))

(define runtime-pipeline
	(lambda (s)
		(annotate-tc 
		(pe->lex-pe 
		(box-set 
		(remove-applic-lambda-nil 
		(parse s)))))))

(define file->list
	(lambda (in-file)
	(let ((in-port (open-input-file in-file)))
	(letrec ((run
	(lambda ()
	(let ((ch (read-char in-port)))
	(if (eof-object? ch)
	(begin
	(close-input-port in-port)
	'())
	(cons ch (run)))))))
	(run)))))


(define insert-to-table
	(lambda (val details)
		(let ((label (get-label))
			(address constants-index))
		(set! constants-table 
			`(,@constants-table ,(list label val details address)))
		(set! constants-index (+ constants-index (length details)))
		label)))

(define fraction?
	(lambda (num)
		(and (number? num) (not (integer? num)))))

(define get-label
	(lambda ()
	(let ((label (string-append "L" (number->string counter-label))))
		(set! counter-label (+ counter-label 1))
		label)))

(define insert-constant
	(lambda (const)
		(let ((labels (map car constants-table))
			(vals (map cadr constants-table)))
		(cond
			((null? const) 				
				(list-ref labels (index-of vals const)))
			((fraction? const)
				(if (not (member const vals))
					(insert-to-table const `(T_FRACTION ,(numerator const) ,(denominator const)))
					(list-ref labels (index-of vals const))))
			((number? const)
				(if (not (member const vals))
					(insert-to-table const `(T_INTEGER ,const))
					(list-ref labels (index-of vals const))))
			((boolean? const)
				 (list-ref labels (index-of vals const))) 
			((char? const)
				(if (not (member const vals))
					(insert-to-table const `(T_CHAR ,(char->integer const)))
					(list-ref labels (index-of vals const))))
			((list? const)
				(if (not (member const vals)) 
				(let*
					((car-label (insert-constant (car const)))
					(cdr-label (insert-constant (cdr const))))
					(insert-to-table const `(T_PAIR ,car-label ,cdr-label)))
				(list-ref labels (index-of vals const))))
            ((pair? const)
                (if (not (member const vals)) 
				(let*
					((car-label (insert-constant (car const)))
					(cdr-label (insert-constant (cdr const))))
					(insert-to-table const `(T_PAIR ,car-label ,cdr-label)))
				(list-ref labels (index-of vals const))))
			((vector? const)
				(if (not (member const vals))
					(let ((elem-labels (map insert-constant (vector->list const))))
						(insert-to-table const `(T_VECTOR ,(vector-length const) ,@elem-labels)))
					(list-ref labels (index-of vals const))))
			((string? const)
				(if (not (member const vals))
					(let ((chars-ascii (map char->integer (string->list const))))
						(insert-to-table const `(T_STRING ,(string-length const) ,const ,@chars-ascii)))
					(list-ref labels (index-of vals const))))
			((symbol? const)
				(insert-symbol const))
			((eq? (void) const)
				(list-ref labels (index-of vals const)))
			(else "L0")))))

(define insert-global
	(lambda (glob tag)
		(let* ((var (symbol->string (cadar glob)))
			(val (cadr glob))
			(vars (map car global-table)))
			(if (not (member var vars))
				(set! global-table `(,@global-table ,(list var val tag))))		
		)))

(define search-constant
	(lambda (pe) 
		(cond
		 ((and (list? pe) (not (null? pe)) (equal? (car pe) 'const))
					(insert-constant (cadr pe)))
		 ((list? pe) (map search-constant pe)))))

(define search-global-set
	(lambda (pe) 
		;(display `(inSearch: ,pe)) (newline)
		(cond
		 ((and (list? pe) (not (null? pe)) (equal? (car pe) 'define))
					(insert-global (cdr pe) 'define)
					(insert-symbol (cadadr pe))
					(search-global-set (caddr pe))
					)
		 ((and (list? pe) (not (null? pe)) (equal? (car pe) 'set) (equal? (caadr pe) 'fvar))
		 	(insert-global (cdr pe) 'set)
		 	(insert-symbol (cadadr pe))
		 	(search-global-set (caddr pe)))
		 ((list? pe) (fold-left (lambda (init curr) (search-global-set curr)) '() pe)))))

(define insert-symbol
	(lambda (pe)
		(let ((str-label (insert-constant (symbol->string pe))))
			(set! symbol-table 
			`(,@symbol-table ,(list pe str-label))))))

	
(define build-constants-table
	(lambda (program)
		(map search-constant program)))

(define build-globals-set-table
	(lambda (program)
		(map search-global-set program)))

(define runtime-functions-scm-code
	(lambda ()
		(map runtime-pipeline 
			(list 
				'(define append (lambda a 
									(let ((res (car a))
											(to-add-list (cdr a)))
										(letrec ((loop (lambda (curr to-add)
															(if (null? to-add)
																(add_to_list curr '())
																(loop (add_to_list curr (car to-add)) (cdr to-add))))))
											(loop res to-add-list)
															))))
				'(define add_to_list (lambda (src to-add)
										(letrec ((loop (lambda (orig)
															(if (null? orig) 
																to-add 
																(cons (car orig) (loop (cdr orig)))))))
										(loop src))))
										
                '(define zero? (lambda (n) (= n 0)))
										
                '(define list (lambda l
                                    (letrec ((loop
                                                (lambda (lst)
                                                    (if (null? lst)
                                                        '()
                                                        (cons (car lst) (loop (cdr lst)))))))
                                            (loop l))))
                                            
                '(define rational? number?)
                
                '(define list_length (lambda (lst)
                                        (letrec ((loop
                                                    (lambda (l)
                                                        (if (null? l)
                                                            0
                                                            (+ 1 (list_length (cdr l)))))))
                                                (loop lst))))
                                                
               '(define mymapyuval 
                    (lambda (f lsts)
                        (letrec ((loop
                                    (lambda (lsts)
                                        (if (null? lsts)
                                            '()
                                            (cons (f (car lsts))
                                                    (loop (cdr lsts)))))))
                                (loop lsts))))
                                                
                '(define map 
                    (lambda (f first . rest)
                        (letrec ((loop
                                    (lambda (lsts)
                                        (if (one_is_null? lsts)
                                            '()
                                            (cons (apply f (mymapyuval car lsts))
                                                    (loop (mymapyuval cdr lsts)))))))
                                (loop (cons first rest)))))

                    
                '(define one_is_null?
                    (lambda (lsts)
                        (letrec ((loop 
                                    (lambda (lsts)
                                        (and (not (null? lsts))
                                            (if (null? (car lsts))
                                                #t
                                                (loop (cdr lsts)))))))
                                (loop lsts))))

				
				))))


(define compile-scheme-file
	(lambda (input output)
		(let* ((file (file->list input))
			(program (pipeline file))
			(runtime (runtime-functions-scm-code))
			(merged-program (append runtime program)))
			;(display runtime) (newline) (newline)
			;(display program) (newline) (newline)
			;(display merged-program) (newline) (newline)
			(build-constants-table merged-program)
			(build-globals-set-table merged-program)
			;(display constants-table) (newline)
			;(display symbol-table) (newline)
			;(display global-table) (newline)
			(call-with-output-file output
			(lambda (a)
				(display
					(overall-code-gen merged-program)
				a)) 'replace))))
