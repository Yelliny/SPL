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
		("mult" NULL *)
    ("divi" NULL /)
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


(define compile-scheme-file
	(lambda (input output)
		(let* ((program (pipeline (file->list input))))
			(display program) (newline)
			(build-constants-table program)
			(build-globals-set-table program)
			;(display constants-table) (newline)
			(display symbol-table) (newline)
			;(display global-table) (newline)
			(call-with-output-file output
			(lambda (a)
				(display
					(overall-code-gen program)
				a)) 'replace))))
