;(load "qq.scm")

(define parse
  (lambda (expr)
  	(cond ((regular-const? expr) (list 'const expr))
  		  ((quote? expr) (list 'const (cadr expr)))
  		  ((if? expr) (handle-if (cdr expr)))
  		  ((or? expr) (handle-or (cdr expr)))
  		  ((reg-lambda? expr) (handle-reg-lambda (cdr expr)))
  		  ((lambda-opt? expr) (handle-lambda-opt (cdr expr)))
  		  ((lambda-var? expr) (handle-lambda-var (cdr expr)))
  		  ((define? expr) (handle-define (cdr expr)))
  		  ((define-mit? expr) (handle-define-mit (cdr expr)))
  		  ((set? expr) (handle-set (cdr expr)))
  		  ((and? expr) (handle-and (cdr expr)))
  		  ((begin? expr) (handle-begin (cdr expr)))
  		  ((let? expr) (handle-let (cdr expr)))
  		  ((let*? expr) (handle-let* (cdr expr)))
  		  ((letrec? expr) (handle-letrec (cdr expr)))
  		  ((cond? expr) (handle-cond (cdr expr)))
  		  ((quasiquote? expr) (handle-quasi (cdr expr)))
  		  ((applic? expr) (handle-applic expr))
  		  ((var? expr) (list 'var expr))
  		  (else "ERROR"))))

(define duplicates-for-let?
	(lambda (expr)
		(let ((seen '()) (vars (map car (second expr))))
				(fold-left
					(lambda (init var)
						(let ((ans (not (member var seen))))
							(set! seen (append seen (list var)))
							(and init ans))) #t vars))))

(define duplicates-for-lambda?
	(lambda (expr)
		(let ((seen '()) (vars (second expr)))
				(fold-left
					(lambda (init var)
						(let ((ans (not (member var seen))))
							(set! seen (append seen (list var)))
							(and init ans))) #t vars))))

(define duplicates-for-lambda-opt?
	(lambda (expr)
		(let ((seen '()) (vars (starting-list (second expr))))
				(fold-left
					(lambda (init var)
						(let ((ans (not (member var seen))))
							(set! seen (append seen (list var)))
							(and init ans))) #t vars))))

(define last
	(lambda (implst)
		(if (pair? implst)
			(last (cdr implst))
			implst)))

(define starting-list
	(lambda (implst)
		(if (pair? implst)
			(append (list (car implst)) (starting-list (cdr implst)))
			'())))

(define first car)

(define second cadr)

(define third caddr)

(define regular-const?
	(lambda (expr)
		(or (null? expr)
			(vector? expr)
			(char? expr)
			(number? expr)
			(string? expr)
			(boolean? expr))))

(define quote?
	(lambda (expr)
		(and (list? expr) (equal? (car expr) 'quote))))

(define const?
	(lambda (expr)
		(cond ((regular-const? expr) (list 'const expr))
			  ((quote? expr) (list 'const (cadr expr)))
			  (else #f))))

(define var?
	(lambda (expr) (if (list? expr) 
					(not (reserved-words? (car expr)))
					(not (reserved-words? expr)))))

(define reserved-words?
	(lambda (expr)
		(let ((words '(and begin cond define do else if lambda
					let let* letrec or quasiquote unquote
					unquote-splicing quote set!)))
		(fold-left
		   (lambda (init rest) (or init (equal? rest expr)))
			#f
		   words))))

(define if?
	(lambda (expr)
		(and (list? expr) (equal? (car expr) 'if) 
			(or (= 2 (length (cdr expr)))
			    (= 3 (length (cdr expr)))))))

(define handle-if
	(lambda (expr)
		(append 
			(list 'if3)
			(if (= (length expr) 3)
				(map  parse expr)
				(list (parse (first expr))
						(parse (second expr))
						(list 'const (void)))))))
(define or?
	(lambda (expr)
		(and (list? expr) (equal? (car expr) 'or))))

(define handle-or
	(lambda (expr)
			(cond 
				((= (length expr) 0) (parse #f))
				((= (length expr) 1) (parse (first expr)))
				(else (append (list 'or) (list (map parse expr)))))))

(define reg-lambda?
	(lambda (expr)
		(and (list? expr)
			 (equal? (car expr) 'lambda)
			 (>= (length expr) 3)
			 (list? (second expr))
			 (duplicates-for-lambda? expr)
			 (list? (cadr expr)))))

(define handle-reg-lambda
	(lambda (expr)
		(append 
			(list 'lambda-simple)
			(list (first expr))
			(list (parse (append (list 'begin) (cdr expr)))))))
		;	(if (> (length (cdr expr)) 1)
		;		(list (list 'seq (map parse (cdr expr))))
		;		(list (parse (second expr)))))))

(define lambda-opt?
	(lambda (expr)
		(and (list? expr)
			 (equal? (car expr) 'lambda)
			 (>= (length expr) 3)
			 (duplicates-for-lambda-opt? expr)
			 (and (not (list? (second expr))) (pair? (second expr))))))

(define handle-lambda-opt
	(lambda (expr)
		(append
			(list 'lambda-opt)
			(list (starting-list (first expr)))
			(list (last (first expr)))
			(list (parse (append (list 'begin) (cdr expr)))))))

(define lambda-var?
	(lambda (expr)
		(and (list? expr)
			 (equal? (car expr) 'lambda)
			 (>= (length expr) 3)
			 (not (pair? (cadr expr))))))

(define handle-lambda-var
	(lambda (expr)
		(append
			(list 'lambda-opt)
			(list '())
			(list (first expr))
			(list (parse (append (list 'begin) (cdr expr)))))))

(define define?
	(lambda (expr)
		(and (list? expr)
			(= (length expr) 3)
			(equal? (car expr) 'define)
			(not (pair? (second expr))))))

(define handle-define
	(lambda (expr)
		(append
			(list 'define)
			(list (parse (first expr)))
			(list (parse (second expr))))))

(define define-mit?
	(lambda (expr)
		(and (list? expr)
			(>= (length expr) 3)
			(equal? (car expr) 'define)
			(pair? (second expr)))))

(define handle-define-mit
	(lambda (expr)
		(append
			(list 'define)
			(list (parse (caar expr)))
			(list (parse (append
				(list 'lambda)
				(list (cdar expr))
				(cdr expr)))))))

(define set?
	(lambda (expr)
		(and (list? expr)
			(= (length expr) 3)
			(equal? (car expr) 'set!))))

(define handle-set
	(lambda (expr)
		(append
			(list 'set)
			(list (parse (first expr)))
			(list (parse (second expr))))))

(define applic?
	(lambda (expr)
		(and (list? expr)
			(>= (length expr) 1)
			(not (reserved-words? (car expr))))))

(define handle-applic
	(lambda (expr)
		(append
			(list 'applic)
			(list (parse (first expr)))
			(list (map parse (cdr expr))))))

(define and?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'and))))

(define handle-and
	(lambda (expr)
		(cond
			((= (length expr) 0) (parse #t))
			((= (length expr) 1) (parse (first expr)))
			(else (and-from-if expr)))))

(define and-from-if
	(lambda (expr)
		(if (= (length expr) 1)
			(parse (first expr))
			(append (list 'if3)
					(list (parse (first expr)))
					(list (and-from-if (cdr expr)))
					(list (parse #f))))))


(define begin?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'begin))))

(define handle-begin
	(lambda (expr)
		(if (> (length expr) 1)
				(list 'seq (map parse (remove-begin expr)))
				(if (= (length expr) 1)
					(parse (first expr))
					(list 'const (void))))))

(define remove-begin
	(lambda (expr)
		(if (equal? expr '())
			'()
			(if (list? (car expr))
				(if (equal? (caar expr) 'begin)
					(append (remove-begin (cdar expr)) (remove-begin (cdr expr)))
					(append (list (car expr)) (remove-begin (cdr expr))))
				(append (list (car expr)) (remove-begin (cdr expr)))))))

(define let?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'let)
			(>= (length expr) 3)
			(duplicates-for-let? expr))))


(define handle-let
	(lambda (expr)
		(let ((vars (map first (first expr)))
			  (vals (map second (first expr)))
			  (body (cdr expr)))
			(let ((result (append
					(list (append (list 'lambda) (list vars) body))
					vals)))
			(parse 
				result)))))

(define let*?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'let*)
			(>= (length expr) 3))))

(define handle-let*
	(lambda (expr)
		(let ((bindings (first expr))
			 (body (cdr expr)))
		(if (<= (length bindings) 1)
			(parse (append (list 'let) (list bindings) body))
		(let ((nested-let (nest-let bindings body)))
			(parse nested-let))))))

(define nest-let
	(lambda (bindings body)
		(if (equal? bindings '())
			body
			(let ((res (nest-let (cdr bindings) body)))
				(if (equal? (car res) 'let)
					(list 'let (list (first bindings)) res)
					(append (list 'let) (list (list (first bindings))) res))))))

(define letrec?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'letrec)
			(>= (length expr) 3)
			(let ((seen '()) (vars (map car (second expr))))
				(fold-left
					(lambda (init var)
						(let ((ans (not (member var seen))))
							(set! seen (append seen (list var)))
							(and init ans))) #t vars)))))

(define handle-letrec
	(lambda (expr)
		(let ((vars (map first (car expr)))
			  (vals (map second (car expr)))
			  (body (cdr expr)))
			(let ((result (append (list 'let) 
				(list (map (lambda (var) (list var #f)) vars))
				(map (lambda (var val) (list 'set! var val)) vars vals)
				(list (list (append (list 'lambda) (list '()) body))))))
				 (parse result)))))

(define cond?
	(lambda (expr)
		(and (list? expr)
			 (>= (length expr) 2)
			 (equal? (first expr) 'cond)
			 (fold-left (lambda (bool pair) 
			 	(and bool (>= (length pair) 2)))
			 	#t (cdr expr)))))


(define handle-cond
	(lambda (expr)
		(parse (cond-from-if expr))))

(define cond-from-if
	(lambda (expr)
		(if (= (length expr) 1)
			(if (equal? (caar expr) 'else)
				(append (list 'begin) (cdar expr))
				(append (list 'if) (list (caar expr)) (list (append (list 'begin) (cdar expr)))))
			(append (list 'if) 
				(list (caar expr)) 
				(list (append (list 'begin) (cdar expr)))
				(list (cond-from-if (cdr expr)))))))


(define quasiquote?
	(lambda (expr)
		(and (list? expr)
			(equal? (car expr) 'quasiquote)
			(= (length expr) 2))))

(define handle-quasi
	(lambda (expr)
		(parse (expand-qq (car expr)))))