(define index-of
	(lambda (lst elem)
		(cond 
			((null? lst) (display `(not-found-elem-is ,elem)) -1)
			((and (null? elem) (null? (car lst))) 0)
			((equal? (car lst) elem) 0)
			((let ((indx (index-of (cdr lst) elem)))
				(if (= -1 indx)
					-1
					(1+ indx)))))))

(define remove-applic-lambda-nil
  (lambda (exp)
  		(cond 
  			((or (null? exp) (not (list? exp))) exp)
  			((and 
  				(equal? (car exp) 'applic)
  				(equal? (caadr exp) 'lambda-simple)
  				(null? (cadadr exp))
  				(null? (caddr exp)))
  					(remove-applic-lambda-nil (caddar (cdr exp))))
  			(else (map remove-applic-lambda-nil exp))
  )))

(define box-set
  (lambda (e)
  	(cond 
  		((or (null? e) (not (list? e))) e)
  		((and (list? e) (equal? (car e) 'lambda-simple)) 
  			(let* ((params (cadr e))
  					(body (caddr e))
  					(box-params (filter (lambda (p) (need-box? p body)) params))
  					(sets (map (lambda (v) `(set (var ,v) (box (var ,v)))) box-params))
  					(new-body (rebody box-params (box-set body))))
  				`(lambda-simple ,params 
  					,(if (null? sets)
  					 new-body 
  					 `(seq (,@sets ,@(if (equal? (car new-body) 'seq) (cadr new-body) (list new-body))))))))
  		((and (list? e) (equal? (car e) 'lambda-opt))
  			(let* ((except-last (cadr e))
 					(last (caddr e))
 					(params (append except-last (list last)))
  					(body (cadddr e))
  					(box-params (filter (lambda (p) (need-box? p body)) params))
  					(sets (map (lambda (v) `(set (var ,v) (box (var ,v)))) box-params))
  					(new-body (rebody box-params (box-set body))))
  				`(lambda-opt ,except-last ,last
  				 ,(if (null? sets)
  				  new-body 
  				  `(seq (,@sets ,@(if (equal? (car new-body) 'seq) (cadr new-body) (list new-body))))))))
  		(else
  			(map box-set e)
  		))))


(define rebody
	(lambda (box-params body)
		(cond
			((or (null? body) (not (list? body))) body)
			((and
				(equal? (car body) 'var)
				(member (cadr body) box-params))
				`(box-get ,body))
			((and 
				(equal? (car body) 'set)
				(member (cadadr body) box-params))
				`(box-set ,(cadr body) ,(rebody box-params (caddr body))))
			((equal? (car body) 'lambda-simple)
 				(let* ((params (cadr body))
  					(body2 (caddr body))
  					(new-box-params (filter (lambda (p) (not (member p params))) box-params)))
 				`(lambda-simple ,params ,(rebody new-box-params body2))))
 			((equal? (car body) 'lambda-opt)
 				(let* 
 					((except-last (cadr body))
 					(last (caddr body))
 					(params (append except-last (list last)))
  					(body2 (cadddr body))
  					(new-box-params (filter (lambda (p) (not (member p params))) box-params)))
 				`(lambda-opt ,except-last ,last ,(rebody new-box-params body2))))
 			(else
 				(map (lambda (e) (rebody box-params e)) body)))))

(define need-box?
	(lambda (p body)
		(and
			(bound? p body)
			(rec-set? p body)
			(rec-get? p body))))

 (define bound?
 	(lambda (p body)
 		(cond 
 			((or (null? body) (not (list? body))) #f)
 			((equal? (car body) 'lambda-simple)
 				(let ((params (cadr body))
  					(body2 (caddr body)))
 				(if (not (member p params))
					(rec-bound? p body2)
 					#f)))
 			((equal? (car body) 'lambda-opt)
 				(let ((params (append (cadr body) (list (caddr body))))
  					(body2 (cadddr body)))
 				(if (not (member p params))
 					(rec-bound? p body2)
 					#f)))
 			(else
 			 (fold-left 
 				(lambda (init rest) (or init (bound? p rest)))
 				#f body)))))

 (define rec-bound?
 	(lambda (p body)
 		(cond 
 			((or (null? body) (not (list? body))) #f)
 			((and (equal? (car body) 'var) 
 				(equal? (cadr body) p)) #t)
 			((equal? (car body) 'lambda-simple)
 				(let ((params (cadr body))
  					(body2 (caddr body)))
 				(if (not (member p params))
 					(rec-bound? p body2)
 					#f)))
 			((equal? (car body) 'lambda-opt)
 				(let ((params (append (cadr body) (list (caddr body))))
  					(body2 (cadddr body)))
 				(if (not (member p params))
 					(rec-bound? p body2)
 					#f)))
 			(else
 			 (fold-left 
 				(lambda (init rest) (or init (rec-bound? p rest)))
 				#f body)))))

(define rec-set?
 	(lambda (p body)
 		(cond 
 			((or (null? body) (not (list? body))) #f)
 			((and (equal? (car body) 'set)
 				(equal? (cadadr body) p)) #t)
 			((equal? (car body) 'lambda-simple)
 				(let ((params (cadr body))
  					(body2 (caddr body)))
 				(if (not (member p params))
 					(rec-set? p body2)
 					#f)))
 			((equal? (car body) 'lambda-opt)
 				(let ((params (append (cadr body) (list (caddr body))))
  					(body2 (cadddr body)))
 				(if (not (member p params))
 					(rec-set? p body2)
 					#f)))
 			(else
 			 (fold-left 
 				(lambda (init rest) (or init (rec-set? p rest)))
 				#f body)))))

(define rec-get?
 	(lambda (p body)
 		(cond 
 			((or (null? body) (not (list? body))) #f)
 			((and (equal? (car body) 'set)
 				(equal? (cadadr body) p)) 
 					(rec-get? p (caddr body)))
 			((and (equal? (car body) 'var)
 				(equal? (cadr body) p)) #t)
 			((equal? (car body) 'lambda-simple)
 				(let ((params (cadr body))
  					(body2 (caddr body)))
 				(if (not (member p params))
 					(rec-get? p body2)
 					#f)))
 			((equal? (car body) 'lambda-opt)
 				(let ((params (append (cadr body) (list (caddr body))))
  					(body2 (cadddr body)))
 				(if (not (member p params))
 					(rec-get? p body2)
 					#f)))
 			(else
 			 (fold-left 
 				(lambda (init rest) (or init (rec-get? p rest)))
 				#f body)))))




(define pe->lex-pe
  (lambda (e)
  		(lexi e '() '())
  ))

(define lexi
	(lambda (exp bounds params)
		(cond
		 ((or (null? exp) (not (list? exp))) exp)
		 ((equal? (car exp) 'lambda-simple)
		 	(let 
		 		((new-bounds (cons params bounds))
		 		(new-params (cadr exp))
		 		(body (caddr exp)))
		 		`(lambda-simple ,new-params ,(lexi body new-bounds new-params))))
		 ((equal? (car exp) 'lambda-opt)
		 	(let* 
		 		((new-bounds (cons params bounds))
		 		(except-last (cadr exp))
		 		(last (caddr exp))
		 		(new-params (append except-last (list last)))
		 		(body (cadddr exp)))
		 		`(lambda-opt ,except-last ,last ,(lexi body new-bounds new-params))))
		 ((equal? (car exp) 'var)
		 	(let ((var (cadr exp)))
		 			(if (in-params? var params)
		 				(let ((loc (params-location var params)))
		 					`(pvar ,var ,loc))
		 				(if (in-bounds? var bounds)
		 					(let ((loc (bound-location var bounds 0)))
		 						`(bvar ,var ,(car loc) ,(cdr loc)))
		 					`(fvar ,var)))))
		 (else
		 	(map (lambda (e) (lexi e bounds params)) exp)))))

(define in-bounds?
	(lambda (var bounds)
		(fold-left (lambda (init rest) (or init (member var rest))) #f bounds)))

(define bound-location
	(lambda (var bounds lstnum)
		(if (member var (car bounds))
			(cons lstnum (index-of (car bounds) var))
			(bound-location var (cdr bounds) (+ 1 lstnum)))))

(define in-params?
	(lambda (var params)
		(member var params)))

(define params-location
	(lambda (var params)
		(index-of params var)))

(define annotate-tc
  (lambda (e)
  	(atp e #f))
  )

(define atp
 	(lambda (tree tp?)
 		(cond 
 			((or (null? tree) (not (list? tree))) tree)
 			((equal? (car tree) 'or) 
 				`(or (
 				,@(map (lambda (e) (atp e #f)) (besides-last (cadr tree)))
 				,(atp (last-element (cadr tree)) tp?))))
 			((equal? (car tree) 'if3)
 				`(if3 ,(atp (cadr tree) #f) ,(atp (caddr tree) tp?) ,(atp (cadddr tree) tp?)))
 			((equal? (car tree) 'seq)
 				`(seq (
 				,@(map (lambda (e) (atp e #f)) (besides-last (cadr tree)))
 				,(atp (last-element (cadr tree)) tp?))))
 			((equal? (car tree) 'lambda-simple)
 				`(lambda-simple ,(cadr tree) ,(atp (caddr tree) #t)))
 			((equal? (car tree) 'lambda-opt)
 				`(lambda-opt ,(cadr tree) ,(caddr tree) ,(atp (cadddr tree) #t)))
 			((equal? (car tree) 'applic)
 				(if tp?
 				`(tc-applic ,(atp (cadr tree) #f) ,(map (lambda (e) (atp e #f)) (caddr tree)))
 				`(applic ,(atp (cadr tree) #f) ,(map (lambda (e) (atp e #f)) (caddr tree)))))
 			((or (equal? (car tree) 'box-set) (equal? (car tree) 'set) (equal? (car tree) 'define))
 				`(,(car tree) ,(cadr tree) ,(atp (caddr tree) #f)))
 			(else (map (lambda (e) (atp e tp?)) tree)))))

(define besides-last
	(lambda (lst) 
		(if (equal? (cdr lst) '())
			'()
			(cons (car lst) (besides-last (cdr lst))))))

(define last-element
	(lambda (lst)
		(if (equal? (cdr lst) '())
			(car lst)
			(last-element (cdr lst)))))



