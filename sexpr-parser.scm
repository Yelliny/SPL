;(load "pc.scm")

(define <sexpr>
	(new
	(*delayed (lambda () <CommentsAndSpaces>)) *star
	(*delayed (lambda () <Boolean>))
	(*delayed (lambda () <Char>))
	(*delayed (lambda () <Number>))
	(*delayed (lambda () <String>))
	(*delayed (lambda () <Symbol>))
	(*delayed (lambda () <ProperList>))
	(*delayed (lambda () <ImproperList>))
	(*delayed (lambda () <Vector>))
	(*delayed (lambda () <Quoted>))
	(*delayed (lambda () <QuasiQuoted>))
	(*delayed (lambda () <UnquoteAndSpliced>))
	(*delayed (lambda () <Unquoted>))
	(*delayed (lambda () <CBName>))
	(*delayed (lambda () <InfixExtension>))
	(*disj 14)
	(*delayed (lambda () <CommentsAndSpaces>)) *star
	(*caten 3)
	(*pack-with (lambda (c1 s c2) s))
	done))

(define <whitespace_1>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <PrefixComment>
	(new
	(*parser (const (lambda (ch) (char=? ch #\newline))))
	(*delayed (lambda () <lineComment_2>))
	(*delayed (lambda () <PrefixExprComment>))
	(*disj 3) *star
	done))

(define <EOL>
        (new
            (*parser (char #\newline))
            (*parser <end-of-input>)
            (*disj 2)
        done))

(define <InfixComment>
	(new
	(*delayed (lambda () <lineComment_2>))
	(*delayed (lambda () <InfixExprComment>))
	(*disj 2) *star
	done))

(define <lineComment_1>
        (new
            (*parser (char #\;))

            (*parser <any-char>)
            (*parser <EOL>)
            *diff *star

            (*parser <EOL>)
            (*caten 3)
        done))

(define <lineComment_2>
	(new
	(*parser (char #\;))
	(*delayed (lambda () <whitespace_2>))
	(*delayed (lambda () <any-char>)) 
	(*parser (const (lambda (ch) (char=? ch #\newline)))) 
	(*parser <end-of-input>)
	(*disj 2)
	*diff *star
	(*parser (const (lambda (ch) (char=? ch #\newline)))) 
	(*parser <end-of-input>)
	(*disj 2)
	(*caten 4)
	done))

(define <CommentHandler>
    (new
        (*parser (word "#;"))
        (*delayed (lambda () <sexpr>))
        (*caten 2)
    done))

(define <newline>
	(new
		(*parser (const (lambda (ch) (char=? ch #\newline))))
		done))

(define <InfixExprComment>
	(new
	(*parser (word "#;"))
	(*delayed (lambda () <whitespace_2>))
	(*delayed (lambda () <InfixExpression>))
	(*caten 3) 
	done))

(define <SexprComment>
  (disj <lineComment_1>
	<CommentHandler>))


(define <PrefixExprComment>
	(new
	(*parser (word "#;"))
	(*delayed (lambda () <whitespace_2>))
	(*parser <sexpr>)
	(*caten 3) 
	done))

	
(define <CommentsAndSpaces>
  (disj <SexprComment>
	<whitespace_1>)) 

(define debug (lambda (a) (display a) (newline) a))

(define <whitespace_2> ; returns empty string
	(new 
	(*parser (const (lambda (ch) (not (char>=? ch #\!))))) *star
	(*pack (lambda (res) "")) 
	done))

(define char->string
	(lambda(ch)
		(if (equal? ch '())
			""
			(list->string (list ch)))))

(define <Boolean>
	(new
	(*parser (word-ci "#t"))
	(*pack (lambda (r) #t))
	(*parser (word-ci "#f"))
	(*pack (lambda (r) #f))
	(*disj 2)
	done))

(define <Char>
	(new
	(*delayed (lambda () <CharPrefix>))
	
	(*delayed (lambda () <NamedChar>))
	(*delayed (lambda () <HexUnicodeChar>))
	(*delayed (lambda () <VisibleSimpleChar>))
	(*disj 3)
	
	(*caten 2)
	(*pack-with (lambda (prefix ch) ch))
	done))

(define <CharPrefix>
	(new
	(*parser (word-ci "#\\"))
	(*pack (lambda (r) ""))
	done))


; TODO check
(define <VisibleSimpleChar>
	(new
	(*parser (range #\! #\xFF))
	done))

(define <NamedChar>
	(new
	(*parser (word-ci "lambda"))
	(*pack (lambda (r) #\λ))
	(*parser (word-ci "newline"))
	(*pack (lambda (r) #\newline))
	(*parser (word-ci "nul"))
	(*pack (lambda (r) #\nul))
	(*parser (word-ci "page"))
	(*pack (lambda (r) #\page))
	(*parser (word-ci "return"))
	(*pack (lambda (r) #\return))	
	(*parser (word-ci "space"))
	(*pack (lambda (r) #\space))
	(*parser (word-ci "tab"))
	(*pack (lambda (r) #\tab))
	(*disj 7)
	done))

(define power 
	(lambda (n e)
		(if (= e 0)
			1
			(* n (power n (- e 1))))))

(define computeNum
	(lambda (num pow base)
		(if (null? num)
			0
			(+ 
				(* (car num) (power base pow))
				(computeNum (cdr num) (- pow 1) base)))))


(define <HexUnicodeChar>
	(new
		(*parser (char #\x))
		(*delayed (lambda () <HexChar>)) *plus
		(*pack (lambda (res) (integer->char (computeNum res (- (length res) 1) 16))))
		(*caten 2)
		(*pack-with (lambda (a b) b))
		done))
	

(define <HexChar> ;return number
	(new
	(*parser (range #\0 #\9))
	(*pack (lambda (ch) (- (char->integer ch) 48)))
	(*parser (range #\a #\f))
	(*pack (lambda (ch) (- (char->integer ch) 87)))
	(*parser (range #\A #\F))
	(*pack (lambda (ch) (- (char->integer ch) 55)))
	(*disj 3)
	done))

(define <Number> 
	(new
	(*delayed (lambda () <Fraction>))
	(*delayed (lambda () <Integer>))
	(*disj 2)
	(*parser (range #\a #\z))
	(*parser (range #\A #\Z))
	(*delayed (lambda () <Operator>))
	(*disj 3)
	*not-followed-by
	done))


(define <Integer> ;returns a number
	(new
	(*delayed (lambda () <Sign>))
	(*delayed (lambda () <Natural>))
	(*caten 2)
	(*pack-with (lambda (s n) (if (equal? s "-") (* n -1) n)))
	done))

(define <Sign>
	(new
	(*parser (char #\+))
	(*parser (char #\-))
	(*parser <epsilon>)
	(*disj 3)
	(*pack char->string)
	done))

(define <Natural>
	(new 
	(*parser (range #\0 #\9)) *plus
	(*pack (lambda (lst)
	 (computeNum (map (lambda (ch) (- (char->integer ch) 48)) lst)
	  (- (length lst) 1) 10)))
	done))

(define <Fraction>
	(new
	(*parser <Integer>)
	(*parser (char #\/))
	(*parser <Natural>)
	(*caten 3)
	(*pack-with (lambda (n1 s n2) (/ n1 n2)))
	done))

(define <String> 
	(new
	(*parser (char #\"))
	(*pack char->string)
	(*delayed (lambda () <StringChar>)) *star ;return list of strings
	(*pack-with string-append)
	(*parser (char #\"))
	(*pack char->string)
	(*caten 3)
	(*pack-with (lambda (q1 s q2) s))
	done))

(define <StringChar> ;returns string
	(new
	(*delayed (lambda () <StringLiteralChar>))
	(*delayed (lambda () <StringMetaChar>))
	(*delayed (lambda () <StringHexChar>))
	(*disj 3)
	done))

(define <StringLiteralChar> ;return string
	(new
	(*parser (const 
				(lambda (ch) (not (or (char=? ch #\\) (char=? ch #\"))))))
	(*pack char->string)
	done))

(define <StringMetaChar>  ;return string
	(new
	(*parser (word "\\\\"))
	(*pack (lambda (c) (string->list "\\"))) ;TODO - check why it should be \\ and not \\\\
	(*parser (word "\\\""))
	(*pack (lambda (c) (string->list "\"")))
	(*parser (word-ci "\\t"))
	(*pack (lambda (c) (list #\tab)))
	(*parser (word-ci "\\f"))
	(*pack (lambda (c) (list #\page)))
	(*parser (word-ci "\\n"))
	(*pack (lambda (c) (list #\newline)))
	(*parser (word-ci "\\r"))
	(*pack (lambda (c) (list #\return)))
	(*disj 6)
	(*pack list->string)
	done))

(define <StringHexChar> ;return string
	(new
	(*parser (word-ci "\\x"))
;	(*pack list->string)
	(*parser <HexChar>) *star
;	(*pack list->string)
	(*parser (char #\;))
;	(*pack char->string)
	(*caten 3)
	(*pack-with (lambda (s1 res s2) 
					(char->string (integer->char (computeNum res (- (length res) 1) 16)))))
	done))

(define <Symbol> 
	(new
	(*delayed (lambda () <SymbolChar>)) *plus
	(*pack list->string)
	(*pack string->symbol)
	done))

(define <SymbolChar> ; returns char
	(new
	(*parser (range #\0 #\9))
	(*parser (range #\a #\z))
	(*parser (range #\A #\Z))
	(*pack (lambda (ch) (integer->char (+ (char->integer ch) 32))))
	(*delayed (lambda () <Operator>))
	(*disj 4)
	done))


(define <Operator> ; returns char
	(new
	(*parser (char #\$))
	(*parser (char #\!))	
	(*parser (char #\^))
	(*parser (char #\*))
	(*parser (char #\-))
	(*parser (char #\_))
	(*parser (char #\=))
	(*parser (char #\+))
	(*parser (char #\>))
	(*parser (char #\<))
	(*parser (char #\?))
	(*parser (char #\/))
	(*disj 12)
	done))


(define <ProperList>
	(new
	(*parser (char #\())
	(*parser <whitespace_2>)
	(*parser <sexpr>) 
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (s1 s s2) s))
	*star
	(*pack-with list)
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (p1 l p2) l))
	done))

(define <ImproperList> 
	(new
	(*parser (char #\())

	(*parser <whitespace_2>)
	(*parser <sexpr>)
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (s1 s s2) s)) 
	*plus

	(*parser (char #\.))
	
	(*parser <whitespace_2>)
	(*parser <sexpr>)
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (s1 s s2) s)) 

	(*parser (char #\)))
	
	(*caten 5)
	(*pack-with (lambda (p1 s1 p2 s2 p3) (append s1 s2)))
	done))




(define <Vector> 
	(new
	(*parser (char #\#))
	(*parser (char #\())

	(*parser <whitespace_2>)
	(*parser <sexpr>)
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (s1 s s2) s))
	*star
	(*pack-with vector)

	(*parser (char #\)))

	(*caten 4)
	(*pack-with (lambda (s1 s2 v s3) v))
	done))




(define <Quoted> 
	(new
	(*parser (char #\'))
	(*parser <sexpr>)
	(*caten 2)
	(*pack-with (lambda (c s) (list 'quote s)))
	done))

(define <QuasiQuoted>
	(new
	(*parser (char #\`))
	(*parser <sexpr>)
	(*caten 2)
	(*pack-with (lambda (c s) (list 'quasiquote s)))
	done))

(define <Unquoted>
	(new
	(*parser (char #\,))
	(*parser <sexpr>)
	(*caten 2)
	(*pack-with (lambda (c s) (list 'unquote s)))
	done))

(define <UnquoteAndSpliced>
	(new
	(*parser (word ",@"))
	(*parser <sexpr>)
	(*caten 2)
	(*pack-with (lambda (c s) (list 'unquote-splicing s)))
	done))

(define <CBName>
	(new
	(*delayed (lambda () <CBNameSyntax1>))
	(*delayed (lambda () <CBNameSyntax2>))
	(*disj 2)
	done))

(define <CBNameSyntax1>
	(new
	(*parser (char #\@))
	(*parser <sexpr>)
	(*caten 2)
	(*pack-with (lambda (a b) (list 'cbname b)))
	done))

(define <CBNameSyntax2>
	(new
	(*parser (char #\{))
	(*parser <sexpr>)
	(*parser (char #\}))
	(*caten 3)
	(*pack-with (lambda (a b c) (list 'cbname b)))
	done))

(define <InfixExtension>
	(new
	(*delayed (lambda () <InfixPrefixExtensionPrefix>))
	(*parser <whitespace_2>)
	(*delayed (lambda () <InfixExpression>))
	(*caten 3)
	(*pack-with (lambda (p w e) e))
	done))


(define <InfixPrefixExtensionPrefix>
	(new
	(*parser (word "##"))
	(*parser (word "#%"))
	(*disj 2)
	done))

(define <InfixExpression>
	(new
	(*parser <InfixComment>)
	(*delayed (lambda () <InfixAddSub>))
	(*parser <InfixComment>)
	(*caten 3)
	(*pack-with (lambda (c1 e c2) e))
	done))

 (define <InfixNumber>
	(new
	(*delayed (lambda () <Fraction>))
	(*delayed (lambda () <Integer>))
	(*disj 2)
	(*delayed (lambda () <InfixSymbolChar>))
	*not-followed-by
	done))

 (define <InfixSymbolChar>
	(new
		(*parser <SymbolChar>)
		(*delayed (lambda () <NotInfixSymbol>))
		*diff
		done))


(define <NotInfixSymbol>
	(new
	(*parser (char #\+))
	(*parser (char #\-))
	(*parser (char #\*))
	(*parser (word "**"))
	(*parser (char #\^))
	(*parser (char #\/))
	(*parser (char #\[)) 
	(*parser (char #\]))
	(*disj 8)
	done))

(define <InfixSymbol>
	(new
	(*parser <InfixSymbolChar>) *plus
	(*pack (lambda (ls) (string->symbol (list->string ls))))
	;(*parser <NotInfixSymbol>)
	;*not-followed-by
	done))


(define <InfixNeg>
	(new
	(*parser <whitespace_2>)
	(*parser (char #\-))
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 m w2) m))

	
	(*delayed (lambda () <InfixFuncall>))
	(*delayed (lambda () <InfixArrayGet>))
	(*delayed (lambda () <InfixParen>))
	(*delayed (lambda () <InfixSymbol>))
	(*disj 4)

	(*pack (lambda (exp) (list '- exp)))

	(*delayed (lambda () <InfixNumber>))
	(*pack (lambda (num) (* -1 num))) 

	(*disj 2)

	(*caten 2)
	(*pack-with (lambda (sign res) res))
	done))

(define <InfixAtomic>
	(new
	(*parser <whitespace_2>)
	(*parser <InfixComment>)
	(*parser <whitespace_2>)

	(*parser <InfixNumber>)
	(*parser <InfixSymbol>)
	(*parser <InfixNeg>)
	(*disj 3)

	(*parser <whitespace_2>)
	(*parser <InfixComment>)
	(*parser <whitespace_2>)

	(*caten 7)
	(*pack-with (lambda (w1 c1 w2 tomic w3 c2 w4) tomic))
	done))

(define <InfixSexprEscape>
	(new
	
	(*parser <whitespace_2>)
	(*parser <PrefixComment>)
	(*parser <whitespace_2>)
	(*caten 3)

	(*parser <InfixPrefixExtensionPrefix>)

	(*parser <whitespace_2>)
	(*parser <PrefixComment>)
	(*parser <whitespace_2>)
	(*caten 3)

	(*parser <sexpr>)
	(*caten 4)
	(*pack-with (lambda (wcw1 pref wcw2 expr) expr))

	(*delayed (lambda () <InfixAtomic>))
	(*disj 2)

	done))

(define <InfixParen>
	(new

	(*parser <InfixSexprEscape>)

	;(*parser <InfixAtomic>)

	(*parser <whitespace_2>)
	(*parser (char #\())
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*parser <InfixExpression>)

	(*parser <whitespace_2>)
	(*parser (char #\)))
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*caten 3)
	(*pack-with (lambda (p1 exp p2) exp))

	(*disj 2)
	done))

(define <InfixArrayGet>
	(new
	(*parser <InfixParen>)
;	(*delayed (lambda () <InfixFuncall>))
;	(*disj 2)
	
	(*parser <whitespace_2>)
	(*parser (char #\[))
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*parser <InfixExpression>)

	(*parser <whitespace_2>)
	(*parser (char #\]))
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*caten 4)
	(*pack-with (lambda (vec p1 idx p2) (list 'vector-ref vec idx)))


	(*parser (char #\[))
	;(*parser <InfixAtomic>)
	(*parser <InfixExpression>)
	(*parser (char #\]))
	(*caten 3) *star
	(*caten 2)
	(*pack-with (lambda (firstlst restlst)
				 (fold-left 
				 	(lambda (a b) (list 'vector-ref a (cadr b)))
				 	firstlst
				 	restlst)))

	(*parser (char #\())
	(*delayed (lambda () <InfixArgList>))
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (p1 lst p2) lst))
	
	(*parser (char #\[))
	;(*parser <InfixAtomic>)
	(*parser <InfixExpression>)
	(*parser (char #\]))
	(*caten 3)
	(*pack-with (lambda (p1 lst p2) (list 'vector-ref lst)))

	(*disj 2)
	*star

	(*caten 2)
	(*pack-with (lambda (arr argLists)
		(fold-left 
			(lambda (a b) 
				(if (equal? (car b) 'vector-ref)
					(list 'vector-ref a (cadr b))
					(append (list a) b)))
					arr argLists)))

	(*parser <InfixParen>)

	(*disj 2)

	done))



(define <InfixFuncall>
	(new
	(*parser <InfixParen>)
	(*parser <InfixArrayGet>)
	(*disj 2)

	(*parser <whitespace_2>)
	(*parser (char #\())
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*delayed (lambda () <InfixArgList>))

	(*parser <whitespace_2>)
	(*parser (char #\)))
	(*parser <whitespace_2>)
	(*caten 3)
	(*pack-with (lambda (w1 p w2) p))

	(*caten 4)
	(*pack-with (lambda (e p1 lst p2) (cons e lst)))

	(*parser (char #\())
	(*delayed (lambda () <InfixArgList>))
	(*parser (char #\)))
	(*caten 3)
	(*pack-with (lambda (p1 lst p2) lst))
	
	(*parser (char #\[))
	;(*parser <InfixAtomic>)
	(*parser <InfixExpression>)
	(*parser (char #\]))
	(*caten 3)
	(*pack-with (lambda (p1 lst p2) (list 'vector-ref lst)))

	(*disj 2)

	*star

	(*caten 2)
	(*pack-with (lambda (funcal argLists) 
		(fold-left 
			(lambda (a b) 
				(if (equal? (car b) 'vector-ref)
					(list 'vector-ref a (cadr b))
					(append (list a) b)))
					funcal argLists)))

	done))




(define <InfixArgList> ; return list of infix expressions
	(new
	(*parser <InfixExpression>)
	;(*pack (lambda (expr) (if (list? expr) expr (list expr))))
	(*pack list)

	(*parser (char #\,))
	(*parser <InfixExpression>)
	(*caten 2) 
	(*pack-with (lambda (c e) e))
	*star

	(*caten 2)
	(*pack-with append)

	(*parser <epsilon>)

	(*disj 2)

	done))




(define <PowerSymbol>
	(new
	(*parser (char #\^))
	(*parser (word "**"))
	(*disj 2)
	(*pack (lambda (p) 'expt))
	done))


(define <InfixPow>
	(new
	(*parser <InfixFuncall>)
	(*parser <InfixArrayGet>)
	(*disj 2)

	(*parser <PowerSymbol>)
	(*delayed (lambda () <InfixPow>))
	(*caten 3)
	(*pack-with (lambda (rand1 rator rand2) (list rator rand1 rand2)))

	(*parser <PowerSymbol>)
	(*parser <InfixFuncall>)
	(*parser <InfixArrayGet>)
	(*disj 2)
	
	(*caten 2) *star
	(*caten 2)
	(*pack-with (lambda (firstlst restlst)
				 (fold-left 
				 	(lambda (a b) (list (car b) a (cadr b)))
				 	firstlst
				 	restlst)))


	(*parser <InfixFuncall>)
	(*parser <InfixArrayGet>)
	(*disj 2)

	(*disj 2)
	done))

(define <InfixMulDiv>
	(new
	(*parser <InfixPow>)
	(*parser (char #\*))
	(*pack (lambda (op) '*))
	(*parser (char #\/))
	(*pack (lambda (op) '/))
	(*disj 2)
	(*parser <InfixPow>)
	(*caten 3)
	(*pack-with (lambda (rand1 rator rand2) (list rator rand1 rand2)))

	(*parser (char #\*))
	(*pack (lambda (op) '*))
	(*parser (char #\/))
	(*pack (lambda (op) '/))
	(*disj 2)
	(*parser <InfixPow>)
	(*caten 2) *star
	(*caten 2)
	(*pack-with (lambda (firstlst restlst)
				 (fold-left 
				 	(lambda (a b) (list (car b) a (cadr b)))
				 	firstlst
				 	restlst)))


	(*parser <InfixPow>)
	(*disj 2)
	done))

(define <InfixAddSub>
	(new
	(*parser <InfixMulDiv>)
	(*parser (char #\+))
	(*pack (lambda (op) '+))
	(*parser (char #\-))
	(*pack (lambda (op) '-))
	(*disj 2)
	(*parser <InfixMulDiv>)
	(*caten 3)
	(*pack-with (lambda (rand1 rator rand2) (list rator rand1 rand2)))

	(*parser (char #\+))
	(*pack (lambda (op) '+))
	(*parser (char #\-))
	(*pack (lambda (op) '-))
	(*disj 2)
	(*parser <InfixMulDiv>)
	(*caten 2)
	(*pack-with (lambda (rator rand3) (list rator rand3)))
	 *star

	(*caten 2)
	(*pack-with (lambda (firstlst restlst)
				 (fold-left 
				 	(lambda (a b) (list (car b) a (cadr b)))
				 	firstlst
				 	restlst)))
	(*parser <InfixMulDiv>)
	(*disj 2)
	done)) 

