(define (eval expr env)
  (cond ((self-evaluating? expr) expr)
	((variable? expr)
	 (lookup-variable expr env))
	((quoted? expr)
	 (text-of-quotation expr))
	((assignment? expr)
	 (eval-assignment expr env))
	((definition? expr)
	 (eval-definition expr))
	((if? expr) (eval-if expr env))
	((lambda? expr)
	 (make-procedure
	   (lambda-parameters expr)
	   (lambda-body expr)
	   env))
	((begin? expr)
	 (eval-sequence
	   (begin-actions expr)
	   env))
	((cond? expr)
	 (eval (cond-if expr) env))
	((application? expr)
	 (apply (eval (operator expr) env)
		(list-of-values
		  (operands expr)
		  env)))
	(else
	  (error "Unknown expression type: EVAL" expr))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values
	      (rest-operands exps)
	      env))))

(define (eval-if expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	  (eval (first-exp exps) env)
	  (eval-sequence (rest-exps exps) env))))

(define (eval-assignment expr env)
  (set-variable-value!
    (assignment-variable expr)
    (eval (assignment-value expr) env)
    env)
  'ok)

(define (eval-definition expr env)
  (define-variable!
    (definition-variable expr)
    (eval (definition-value expr) env)
    env)
  'ok)

; The only self-evaluating items are numbers and strings
(define (self-evaluating? expr)
  (cond ((number? expr) true)
	((string? expr) true)
	(else false)))

; Variables are represented as symbols
(define (variable? expr) (symbol? expr))

; Quotation have the form (quote <text-of-quotation>)
(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr)
  (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Assignments have the form (set! <var> <value>)
(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-value expr)
  (caddr expr))

; Definitions have the form (define <var> <value>) or
; (define (<var> <param 1> ... <param n>)
;	<body>)
(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caddr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda
	(cdadr expr)
	(cddr expr))))

; Lambda expressions are lists that begin with the symbol lambda
(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Conditionals begin with `if` and have a predicate, a
; consequent, and an optional alternative. If the
; expression has no alternative, we provide `false`
; as the alternative.
(define (if? expr) (tagged-list? expr 'if))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr)
  (if (not (null? (cadddr expr)))
      (cadddr expr)
      'false))

(define (make-if predicate
		 consequent
		 alternative)
  (list 'if predicate consequent alternative))

(define (begin? expr)
  (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Cond expression
(define (cond? expr)
  (tagged-list? expr 'cond))
(define (cond-clauses expr) (cdr expr))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp
		  (cond-actions first))
		(error "ELSE clause isn't last: COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp
		       (cond-actions first))
		     (expand-clauses rest))))))

