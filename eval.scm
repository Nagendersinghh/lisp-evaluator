(define (eval expr env)
  (cond ((self-evaluating? expr)
	 expr)
	((variable? expr)
	 (lookup-variable-value expr env))
	((quoted? expr)
	 (text-of-quotation expr))
	((assignment? expr)
	 (eval-assignment expr env))
	((definition? expr)
	 (eval-definition expr env))
	((if? expr)
	 (eval-if expr env))
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
	 (eval (cond->if expr) env))
	((application? expr)
	 (apply (eval (operator expr) env)
		(list-of-values
		  (operands expr)
		  env)))
	(else
	  (error "Unknown expression type: EVAL" expr))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure
				    arguments))
	((compound-procedure? procedure)
	 (eval-sequence (procedure-body procedure)
			(extend-environment (procedure-parameters procedure)
					    arguments
					    (procedure-environment procedure))))
	(else
	  (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps)
			    env))))

; A procedure that guarantees operand evaluation in left
; to right sequence.
(define (l2r-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (eval (first-operand exps) env)))
	(let ((first (l2r-list-of-values (rest-operands exps) env)))
	  (cons first rest)))))

#|
 |A procedure that guarantees operand evaluation in right
 |to left sequence.
 |#
(define (r2l-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (r2l-list-of-values (rest-operands exps)
				       env)))
	(let ((last (eval (first-operand exps) env)))
	  (cons first last)))))

(define (eval-if expr env)
  (if (true? (eval (if-predicate expr) env))
      (eval (if-consequent expr) env)
      (eval (if-alternative expr) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	  (eval (first-exp exps) env)
	  (eval-sequence (rest-exps exps)
			 env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
		       (eval (assignment-value expr)
			     env)
		       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
		    (eval (definition-value expr) env)
		    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
	(cdadr exp)
	(cddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate
		 consequent
		 alternative)
  (list 'if
	predicate
	consequent
	alternative))

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (begin-actions seq) (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exp seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause not the last clause" exp))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))

(define (enclosing-environment env)
  (cdr env))
(define (first-frame env)
  (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound Variable:" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable: var" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars)
			(cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
	(list 'cons cons)
	(list 'null? null?)))

(define (primitive-procedure-objects)
  (map (lambda (proc)
	 (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (setup-environment)
  (let ((initial-env
	  (extend-environment (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment
  (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
	    (eval input
		  the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display
	(list 'compound-procedure
	      (procedure-parameters object)
	      (procedure-body object)
	      '<procedure-env>))
      (display object)))
