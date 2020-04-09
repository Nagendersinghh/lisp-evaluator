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
	 (make-procedure (lambda-parameters expr)
			 (lambda-body expr)
			 env))
	((begin? expr)
	 (eval-sequence
	   (begin-actions expr)
	   env))
	((cond? expr)
	 (eval (cond->if expr) env))
	((application? expr)
	 (_apply (eval (operator expr) env)
		 (list-of-values (operands expr)
				 env)))
	(else (error "Unknown expression type: EVAL"
		     expr))))

(define (_apply procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
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
	    (list-of-values (rest-operands exps) env))))

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
  (set-variable-value! (assignment-variable expr)
		       (eval (assignment-value expr) env)
		       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
		    (eval (definition-value expr) env)
		    env)
  'ok)

(define (self-evaluating? expr)
  (or (number? expr)
      (string? expr)))

(define (variable? expr)
  (symbol? expr))

(define (quoted? expr)
  (tagged-list? expr 'quote))
(define (text-of-quotation expr)
  (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      false))

(define (assignment? expr)
  (tagged-list? expr 'set!))
(define (assignment-variable expr)
  (cadr expr))
(define (assignment-value expr)
  (caddr expr))

(define (definition? expr)
  (tagged-list? expr 'define))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))
(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr)
		   (cddr expr))))

(define (lambda? expr)
  (tagged-list? expr 'lambda))
(define (lambda-parameters expr)
  (cadr expr))
(define (lambda-body expr)
  (cddr expr))
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (if? expr)
  (tagged-list? expr 'if))
(define (if-predicate expr)
  (cadr expr))
(define (if-consequent expr)
  (caddr expr))
(define (if-alternative expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))
(define (make-if predicate
		 consequent
		 alternative)
  (list 'if predicate consequent alternative))

(define (begin? expr)
  (tagged-list? expr 'begin))
(define (begin-actions expr)
  (cdr expr))

(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? expr)
  (tagged-list? expr 'cond))
(define (cond-clauses expr)
  (cdr expr))
(define (cond-else-clause? clause)
  (eq? 'else (cond-predicate clause)))
(define (cond-actions clause)
  (cdr clause))
(define (cond-predicate clause)
  (car clause))
(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))
(define (expand-clauses clauses)
  (if (null? clauses)
      false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "Else clause is not the last clause in cond expression"
		       first))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (primitive-procedure? expr)
  (tagged-list? expr 'primitive))

(define (compound-procedure? expr)
  (tagged-list? expr 'procedure))
(define (procedure-body expr)
  (caddr expr))
(define (procedure-parameters expr)
  (cadr expr))
(define (procedure-environment expr)
  (cadddr expr))

(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals)
	base-env))
(define (make-frame vars vals)
  (cons vars vals))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? (car vars) var)
	     (car vals))
	    (else
	      (scan (cdr vars) (cdr vals)))))
    (if (eq? the-empty-environment env)
	(error "Unbound variable: " var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
	    ((eq? (car vars) var)
	     (set-car! vals val))
	    (else
	      (scan (cdr vars) (cdr vals)))))
    (if (eq? the-empty-environment env)
	(error "Unbound variable: " var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? (car vars) var)
	     (set-car! vals val))
	    (else
	      (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define (add-binding-to-frame! var val frame)
  (let ((vars (frame-variables frame))
	(vals (frame-values frame)))
    (set-car! frame (cons var vars))
    (set-cdr! frame (cons val vals))))

(define (first-frame env)
  (car env))
(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))

(define (enclosing-environment env)
  (cdr env))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)))

(define primitive-procedure-names
  (map car primitive-procedures))
(define primitive-procedure-values
  (map (lambda (proc)
	 (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-implementation proc)
  (cadr proc))

(define (apply-primitive-procedure procedure arguments)
  (apply (primitive-implementation procedure) arguments))

(define (setup-environment)
  (let ((initial-environment (extend-environment primitive-procedure-names
						 primitive-procedure-values
						 the-empty-environment)))
    (define-variable! 'true true initial-environment)
    (define-variable! 'false false initial-environment)
    initial-environment))

(define the-global-environment (setup-environment))

(define input-prompt ";;;Input->")
(define output-prompt ";;;Output->")

(define (prompt-for-input string)
  (newline)
  (display string))

(define (announce-output string)
  (newline)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display
	(list 'compound-procedure
	      (procedure-parameters object)
	      (procedure-body object)
	      '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
