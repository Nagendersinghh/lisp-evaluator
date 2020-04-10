# lisp-evaluator
A lisp metacircular evaluator.

# Prerequisites
Download and install scheme from [here](https://www.gnu.org/software/mit-scheme/)

Once you have scheme installed, run the repl and type the following
- (load "eval.scm")
- (driver-loop)

The evaluator is now running. You can now enter lisp expressions to evaluate.
As of now, only the following functions from the scheme library are available:
- cons
- car
- cdr
- null?

## Example
```scheme
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(1 2 3) '(4 5 6))
>> (1 2 3 4 5 6)
```
