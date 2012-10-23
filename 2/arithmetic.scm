; a naive solution that is broken 
; see http://www.engr.mun.ca/~theo/Misc/exp_parsing.htm for classical solutions
; and possible alternatives
;
; that change required is that you need to account for associativity and
; precedence and then build and consume expressions according to the
; associativity
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (take-while f seq)
  (cond ((null? seq) seq)
        ((f (car seq)) 
         (cons (car seq) (take-while f (cdr seq))))
        (else null)))


(define (arithmetic-lhs expr sym)
  (let ((e (take-while (lambda (x) (not (eq? sym x))) expr)))
    (if (expression? e)
      e
      (car e))))

(define (arithmetic-rhs expr sym)
  (let ((e (cdr (memq sym expr))))
    (if (expression? e)
      e
      (car e))))

(define (sum? x)
  (memq '+ x))

(define (addend s) 
  (arithmetic-lhs s '+))

(define (augend s) 
  (arithmetic-rhs s '+))

(define (difference? x)
  (memq '- x))

(define (minuend s)
  (arithmetic-lhs s '-))

(define (subtrahend s)
  (arithmetic-rhs s '-))

(define (product? x)
  (memq '* x))

(define (multiplier p) 
  (arithmetic-lhs p '*))

(define (multiplicand p) 
  (arithmetic-rhs p '*))


(define (expression? x)
  (or (product? x)
      (sum? x)
      (difference? x)
      (exponentiation? x)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-difference a1 a2)
  (cond ((=number? a1 0) (- a2))
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list a1 '- a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e)) 
        (else (list '** b e))))

(define (eval-exp exp)
  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((exponentiation? exp)
         (make-exponentiation (eval-exp (base exp))
                              (eval-exp (exponent exp))))
        ((product? exp)
         (make-product (eval-exp (multiplier exp))
                       (eval-exp (multiplicand exp))))
        ((difference? exp)
         (make-difference (eval-exp (minuend exp))
                          (eval-exp (subtrahend exp))))
        ((sum? exp)
         (make-sum (eval-exp (addend exp))
                   (eval-exp (augend exp))))
        (else
         (error "unknown expression type -- eval-exp" exp))))


(define eval-exp-tests
  (test-suite
    "eval-exp"
    (test-case
      "simple arithmetic"
      (check-eq? (eval-exp '(1 + 1)) 2)
      (check-eq? (eval-exp '(2 * 3)) 6)
      (check-eq? (eval-exp '((2 * 3) + (2 * 3))) 12)
      (check-eq? (eval-exp '(2 * 3 + (2 * 3))) 12)
      (check-eq? (eval-exp '(3 + 2 * 3)) 9)
      (check-eq? (eval-exp '(2 * 3 + 2 * 3)) 12)
      (check-eq? (eval-exp '(2 + 3 * 2 + 3)) 11)
      (check-eq? (eval-exp '(2 + (3 * 2) + 3)) 11)
      (check-eq? (eval-exp '((2 + 3) * (2 + 3))) 25)
      (check-eq? (eval-exp '(10 - 5 - 1)) 4)
      (check-eq? (eval-exp '(2 * 5 - 1)) 9))))

(run-test eval-exp-tests)
