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

(define (precedence sym)
  (cond
    ((eq? sym '*) 6)
    ((eq? sym '/) 6)
    ((eq? sym '+) 7)
    ((eq? sym '-) 7)
    (else (error "unknow symbol -- " sym))))

(define (create-node op lhs rhs)
  (list (precedence op) op lhs rhs))

(define (node-right-leaf node)
  (cadddr node))

(define (node-left-leaf node)
  (caddr node))

(define (node-op node)
  (cadr node))

(define (node-prec node)
  (car node))

(define (eval-node node)
  (eval (cdr node)))

(define (rhs expr)
  (caddr expr))

(define (op expr)
  (cadr expr))

(define (lhs expr)
  (car expr))

(define (add-node o l r tree)
  ((cond
     ((null? tree) (create-node o l r))
     ((> (precedence (node-op tree)) (precedence o))
           ; create on left
         )
     (else ; create on right))))



(define (build-arithmetic-tree expr tree)
  (cond 
    ((null? expr) '())






(define (base e)
  (lhs e))

(define (exponent e)
  (rhs e))

(define (more? expr)
  (cond 
    ((null? expr) false)
    (else (memq (car expr) '(- * + **)))))

(define (pop-expr e)
  (drop e 3))

(define (sum? x)
  (memq '+ x))

(define (addend s) 
  (lhs s))

(define (augend s) 
  (rhs s))

(define (difference? x)
  (memq '- x))

(define (minuend s)
  (lhs s))

(define (subtrahend s)
  (rhs s))

(define (product? x)
  (memq '* x))

(define (multiplier p) 
  (lhs p))

(define (multiplicand p) 
  (rhs p))


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


(define (apply-left-assoc type op expr)
  (if (more? (pop-expr expr))
    (type (cons 
            (op (type (lhs expr))
                (type (rhs expr)))
            (pop-expr expr)))
    (op 
      (type (lhs expr))
      (type (rhs expr)))))



(define (infix exp)
  (cond ((number? exp) exp)
        ;((exponentiation? exp) (apply-right-assoc infix make-exponentiation exp))
        ((product? exp) (apply-left-assoc infix make-product exp))
        ((sum? exp) (apply-left-assoc infix make-sum exp))
        ((difference? exp) (apply-left-assoc infix make-difference exp))
        (else
         (error "unknown expression type -- eval-exp" exp))))


(define eval-exp-tests
  (test-suite
    "infix"
    (test-case
      "simple arithmetic"
      (check-eq? (infix '(1 + 1)) 2)
      (check-eq? (infix '(2 * 3)) 6)
      (check-eq? (infix '((2 * 3) + (2 * 3))) 12)
      (check-eq? (infix '(2 * 3 + (2 * 3))) 12)
      (check-eq? (infix '(3 + 2 * 3)) 9)
      (check-eq? (infix '(2 * 3 + 2 * 3)) 12)
      (check-eq? (infix '(2 + 3 * 2 + 3)) 11)
      (check-eq? (infix '(2 + (3 * 2) + 3)) 11)
      (check-eq? (infix '((2 + 3) * (2 + 3))) 25)
      (check-eq? (infix '(10 - 5 - 1)) 4)
      (check-eq? (infix '(2 * 5 - 1)) 9))))

(run-test eval-exp-tests)
