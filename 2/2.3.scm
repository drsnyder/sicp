
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; 2.54
(define (myequal? a b)
  (cond
    [(and (pair? a) (pair? b)) 
     (and (eq? (car a) (car b)) 
          (myequal? (cdr a) (cdr b)))]
    [else (eq? a b)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (cond ((< 3 (length s)) 
         (cons '+ (cddr s)))
        (else (caddr s))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) 
  (cadr p))

(define (multiplicand p) 
  (cond ((< 3 (length p)) 
         (cons '* (cddr p)))
        (else (caddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


; 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
             (make-product (exponent exp)
                           (make-exponentiation (base exp)
                                                (make-sum (exponent exp) -1)))
             (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


; 2.57
; see augend and multiplicand above
(deriv '(* (* x y) (+ x 3)) 'x) => (deriv '(* x y (+ x 3)) 'x)

; 2.58

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (take-while f seq)
  (cond ((null? seq) seq)
        ((f (car seq)) 
         (cons (car seq) (take-while f (cdr seq))))
        (else null)))

(define (expression? x)
  (or (product? x)
      (sum? x)
      (exponentiation? x)))

(define (sum? x)
  (memq '+ x))

(sum? '(1 + 2))

(define (addend s) 
  (let ((e (take-while (lambda (x) (not (eq? '+ x))) s)))
    (if (expression? e)
      e
      (car e))))

(define (augend s) 
  (let ((e (cdr (memq '+ s))))
    (if (expression? e)
      e
      (car e))))

(define (product? x)
  (memq '* x))

(product? '(1 * 2))


(define (multiplier p) 
  (let ((e (take-while (lambda (x) (not (eq? '* x))) p)))
    (if (expression? e)
      e
      (car e))))


(define (multiplicand p) 
  (let ((e (cdr (memq '* p))))
    (if (expression? e)
      e
      (car e))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))


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


(define t1 '((x * y) * (x + 3)))
(define t2 '(x + 3 * (x + y + 2)))
(define t3 '((2 + 3) * (3 * 3)))
(define t4 '(2 + 3 * 3 * 3))
(deriv t1 'x)
(deriv t2 'x)


(define (eval-exp exp)
  (cond ((number? exp) exp)
        ((variable? exp) exp)
        ((sum? exp)
         (make-sum (eval-exp (addend exp))
                   (eval-exp (augend exp))))
        ((product? exp)
         (make-product (eval-exp (multiplier exp))
                       (eval-exp (multiplicand exp))))
        ((exponentiation? exp)
         (make-exponentiation (eval-exp (base exp))
                              (eval-exp (exponent exp))))
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
      )))























































