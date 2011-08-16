(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-iter? guess x change)
  (< (abs (- change (abs (- (square guess) x)))) 0.0000001))

(define (good-enough-iter1? guess x change)
  (< (/ change guess) 0.0000001))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 3)
(sqrt 9)
(sqrt 0.001)
(sqrt 4294967296)
(sqrt (+ 1099511627776 5))
(sqrt 0.002)
