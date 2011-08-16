(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 1 (p))

(test 0 (p))

