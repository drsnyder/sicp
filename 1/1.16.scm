(define (square n) (* n n))

(define (my-expt b n) 
  (cond
    ((= n 0) 1)
    (else (expt-iterf b b n))))

(define (even? n)
    (= (remainder (truncate n) 2) 0))

(define (expt-iterf a b n)
  (cond
    ((= n 1) a)
    ((even? n) (expt-iterf (square a)
                           b
                           (/ n 2)))
    (else (* a (expt-iterf (square a)
                      b
                      (/ (- n 1) 2))))))

(define (my-expt2 b n)
    (define (expt-iterf2 acc b n)
      (cond
        ((= n 0) acc)
        ((even? n) (expt-iterf2 acc
                                (square b)
                                (/ n 2)))
        (else (expt-iterf2 (* acc b)
                           (square b)
                           (/ (- n 1) 2)))))
    (expt-iterf2 1 b n))


;;;;
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (slow* a b)
  (if (= b 0)
    0
    (+ a (slow* a (- b 1)))))
;;;

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (f* a b)
    (define (fast* a b)
      (cond
        ((= b 0) 0)
        ((even? b) (+ (double (fast* a (halve b)))))
        (else (+ a (fast* a (- b 1))))))
    (if (< a b)
      (fast* b a)
      (fast* a b)))

(define (fi* a b)
    (define (fast-iter* a b)
      (cond
        ((= b 1) a)
        ((even? b) (fast-iter* (double a)
                               (halve b)))
        (else (+ a (fast-iter* (double a)
                               (halve (- b 1)))))))
    (if (< a b)
      (fast-iter* b a)
      (fast-iter* a b)))

(define (fi2* a b)
  (define (iter acc a b) 
    (cond ((= b 0) acc) 
          ((even? b) (iter acc (double a) (halve b))) 
          (else (iter (+ acc a) a (- b 1))))) 
  (iter 0 a b))
