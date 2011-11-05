; Simpson's Rule http://en.wikipedia.org/wiki/Simpson's_rule
(define (simpson-integral f a b n)

  (define (h a b n) 
    (/ (- b a) n))

  (define (integrate h)

      (define (fx k)

        (define (y a k h)
          (+ a (* k h)))

        (cond ((even? k) (* 2 (f (y a k h))))
              (else (* 4 (f (y a k h))))))

      (/ (* h (sum fx 0 inc n)) 3)) 

  (integrate (h a b n)))

; 1.30 iterative sum
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum identity 0 inc 10)

; 1.31 product
; TODO: compute pi using these
(define (product-r term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-r term (next a) next b))))

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))
    

; 1.32 accumulate
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) 
              (accumulate combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))
