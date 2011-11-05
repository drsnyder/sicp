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

; 1.33 filtered accumulate
(define (filtered-accumulate combiner null-value term a next b fil)
  (if (> a b)
    null-value
    (if (fil a)
        (combiner (term a) 
                  (filtered-accumulate combiner null-value term (next a) next b fil))
        (filtered-accumulate combiner null-value term (next a) next b fil))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (pass x) #t)

; product 1..5
(filtered-accumulate * 1 identity 1 inc 5 pass) 

; sum square prime? 1..5
(filtered-accumulate + 0 square 1 inc 5 prime?)
(filtered-accumulate + 0 square 2 inc 3 prime?)


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (product-relatively-prime a n)
    (define (relatively-prime? i)
      (= (gcd i n) 1))
    (filtered-accumulate * 1 identity 1 inc n relatively-prime?))

