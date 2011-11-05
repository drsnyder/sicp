(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fact-i n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (inc a)
    (+ a 1))

(define (dec a)
    (- a 1))

(define (pr a b)
  (if (= a 0)
      b
      (inc (pr (dec a) b))))

(define (pi a b)
  (if (= a 0)
      b
      (pi (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 2 1) = 2 2
(A 2 2) = 4 2^2
(A 2 3) = 16 2^(2^2)
(A 2 4) = 65536 2^(2^(2^2))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1)) (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

10
5 5
11111 5
11111 11111

15
5 5 5
5 10
5 5 11111
111111111111111
10 11111
11111 11111 5

10
1s yes
5s yes
10s yes


; 1.11
; f(n) = n if n<3
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) 
(define (f3 n)
  (cond ((< n 3) n)
        (else (+ (f3 (- n 1)) 
                 (* 2 (f3 (- n 2)))
                 (* 3 (f3 (- n 3)))))))



(define (f a b c) (+ a (* 2 b) (* 3 c)))

(define (f3-iter a b c n)
  ; if n = 3 then compute f with the current values for a, b, c  
  (cond ((= n 3) (f a b c))
        ; compute new a, shift a, b back one
        (else (f3-iter (f a b c) a b (- n 1)))))

(define (f3i n)
  (cond ((< n 3) n)
        ; initial values for a, b, c when n >= 3
        (else (f3-iter 2 1 0 n))))




; another solution
(define (fi n)
  (define (fi-iter a b c count)
    ; f(n) = a + b + c = f(n-1) + 2f(n-2) + 3f(n-3).
    ; Use f(n) to calculate f(n+1) = f(n) + 2f(n-1) + 3f(n-2).
    (if (= count (+ n 1))
        a
        (fi-iter (+ a b c) (* 2 a) (* 3 (/ b 2)) (+ count 1))))
  (if (< n 3)
      n
      ; f(3) = f(2) + 2f(1) + 3f(0).
      (fi-iter 2 2 0 3)))


; 1.12 compute the elements of pascals triangle via recursive process
(define (pt-cols r)
    (+ r 1))

(define (pt r c)
    (cond ((or (< (pt-cols r) c) (> 0 c)) 0) ; out of bounds
          ((< r 0) 0) ; off the top
          ((= c 0) 1) ; base
          (else (+ (pt (- r 1) (- c 1)) (pt (- r 1) c)))))
        


(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(define (expti b n)
    (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product)))) 


(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


; a = 1 : 1
; a = 2 : 2*2


; 1.16
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (fast-expti b n)
    (fast-expt-iter b n 1))

; 1.17
; original linear
(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))
    

(define (even? n)
  (= (remainder n 2) 0))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

; log n
(define (multf a b)
  (cond ((= b 0) 0)
        ((even? b) (multf (double a) (halve b)))
        (else (+ a (multf a (- b 1))))))

; 1.18
(define (multfi a b)
  (multf-iter a b 0))

(define (multf-iter a b p)
  (cond ((= b 0) p)
        ((even? b) (multf-iter (double a) (halve b) p))
        (else (multf-iter a (- b 1) (+ a p)))))


; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; prime
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


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))        

(define (un-random n)
  (cond ((or (= n 1) (= n 0)) n)
        ((= (remainder n 2) 0) (/ n 2))
        (else (un-random (- n 1)))))
  
(define (fermat-test n)
    (define (try-it a)
          (= (expmod a n n) a))
      (try-it (+ 1 (un-random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))

(define (runtime) (current-inexact-milliseconds))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (and (report-prime (- (runtime) start-time)) #t)
    (and (display " *** no prime ***") #f)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (find-prime n)
  (if (even? n)
    (find-prime (+ 1 n))
    (if (prime? n) 
        n
        (find-prime (+ n 1)))))

(define (find-prime-ft n)
  (if (even? n)
    (find-prime (+ 1 n))
    (if (fermat-test n) 
        n
        (find-prime (+ n 1)))))


; 1.22
(define (find-primes-greater-than n c)
  (if (= c 0)
    (display " done \n")
    (if (= (remainder n 2) 0) 
      (find-primes-greater-than (+ n 1) c)
      (if (timed-prime-test n)
        (find-primes-greater-than (+ n 2) (- c 1))
        (find-primes-greater-than (+ n 2) c)))))

; 1.23
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (next-divisor n)
  (if (= n 2) 
    3
    (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (find-divisor-s n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-s n (+ test-divisor 1)))))

; 1.25
; looks equivalent to me
(define (expmod b n m)
  (cond ((= n 0) 1)
        ((even? n)
         (remainder (square (expmod b (/ n 2) m)) m))
        (else
          (remainder (* b (expmod b (- n 1) m)) m))))        

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod-f base exp m)
    (remainder (fast-expt base exp) m))

; 1.27
(define (carmichael? n)
    (define (try-it a)
      (= (expmod a n n) a))
  (define (carmichael-iter n a)
    (cond ((= a 1) #t)
          ((try-it a) (carmichael-iter n (- a 1)))
          (else #f)))
  (and (not (prime? n)) (carmichael-iter n (- n 1)))) 

; 1.28
; revisit


; 1.29
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
    (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)

