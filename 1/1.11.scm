(define (f-r n)
  (cond
    ((< n 3) n)
    (else (+ (f-r (- n 1))
             (* 2 (f-r (- n 2)))
             (* 3 (f-r (- n 3)))))))

(define (iter a b c count n)
  (cond
    ((< n 3) n)
    ((> count n) a)
    (else (iter (+ a (* 2 b) (* 3 c)) 
                a 
                b 
                (+ 1 count) 
                n))))

(define (f-i n)
  (iter 2 1 0 3 n))
