(define factorial
  (lambda (n)
    (cond 
      ((= n 0) 1)
      ((= n 1) 1)
      ((< n 0) (error "n < 0"))
      (else (* n (factorial (- n 1)))))))

(define nchoosek
  (lambda (n k)
    (cond 
      ((= n 0) 1)
      ((= n 1) 1)
      ((= k 0) 1)
      ((= n k) 1)
      ((> k n) 0)
      (else 
        (and 
          (fprintf (current-output-port) "~a ~a ~a.~n" n k (- n k))
          (/ (factorial n) 
             (* (factorial k) 
                (factorial (- n k)))))))))

(define pascalt 
  (lambda (row index)
    (cond
        ((= row 0) 1)
        ((= index 0) 1)
        ((= row index) 1)
        (else (+ (nchoosek (- row 1) (- index 1))
                           (nchoosek (- row 1) index))))))
