
(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (abs n)
  (if (> n 0)
    n
    (* -1 n)))

(define (negative? a b)
  (and 
    (or (< a 0) (< b 0))
    (not (and (< a 0) (< b 0)))))

(define (sign a b)
  (if (negative? a b)
    -1
    1))

(define (make-rat n d)
  (let ((g (gcd n d)))
      (cons (* (sign n d) (abs (/ n g))) 
            (abs (/ d g)))))
; (make-rat 1 2) => '(1 . 2)
; (make-rat 1 -2) => '(-1 . 2)
; (make-rat -1 2) => '(-1 . 2)
; (make-rat -1 -2) => '(1 . 2)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
