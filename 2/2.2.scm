; 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (add-points p1 p2)
  (make-point (+ (x-point p1)
                 (x-point p2))
              (+ (y-point p1)
                 (y-point p2))))

(define (print-point p)
  (newline)
  (display "(")
           (display (x-point p))
           (display ",")
           (display (y-point p))
           (display ")"))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (add-points (start-segment s)
             (make-point 
               (/ (- (x-point (end-segment s))
                     (x-point (start-segment s)))
                  2)
               (/ (- (y-point (end-segment s))
                     (y-point (start-segment s)))
                  2))))

; (midpoint-segment 
;   (make-segment 
;     (make-point -1 -1)
;     (make-point 3 3)))

; 2.3
;
; abstraction barriers 
;   make-rectangle
;   first-corner
;   second-corner
;   third-corner
;   fourth-corner

; (define r (make-rectangle 
;   (make-point 0 0) 
;   (make-point 0 1) 
;   (make-point 1 1) 
;   (make-point 1 0))) 
;
; A different implementation would be to change the list that contains the 4
; points (list ...). You could also wrap this in a closure where you give the closure a
; number and it gives you that point. 
(define (make-rectangle a b c d)
  (cons 
    (cons a b)
    (cons c d)))

(define (first-corner r)
  (car (car r)))

(define (second-corner r)
  (cdr (car r)))

(define (third-corner r)
  (car (cdr r)))

(define (fourth-corner r)
  (cdr (cdr r)))

(define (square x)
  (* x x))

(define (edge-distance e1 e2)
  (sqrt 
    (+ (square 
         (- (x-point e1)
            (x-point e2)))
       (square 
         (- (y-point e1)
            (y-point e2))))))

(define (rectangle-perimeter r)
  (+
    (edge-distance (first-corner r) (second-corner r))
    (edge-distance (second-corner r) (third-corner r))
    (edge-distance (third-corner r) (fourth-corner r))
    (edge-distance (fourth-corner r) (first-corner r))))

(define (rectangle-area r)
  (*
    (edge-distance (first-corner r) (second-corner r))
    (edge-distance (first-corner r) (fourth-corner r))))

; (edge-distance 
;   (make-point 0 0) 
;   (make-point 1 1)) => 1.4
; 
; (edge-distance 
;   (make-point 3 2) 
;   (make-point 9 7)) => 7.8
;
; (edge-distance
;   (make-point 0 0)
;   (make-point 0 1)) => 1

; 2.4

(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))

(define (mycdr z)
  (z (lambda (p q) q)))

; 2.5

(define (carval a)
  (expt 2 a))

(define (cdrval b)
  (expt 3 b))

(define (reduce-i test term n c)
  (if (test (term n))
    (reduce-i test term (term n) (+ c 1))
    c))

(define (icar c)
  (if (even? c)
    (reduce-i (lambda (i) (and (integer? i) (even? i)))
              (lambda (y) (/ y 2)) 
              c 
              1)
    0))

(define (odd-test i)
  (and (integer? i) (odd? i)))

(define (d3 i)
  (/ i 3))

(define (icdr c)
  (if (odd? c) 
    (reduce-i odd-test
              d3
              c 
              0)
    (reduce-i odd-test
              d3
              (/ c (expt 2 (icar c))) 
              0)))

(define (icons a b)
  (* (carval a) (cdrval b)))

(define x (icons 1 1))
(define y (icons 3 2))
(define z (icons 16 27))
(icar x)
(icdr x)

; 2.17
(define (last-pair s)
  (if (null? (cdr s))
    s
    (last-pair (cdr s))))

; 2.18
(define (reverse s)
  (define (reverse-i is os)
    (if (null? is)
      os
      (reverse-i (cdr is) (cons (car is) os))))
  (reverse-i s '()))

; 2.20
(define (same-parity s . rest)
  (define (take-with-parity p l)
    (if (null? l)
      '()
      (if (= (remainder (car l) 2) p)
        (cons (car l) (take-with-parity p (cdr l)))
        (take-with-parity p (cdr l)))))
  (let ((p (remainder s 2)))
    (cons s (take-with-parity p rest))))

; 2.21
(define (square-list items)
  (if (null? items)
    '()
    (cons (* (car items) (car items)) 
          (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

; 2.22
(define (square x) (* x x))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things) 
            (cons (square (car things))
                  answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

; 2.23
(define (for-each f items)
  (and 
    (not (null? items))
    (or (f (car items)) #t)
    (for-each f (cdr items))))

; 2.27
(define (deep-reverse s)
    (define (reverse-i s a)
      (cond [(null? s) a]
            [(list? s) 
             (reverse-i (cdr s) 
                        (cons (reverse-i (car s) '()) a))]
            [else s]))
    (reverse-i s '()))
    
