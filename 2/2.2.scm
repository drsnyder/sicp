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
