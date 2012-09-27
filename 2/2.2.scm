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

; 2.28
; fridge / flatten
(define (flatten s)
  (cond [(null? s) '()]
        [(list? s) (append 
                     (flatten (car s)) 
                     (flatten (cdr s)))]
        [else s]))
    
; 2.28 fridge/flatten
(define (flatten s)
  (cond [(null? s) '()]
        [(list? s) (cons (flatten (car s)) (flatten (cdr s)))]
        [else s]))

; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch len structure)
  (list len structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (display branch)
  (newline)
  (car (cdr branch)))

(define (branch-weight branch)
  (branch-structure branch))

(define (mobile? structure)
  (and (list? (left-branch structure)) 
       (list? (right-branch structure))))

(define (weight? structure)
  (integer? (right-branch structure)))

(define (structure? structure)
  (not (weight? (right-branch structure))))


(define (total-weight mobile)
  (display mobile)
  (newline)
  (cond 
    [(mobile? mobile) 
     (+ (total-weight (left-branch mobile)) 
        (total-weight (right-branch mobile)))]
    [(weight? mobile) 
     (branch-weight mobile)]
    [(structure? mobile) 
     (total-weight (branch-structure mobile))]
    [else 0]))

(define (torque node)
  (* (branch-length node) 
     (total-weight node)))

(define (balanced? mobile)
  (if (mobile? mobile)
    (and 
        (= (torque (left-branch mobile))
           (torque (right-branch mobile)))
        (balanced? (left-branch mobile))
        (balanced? (right-branch mobile)))
    #t))


(define b (make-branch 25 4))
(define c (make-branch 2 5))
(define d (make-branch 2 5))
(define e (make-mobile c d))
(define a (make-branch 10 e))
(define m (make-mobile a b))

(balanced? m) ; #t

(define nb (make-mobile (make-branch 2 3) (make-branch 4 5))) 
(balanced? nb) ; #f


;;;;
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 
            10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; 2.30
(define (square x) (* x x))
(define (square-tree tree)
  (cond 
    [(null? tree) '()]
    [(not (pair? tree)) (square tree)]
    [else (cons (square-tree (car tree))
                (square-tree (cdr tree)))]))

(define (square-tree tree)
  (map (lambda (leaf)
         (if (pair? leaf)
           (square-tree leaf)
           (square leaf))) tree))

; 2.31
(define (tree-map tree f)
  (map (lambda (leaf)
         (if (pair? leaf)
           (tree-map leaf f)
           (f leaf))) tree))

; 2.32
; The subsets of s are all of the subsets of s (say s') without the 
; first element x plus all of the sets s' with x. 
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (e)
                            (cons (car s) e))
                            rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


; 2.33
(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (mylength sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))


; 2.34

(define (showseq seq)
  (accumulate (lambda (x y) (display x) (display " ") (display y) (display "\n") y) 0 seq))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; 2.35

; original
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x (cons (list 1 2) (list 3 4)))

(define (acount-leaves t)
    (accumulate (lambda (leaf total) (+ leaf total)) 0 
                (map count-leaves t)))

; 2.36
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37
(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define mp '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define mx '((1 2 3) (4 5 6) (7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (car m) (car mp))

(define (matrix-*-vector m v)
  (map 
    (lambda (mp) (dot-product mp v))
      m))


; iter
(define (transpose mat)
  (define (iter matp mat)
    (if (null? (car mat))
      matp
      (iter 
        (append matp (list 
                     (accumulate (lambda (e col) (cons (car e) col)) '() mat)))
        (map (lambda (e) (cdr e)) mat))))
  (iter '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (map (lambda (mp)
                  (dot-product v mp)) cols))
           m)))


; 2.38
; fold-right and fold-left will give the same answers as long as the function being applied
; is distributive. For example, * and + are distributive, so they will produce the same results using either.
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; accum
(define (transpose mat)
  (accumulate-n (lambda (e col) 
                  (cons e col))
                '()
                mat))


(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))



(define (enumerate-interval low high)
  (if (> low high)
     null 
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))


; 2.33
(define (mymap p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (myappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (mylength sequence)
    (accumulate (lambda (x y) (+ y 1)) 0 sequence))


; 2.39
(define (reverse sequence)
  (fold-right 
    (lambda (x y) 
      (append y (list x))) 
    null sequence))

(define (reverse sequence)
  (fold-left 
    (lambda (x y) 
      (append (list y) x)) 
    null sequence))




;-----
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
;-------


(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (all-pairs n)
  (flatmap 
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))




























