
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; 2.54
(define (myequal? a b)
  (cond
    [(and (pair? a) (pair? b)) 
     (and (eq? (car a) (car b)) 
          (myequal? (cdr a) (cdr b)))]
    [else (eq? a b)]))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) 
  (cond ((< 3 (length s)) 
         (cons '+ (cddr s)))
        (else (caddr s))))

(define (product? x)
    (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) 
  (cadr p))

(define (multiplicand p) 
  (cond ((< 3 (length p)) 
         (cons '* (cddr p)))
        (else (caddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


; 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
             (make-product (exponent exp)
                           (make-exponentiation (base exp)
                                                (make-sum (exponent exp) -1)))
             (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


; 2.57
; see augend and multiplicand above
(deriv '(* (* x y) (+ x 3)) 'x) => (deriv '(* x y (+ x 3)) 'x)

; 2.58

;; sets

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; 2.59
; recursively, until set1 is empty, union the cdr of set1 to the set resulting from adjoining the car of set1 to
; set2
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (union-set (cdr set1) 
                         (adjoin-set (car set1) set2)))))

;; sets as orderd lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()    
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

; 2.61
; adjoin-set
; if s1 is empty, create a new set with x
; if x = s1 return s1
; if x < s1 cons x onto s1
; if x > s1 cons car s onto adjoin-set x with the cdr of s
(define (adjoin-set x s)
  (cond ((null? s) (cons x '()))
        ((= x (car s)) s)
        ((< x (car s)) 
         (cons x s))
        ((> x (car s)) 
         (cons (car s) 
               (adjoin-set x (cdr s))))))

; 2.62
; union-set
; correct? tests?
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((< x1 x2) 
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) 
                       (cons x2 (union-set set1 (cdr set2))))
                      (else (cons x1 (union-set (cdr set1) (cdr set2)))))))))


; sets as tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry)
  (make-tree entry '() '()))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2.63

(define (rand-seq n)
  (if (= n 0) 
    '()
    (cons (random 100)
          (rand-seq (- n 1)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define s (rand-seq 10))
(define t (accumulate (lambda (x set) (adjoin-set x set)) '() s))  

; the difference between these two depends on append
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (copy-to-list tree result-list)
  (if (null? tree)
    result-list
    (copy-to-list (left-branch tree)
                  (cons (entry tree)
                        (copy-to-list (right-branch tree)
                                      result-list)))))

(define (tree->list-2 tree)
  (copy-to-list tree '()))

(define t1
  (make-tree 7
             (make-tree 3
                        (make-leaf 1)
                        (make-leaf 5))
             (make-tree 9
                        '()
                        (make-leaf 11))))

(define t2
  (make-tree 3
             (make-leaf 1)
             (make-tree 7
                        (make-leaf 5)
                        (make-tree 9
                                   '()
                                   (make-leaf 11)))))

(define t3
  (make-tree 5
             (make-tree 3
                        (make-leaf 1)
                        '())
             (make-tree 9
                        (make-leaf 7)
                        (make-leaf 11))))


