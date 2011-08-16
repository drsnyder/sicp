
(define (sum-squares a b) (+ (square a) (square b)))

(define (sum-squares-max a b c)
  (if (> a b)
    ; true a > b
    (if (> b c)
      (sum-squares a b)
      (sum-squares a c))
    ; false b > a
    (if (> a c)
      (sum-squares b a)
      (sum-squares b c))
    )
  )


(sum-squares-max 4 3 5)
(sum-squares-max 3 4 5)
(sum-squares-max 5 4 3)
(sum-squares-max 5 3 4)

