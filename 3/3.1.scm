; 3.1
(define (make-accumulator start)
	(lambda (increment)
		(set! start (+ start increment))
		start))

(define A (make-accumulator 10))
(A 5)

; 3.2
(define (make-monitored f)
	(let ((times-called 0))
		(lambda (input)
			(cond ((eq? input 'how-many-calls?) times-called)
						((eq? input 'reset-count) (set! times-called 0))
						(else (set! times-called (+ times-called 1))
									(f input))))))

(define (sqrt n)
	(* n n))

(define s (make-monitored sqrt))
(s 2)
(s 4)
(s 'how-many-calls?)

;3.3 & 3.4
(define (make-account balance password)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount))
						 balance)
			"Insufficient funds"))
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance)
	(let ((failed-attempts 0))
		(define (dispatch given-password m)
			(if (eq? password given-password)
				(cond ((eq? m 'withdraw) withdraw)
							((eq? m 'deposit) deposit)
							(else (error "Unknown request -- MAKE-ACCOUNT"
													 m)))
				(begin (set! failed-attempts (+ failed-attempts 1))
							 (if (> failed-attempts 7)
								 (call-the-cops)
								 (error "Incorrect password")) )))
		dispatch))

(define acc  (make-account 100 'secret))
((acc 'secret 'deposit) 10)
((acc 'secret 'withdraw) 10)

((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)
((acc 'bad 'deposit) 10)

