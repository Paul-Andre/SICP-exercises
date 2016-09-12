#lang planet neil/sicp
; coin-types should be a list of all the possible coin types
; ordered from biggest to smallest
(define (count-change amount coin-types) 
	(cond ((= amount 0) 
			1)
		(( or  (null? coin-types)
			(< amount 0)) 0)
		(else
			(+ (count-change (- amount (car coin-types))
					coin-types) 
				(count-change amount
					(cdr coin-types))))))


(define (count-change-special amount coin-types)

	(define (make-starting-vector) (make-vector amount 0))
	
	(define (count vec coin)
	
		(define (sub-count n)
			(+ (cond ((= n coin) 1)
					((< n coin) 0)
					(else (vector-ref vec (- n coin 1))))
			(vector-ref vec (- n 1))))
			
		(define (iter n)
			(cond ((>= amount n) 
					(vector-set! vec (- n 1) (sub-count n))
					(iter (+ n 1)))))
					
		(iter coin)
		vec)
				
	; If coin-types was a vector or a list that went in the other direction, this part would be simpler.
	(define (down coin-types)
		( if (null? (cdr coin-types))
			(count (make-starting-vector) (car coin-types))
			(count (down (cdr coin-types)) (car coin-types))))
			
	(vector-ref (down coin-types)  (- amount 1)))
	

(define (ccs v)
	(count-change-special v '(50 25 10 5 1)))
(define (ccn v)
	(count-change v '(50 25 10 5 1)))


(newline)
(display (ccs 300))
(newline)
(display (ccn 300))
		