(define (f-rec n)
	(define (intermed n m) (* m (f-rec (- n m))))
	(if (< n 3)
		n
		(+ (intermed n 1)
			(intermed n 2)
		 	(intermed n 3))))

(define (f-iter m)
	(define (op a b c) (+ 
		a
		(* 2 b)
		(* 3 c)))
	(define (iter n f1 f2 f3)
		(if (= n m)
			(op f1 f2 f3)
			(iter (+ n 1) 
				(op f1 f2 f3)
				f1 
				f2)))
	(iter 3 2 1 0))
				