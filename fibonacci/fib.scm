#lang racket
;(fib 0) = 0
;(fib 1) = 1

(define fib-seed-0 0)
(define fib-seed-1 1)

(define (fib-tree a)
  (cond ((= a 0) fib-seed-0)
        ((= a 1) fib-seed-1)
        (else (+ (fib-tree (- a 1))
                 (fib-tree (- a 2))))))

(define (fib-iter n)
  (define (iter a b i)
    (cond ((= i n) b)
          (else (iter b (+ a b) (+ i 1)))))
  (if (= n 0)
      fib-seed-0
      (iter fib-seed-0 fib-seed-1 1)))

(define (fib-sicp n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter fib-seed-1 fib-seed-0 0 1 n))

; This algorithm is based on the following:
;    f(a)=f(b)f(c)+f(b-1)f(c-1)
;  where:
;    a=b+c
;    f(0)=1
;    f(1)=1
;    f(n)=f(n-1)+f(n-2)
; In each vector, the last value is the greatest
; The interior calulations are done with f(0)=1 f(1)=1
;and the result is adjusted to the actual seed values
;using the above equation
(define (fib-mine-rec n)
  (define (fib-iter-internal n)
    (define (iter a b i)
      (cond ((= i n) b)
            (else (iter b (+ a b) (+ i 1)))))
    (if (= n 0) 1
        (iter 1 1 1)))
  (define (sum-of-products a b c d)
    (+ (* a b) (* c d)))
  (define (request-3 n)
    ; find f(n), f(n-1), and f(n-2)
    (let ((l (request-2 (- n 1))))
      (let ((a (vector-ref l 0))
            (b (vector-ref l 1)))
        (vector a
                b
                (+ a b)))))           
  (define (request-2 n)
    ; find f(n) and f(n-1)
    (if (< n 10) ; should be tweaked for best performance
        (vector (fib-iter-internal (- n 1))
                (fib-iter-internal n))
        (let ((l (request-3 (quotient (+ n 1) 2))))
          (let((a (vector-ref l 0))
               (b (vector-ref l 1))
               (c (vector-ref l 2)))
            (if (even? n)
                (vector (sum-of-products c b b a)
                        (sum-of-products c c b b))
                (vector (sum-of-products b b a a)
                        (sum-of-products c b b a)))))))
  ;this is used to adjust the result to the actual seed values
  (let ((v (request-2 (- n 1))))
    (let ((a (vector-ref v 0))
          (b (vector-ref v 1)))
      (sum-of-products a fib-seed-0 b fib-seed-1))))





(define (test n)
  (values
   ; (fib-tree n)
   ;(fib-iter n)
   (fib-sicp n)
   (fib-mine-rec n)
   ))
(fib-mine-rec 100000)