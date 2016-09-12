#lang planet neil/sicp

(define (halve a) (/  a 2))
(define (double a) (* a 2))

(define (mult-recursive a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (mult-iterative a b)
  (define (iter p a b)
    (if (= b 0)
        p
        (iter (+ a p) a (- b 1))))
  (iter 0 a b))

;exercise 1.17
(define (mult-fast-recursive a b)
  (cond ((= 0 b) 0)
        ((= 0 (remainder b 2))
         (double (mult-fast-recursive a (halve b))))
        (else (+ a (mult-fast-recursive a (- b 1))))))

;exercise 1.18
(define (mult-fast-iterative a b)
  (define (iter p a b)
    (cond ((= 0 b) p)
          ((= 0 (remainder b 2))
           (iter p (double a) (halve b)))
          (else (iter (+ p a) a (- b 1)))))
  (iter 0 a b))

(mult-fast-iterative 23 10)