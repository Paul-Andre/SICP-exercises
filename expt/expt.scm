#lang planet neil/sicp

(define (expt-rec b n)
  (if (= n 0) 1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n)
  (define (iter a n)
    (if (= 0 n)
        a
        (iter (* a b) (- n 1))))
  (iter b (- n 1)))

(define (expt-fast-rec b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        (else 
         (if (= 0 (remainder n 2))
             (square (expt-fast-rec b (/ n 2)))
             (* b (expt-fast-rec b (- n 1)))))))

;exercise 1.16
(define (expt-fast-iter b n)
  (define (square x) (* x x))
  (define (iter a b n)
    (cond ((= 0 n) a)
          ((= 0 (remainder n 2))
           (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
