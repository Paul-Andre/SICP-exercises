#lang planet neil/sicp

(define (gdc . args)
  (define (single-gdc a b)
    (if (= b 0)
        a
        (single-gdc b (remainder a b))))
  (if (null? args)
      0
      (single-gdc (car args) (apply gdc (cdr args)))))