#lang planet neil/sicp
;sine approximation

(define (sine-approx a)
  (cond ((> 0.01 (abs a)) a)
        (else (let ((b (sine-approx (/ a 3))))
                (+ (* 3 b)
                   (* -4 b b b))))))

(sine-approx 3.14)
