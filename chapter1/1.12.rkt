#lang sicp

(define (pascal-triangle i j)
  (if (or (= j 1) (= i j))
      1
      (+ (pascal-triangle (- i 1) (- j 1))
         (pascal-triangle (- i 1) j))))