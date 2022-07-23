#lang sicp

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve-cbrt guess x))
      (improve-cbrt guess x)
      (cbrt-iter (improve-cbrt guess x)
                 x)))

(define (square x) (* x x))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x)
  (cbrt-iter 1.0 x))