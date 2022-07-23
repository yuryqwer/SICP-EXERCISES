#lang sicp

; 对很小的数，很容易满足good-enough?，因此迭代
; 几次以后就停止了，误差很大，(sqrt 0.00001) ->
; 0.03135649010771716
; 对很大的数，两个大数之差的精度达不到0.001，迭代
; 会一直进行下去，永不停止
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess new-guess)
  (< (/ (abs (- old-guess new-guess))
        old-guess)
     0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))