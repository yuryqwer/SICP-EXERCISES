#lang sicp

; 练习1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 练习1.3
(define (sum-biggest-two x y z)
  (cond ((and (< x y) (< x z))
         (+ y z))
        ((and (< y x) (< y z))
         (+ x z))
        (else (+ x y))))

; 练习1.4
; 如果b > 0, 那么运算符为+。否则运算符为-。
; 最终的效果是a + |b|

; 练习1.5
; 应用序求值：永不停止。因为需要分别求值每个参数，
; 碰到(p)这个参数的时候求值会无限调用自身
;（scheme有尾递归所以不会栈溢出）
; 正则序求值：打印出0。完全展开后归约的过程：
; (test 0 {p)) -> (if (= 0 0) -> 0
;                     0
;                     (p))

; 练习1.6
; 原来的if只有在条件不满足时才会对第二个表达式进行求值
; 这样可以方便做到尾递归优化；而将if改写为常规过程后，
; 由于解释器是应用序求值的，需要先将参数的值都求出来，
; 第二个参数的值是求不完的，这样会造成栈溢出

; 练习1.7
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

; 练习1.8
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
