#lang sicp

;;;;;;;;;;;;;;;;;;;; 练习1.2 ;;;;;;;;;;;;;;;;;;;;
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;;;;;;;;;;;;;;;;;;; 练习1.3 ;;;;;;;;;;;;;;;;;;;;
(define (sum-biggest-two x y z)
  (cond ((and (< x y) (< x z))
         (+ y z))
        ((and (< y x) (< y z))
         (+ x z))
        (else (+ x y))))

;;;;;;;;;;;;;;;;;;;; 练习1.4 ;;;;;;;;;;;;;;;;;;;;
; 如果b > 0, 那么运算符为+。否则运算符为-。
; 最终的效果是a + |b|

;;;;;;;;;;;;;;;;;;;; 练习1.5 ;;;;;;;;;;;;;;;;;;;;
; 应用序求值：永不停止。因为需要分别求值每个参数，
; 碰到(p)这个参数的时候求值会无限调用自身
;（scheme有尾递归所以不会栈溢出）
; 正则序求值：打印出0。完全展开后归约的过程：
; (test 0 {p)) -> (if (= 0 0) -> 0
;                     0
;                     (p))

;;;;;;;;;;;;;;;;;;;; 练习1.6 ;;;;;;;;;;;;;;;;;;;;
; 原来的if只有在条件不满足时才会对第二个表达式进行求值
; 这样可以方便做到尾递归优化；而将if改写为常规过程后，
; 由于解释器是应用序求值的，需要先将参数的值都求出来，
; 第二个参数的值是求不完的，这样会造成栈溢出

;;;;;;;;;;;;;;;;;;;; 练习1.7 ;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;; 练习1.8 ;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;; 练习1.9 ;;;;;;;;;;;;;;;;;;;;
; 第一个过程是递归的
; (+ 4 5) ->
; (inc (+ 3 5)) ->
; (inc (inc (+ 2 5))) ->
; (inc (inc (inc (+ 1 5)))) ->
; (inc (inc (inc (inc (+ 0 5))))) ->
; (inc (inc (inc (inc 5)))) ->
; (inc (inc (inc 6))) ->
; (inc (inc 7)) ->
; (inc 8) ->
; 9
; 第二个过程是迭代的
; (+ 4 5) ->
; (+ 3 6) ->
; (+ 2 7) ->
; (+ 1 8) ->
; (+ 0 9) ->
; 9

;;;;;;;;;;;;;;;;;;;; 练习1.10 ;;;;;;;;;;;;;;;;;;;;
; (A 1 10) ->
; (A 0 (A 1 9)) ->
; (A 0 (A 0 (A 1 8))) ->
; (A 0 (A 0 ... (A 1 1)...)) ->
; (* 2 (* 2 ... 2...)) ->
; 2^10 = 1024

; (A 2 4) ->
; (A 1 (A 2 3)) ->
; (A 1 (A 1 (A 2 2))) ->
; (A 1 (A 1 (A 1 (A 2 1)))) ->
; (A 1 (A 1 (A 1 2))) ->
; 2^(2^(2^2)) = 2^(2^4) = 2^16 = 65536

; (A 3 3) ->
; (A 2 (A 3 2)) ->
; (A 2 (A 2 (A 3 1))) ->
; (A 2 (A 2 2)) ->
; (A 2 (A 1 (A 2 1))) ->
; (A 2 (A 1 2)) ->
; (A 2 4) ->
; 65536

; (f n) -> 2n
; (g n) -> 2^n
; (h n) -> 2的2的...2次方（n个2）

;;;;;;;;;;;;;;;;;;;; 练习1.11 ;;;;;;;;;;;;;;;;;;;;
; 递归计算过程:
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; 迭代计算过程:
(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n)))

(define (f-iter a b c count)
  (if (< count 3)
      a
      (f-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))

;;;;;;;;;;;;;;;;;;;; 练习1.12 ;;;;;;;;;;;;;;;;;;;;
(define (pascal-triangle i j)
  (if (or (= j 1) (= i j))
      1
      (+ (pascal-triangle (- i 1) (- j 1))
         (pascal-triangle (- i 1) j))))

;;;;;;;;;;;;;;;;;;;; 练习1.13 ;;;;;;;;;;;;;;;;;;;;
; 略

;;;;;;;;;;;;;;;;;;;; 练习1.14 ;;;;;;;;;;;;;;;;;;;;
; 略

;;;;;;;;;;;;;;;;;;;; 练习1.15 ;;;;;;;;;;;;;;;;;;;;
; a)
; (sine 12.15) ->
; (p (sine 4.05)) ->
; (p (p (sine 1.35))) ->
; (p (p (p (sine 0.45)))) ->
; (p (p (p (p (sine 0.15))))) ->
; (p (p (p (p (p (sine 0.05))))))
; p被使用5次

; b)
; 空间和时间复杂度都为Θ(log a)

;;;;;;;;;;;;;;;;;;;; 练习1.16 ;;;;;;;;;;;;;;;;;;;;
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b)
                                   (/ n 2)
                                   a))
        (else (fast-expt-iter b
                              (- n 1)
                              (* b a)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;;;;;;;;;;;;;;;;;;;; 练习1.17 ;;;;;;;;;;;;;;;;;;;;
(define (double x) (* 2 x))

(define (halve x) (/ x 2))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-multi (double a)
                               (halve b)))
        (else (+ a (fast-multi a (- b 1))))))

;;;;;;;;;;;;;;;;;;;; 练习1.18 ;;;;;;;;;;;;;;;;;;;;
(define (fast-multi-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (fast-multi-iter (double a)
                                    (halve b)
                                    c))
        (else (fast-multi-iter a
                               (- b 1)
                               (+ a c)))))

(define (fast-multi a b)
  (fast-multi-iter a b 0))