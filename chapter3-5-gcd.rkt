#lang pie

(claim +
  (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat n
      m
      (lambda (n-1) (add1 n-1)))))

(check-same Nat (+ 1 3) 4)
(check-same Nat (+ 3 1) 4)
(check-same Nat (+ 3 0) 3)
(check-same Nat (+ 0 3) 3)
(check-same Nat (+ 100 200) 300)

(claim *
  (-> Nat Nat Nat))
(define *
  (lambda (m n)
    (iter-Nat n
      0
      (lambda (n-1) (+ m n-1)))))

(check-same Nat (* 3 4) 12)
(check-same Nat (* 0 4) 0)
(check-same Nat (* 4 0) 0)
(check-same Nat (* 4 100) 400)

(claim dec
  (-> Nat Nat))
(define dec
  (λ (n)
    (which-Nat n
      0
      (lambda (n-1) n-1))))

(check-same Nat (dec 0) 0)
(check-same Nat (dec 1) 0)
(check-same Nat (dec 8) 7)
(check-same Nat (dec 70) 69)

(claim -
  (-> Nat Nat Nat))
(define -
  (lambda (m n)
    (iter-Nat n
      m
      dec)))

(check-same Nat (- 5 2) 3)
(check-same Nat (- 2 3) 0)
(check-same Nat (- 7 0) 7)
(check-same Nat (- 351 420) 0)

(claim abs-diff
  (-> Nat Nat Nat))
(define abs-diff
  (lambda (m n) (+ (- m n) (- n m))))

(check-same Nat (abs-diff 5 2) 3)
(check-same Nat (abs-diff 2 3) 1)
(check-same Nat (abs-diff 7 0) 7)
(check-same Nat (abs-diff 351 420) 69)

(claim sg
  (-> Nat Nat))
(define sg
  (lambda (x)
    (which-Nat x
      0
      (lambda (_) 1))))

(check-same Nat (sg 0) 0)
(check-same Nat (sg 1) 1)
(check-same Nat (sg 42) 1)
(check-same Nat (sg 9) 1)

(claim rev-sg
  (-> Nat Nat))
(define rev-sg
  (lambda (x)
    (which-Nat x
      1
      (lambda (_) 0))))

(check-same Nat (rev-sg 0) 1)
(check-same Nat (rev-sg 1) 0)
(check-same Nat (rev-sg 42) 0)
(check-same Nat (rev-sg 9) 0)

(claim rem
  (-> Nat Nat Nat))
(define rem
  (lambda (x y)
    (rec-Nat x
      0
      (lambda (_ so-far)
        (* (add1 so-far) (sg (abs-diff y (add1 so-far))))))))

(check-same Nat (rem 5 2) 1)
(check-same Nat (rem 15 1) 0)
(check-same Nat (rem 0 3) 0)
(check-same Nat (rem 54 7) 5)
(check-same Nat (rem 356 48) 20)

(claim divides
  (-> Nat Nat Nat))
(define divides
  (lambda (x y) (rev-sg (rem y x))))

(check-same Nat (divides 1 7) 1)
(check-same Nat (divides 6 20) 0)
(check-same Nat (divides 3 73) 0)
(check-same Nat (divides 15 375) 1)

(claim gcd
  (-> Nat Nat Nat))
(define gcd
  (lambda (x y)
    ((the (-> Nat Nat)
       (lambda (z)
         (rec-Nat z
           y
           (lambda (z-1 so-far)
             (which-Nat (* (divides (add1 z-1) x) (divides (add1 z-1) y))
               so-far
               (lambda (_) (add1 z-1))))))) x)))

(check-same Nat (gcd 0 5) 5)
(check-same Nat (gcd 7 0) 7)
(check-same Nat (gcd 24 36) 12)
(check-same Nat (gcd 72 48) 24)
(check-same Nat (gcd 140 350) 70)
(check-same Nat (gcd 15 8) 1)
(check-same Nat (gcd 99 100) 1)