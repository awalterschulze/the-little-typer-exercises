#lang pie

;;; Define a function called max that takes two Nat arguments and evaluates to a Nat.
;;; max evaluates to the larger of the two passed arguments.

(claim min1 (-> Nat Nat))
(define min1
  (lambda (x)
    (rec-Nat x 
      0
      (lambda (x-1 res)
        x-1
      )
    )
  )
)

(check-same Nat (min1 5) 4)
(check-same Nat (min1 0) 0)
(check-same Nat (min1 1) 0)

(claim - (-> Nat Nat Nat))
(define -
  (lambda (x y)
    (rec-Nat y
      x
      (lambda (x-1 res)
        (min1 res)
      ))))

(check-same Nat (- 5 1) 4)
(check-same Nat (- 5 4) 1)
(check-same Nat (- 5 5) 0)
(check-same Nat (- 1 5) 0)

(claim zero? (-> Nat Atom))
(define zero?
  (lambda (x)
    (rec-Nat x
      't
      (lambda (x-1 res)
        'nil
        ))))

(check-same Atom (zero? 0) 't)
(check-same Atom (zero? 5) 'nil)

(claim max (-> Nat Nat Nat))
(define max
  (lambda (x y)
    (rec-Nat (- x y)
      y
      (lambda (x-1 res)
        x
        ))))

(check-same Nat (max 1 3) 3)
(check-same Nat (max 3 1) 3)
(check-same Nat (max 0 1) 1)
(check-same Nat (max 1 0) 1)

(check-same Nat (max 19 20) 20)
(check-same Nat (max 19 19) 19)

;; A second, better implementation, that I learned from Ayman Osman,
;; https://gist.github.com/aymanosman/9db1db11755ef4b4759c4ee9a4bdb515
;; where we deconstruct both x and y at the "same" time.

(claim max2 (-> Nat Nat Nat))
(define max2
  (lambda (x)
    (rec-Nat x
      (the (-> Nat Nat) (lambda (y) y))
      (lambda (x-1 max-x-1)
        (lambda (y) 
          (add1 (rec-Nat y
            x-1
            (lambda (y-1 _)
              (max-x-1 y-1)
            ))))))))

(check-same Nat (max2 2 4) 4)
(check-same Nat (max2 4 2) 4)
(check-same Nat (max2 0 1) 1)
(check-same Nat (max2 1 0) 1)

(check-same Nat (max2 19 20) 20)
(check-same Nat (max2 19 19) 19)
