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
