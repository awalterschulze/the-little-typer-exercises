#lang pie

;;; Define a function called exp that takes two Nat arguments and evaluates to a Nat.
;;; exp evaluates to the exponentiation, a^b, of the two passed arguments.
;;;

(claim + (-> Nat Nat Nat))
(define +
  (lambda (i j)
    (rec-Nat j
      i
      (lambda (j-1 sum-j-1)
        (add1 sum-j-1)
      )
    )
  )
)

(claim step-* (-> Nat Nat Nat Nat))
(define step-*
  (lambda (j j-1 mul-1)
    (+ j mul-1)
    ))

(claim * (-> Nat Nat Nat))
(define *
  (lambda (i j)
    (rec-Nat i
      0
      (step-* j)
    )
  )
)

(check-same Nat (* 3 4) 12)
(check-same Nat (* 0 4) 0)
(check-same Nat (* 4 0) 0)
(check-same Nat (* 4 100) 400)

(claim step-^ (-> Nat Nat Nat Nat))
(define step-^
  (lambda (j j-1 pow-1)
    (* j pow-1)
  ))

(claim ^ (-> Nat Nat Nat))
(define ^
  (lambda (i j)
    (rec-Nat j
      1
      (step-^ i)
      )))

(check-same Nat (^ 0 2) 0)
(check-same Nat (^ 2 0) 1)
(check-same Nat (^ 2 2) 4)
(check-same Nat (^ 2 3) 8)
(check-same Nat (^ 10 3) 1000)
