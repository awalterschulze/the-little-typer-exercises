#lang pie

;; Define a function called sum-List that takes one List Nat argument and
;; evaluates to a Nat, the sum of the Nats in the list.

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

(claim step-sum
  (-> Nat (List Nat) Nat Nat))
(define step-sum
  (lambda (e es sum)
    (+ e sum)))

(claim sum-List
  (-> (List Nat) Nat))

(define sum-List
  (lambda (es)
    (rec-List es
      0
      step-sum)))



(check-same Nat (sum-List (:: 1 (:: 2 (:: 3 nil)))) 6)
(check-same Nat (sum-List nil) 0)
