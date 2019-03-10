#lang pie

;; In the following exercises we'll use the function called <= that takes two
;; Nat arguments a, b and evaluates to a type representing the proposition
;; that a is less than or equal to b.

;(claim <=
;       (-> Nat Nat
;           U))
;
;(define <=
;  (λ (a b)
;    (Σ ([k Nat])
;       (= Nat (+ k a) b))))

;; Using <=, state and prove that 1 is less than or equal to 2.

(claim +
  (-> Nat Nat Nat))

(define +
  (lambda (x y)
    (rec-Nat x
      y
      (lambda (_ y+x-1)
        (add1 y+x-1)))))

;; End of preamble

(claim <=
       (-> Nat Nat
           U))

(define <=
  (lambda (a b)
    (Sigma ((k Nat))
       (= Nat (+ k a) b))))

(claim 1<=2
  (<= 1 2))

(define 1<=2
  (cons 1 (same 2)))

