#lang pie

;; Define a function called append that takes an argument of type (Vec E m) and an
;; argument of type (Vect E n) and evaluates to a value of type (Vec (+ m n)), the
;; result of appending the elements of the second argument to the end of the first.

(claim + (-> Nat Nat Nat))
(define +
  (lambda (i j)
    (rec-Nat i
      j
      (lambda (i-1 sum-i-1)
        (add1 sum-i-1)
      ))))

(claim mot-append
  (Pi ((E U) (n Nat) (m Nat)) U))

(define mot-append
  (lambda (E n m)
    (-> (Vec E m) (Vec E n) (Vec E (+ m n)))))

(claim base-append
  (Pi ((E U) (n Nat))
    (mot-append E n 0)))
;;(-> (Vec E 0) (Vec E n) (Vec E (+ 0 n)))))

(define base-append
  (lambda (E n)
    (lambda (ms ns)
      ns
    )))

(claim step-append
  (Pi ((E U) (n Nat) (m-1 Nat))
   (->
    (mot-append E n m-1)
    (mot-append E n (add1 m-1)))))
    ;;(-> (Vec E m-1) (Vec E n) (Vec E (+ m-1 n)))
    ;;(-> (Vec E (add1 m-1)) (Vec E n) (Vec E (+ (add1 m-1) n))))))
  
(define step-append
  (lambda (E n m-1)
    (lambda (append-m-1)
      (lambda (ms ns)
        (vec:: (head ms) (append-m-1 (tail ms) ns))))))

;;(ind-Nat target
;;  (mot target-type) -> U
;;  base -> (mot zero)
;;  step -> n -> (mot n-1) -> (mot (add1 n-1))

(claim append
  (Pi ((E U) (m Nat) (n Nat))
    (-> (Vec E m) (Vec E n) (Vec E (+ m n)))))

(define append
  (lambda (E m n)
      (ind-Nat m
        (mot-append E n)
        (base-append E n)
        (step-append E n))))

(check-same (Vec Nat 3)
    (vec:: 1 (vec:: 2 (vec:: 3 vecnil)))
    (append Nat 1 2 (vec:: 1 vecnil) (vec:: 2 (vec:: 3 vecnil)))
)

(check-same (Vec Nat 3)
    (vec:: 3 (vec:: 2 (vec:: 1 vecnil)))
    (append Nat 2 1 (vec:: 3 (vec:: 2 vecnil)) (vec:: 1 vecnil))
)