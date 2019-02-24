#lang pie

;; Define a function called plus-assoc that states and proves that
;; + is associative.

;; (claim plus-assoc
;;  (Pi ((n Nat) (m Nat) (k Nat))
;;    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(claim + (-> Nat Nat Nat))

(define +
  (lambda (a b)
    (rec-Nat a
      b
      (lambda (_ b+a-1)
        (add1 b+a-1)))))

;; practice by first proving that n=0+n

(claim n=0+n
  (Pi ((n Nat))
    (= Nat n (+ 0 n))))

(define n=0+n
  (lambda (n)
    (same n)))

;; practice by first proving that n=n+0

(claim n=n+0
  (Pi ((n Nat))
    (= Nat n (+ n 0))))

(claim mot-n=n+0
  (-> Nat U))

(define mot-n=n+0
  (lambda (n)
    (= Nat n (+ n 0))))

(claim base-n=n+0
  (= Nat 0 (+ 0 0)))

(define base-n=n+0
  (same 0))

(claim step-n=n+0
  (Pi ((n-1 Nat))
    (-> (mot-n=n+0 n-1)
        (mot-n=n+0 (add1 n-1)))))

(define step-n=n+0
  (lambda (n-1)
    (lambda (n-1=n-1+0)
      (cong n-1=n-1+0 (+ 1)))))

(define n=n+0
  (lambda (n)
    (ind-Nat n
      mot-n=n+0
      base-n=n+0
      step-n=n+0
      )))

;; start actual exercise

(claim mot-plus-assoc
  (-> Nat Nat Nat U))

(define mot-plus-assoc
  (lambda (n m k)
    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(claim base-plus-assoc
  (Pi ((n Nat) (m Nat))
    (= Nat (+ 0 (+ n m)) (+ (+ 0 n) m))))

(define base-plus-assoc
  (lambda (n m)
    (same (+ n m))))

(claim step-plus-assoc
  (Pi ((n Nat) (m Nat) (k-1 Nat))
    (-> (mot-plus-assoc n m k-1)
        (mot-plus-assoc n m (add1 k-1)))))

(define step-plus-assoc
  (lambda (n m k-1)
    (lambda (mot-plus-assoc-k-1)
      (cong mot-plus-assoc-k-1 (+ 1)))))

(claim plus-assoc
  (Pi ((n Nat) (m Nat) (k Nat))
    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(define plus-assoc
  (lambda (n m k)
    (ind-Nat k
      (mot-plus-assoc n m)
      (base-plus-assoc n m)
      (step-plus-assoc n m))))
