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

;; Define a function called <=-trans that states and proves that <= is
;; transitive.

;(claim <=-trans
;       (Π ([a Nat]
;           [b Nat]
;           [c Nat])
;          (-> (<= a b)
;              (<= b c)
;              (<= a c))))

(claim +
  (-> Nat Nat Nat))

(define +
  (lambda (x y)
    (rec-Nat x
      y
      (lambda (_ y+x-1)
        (add1 y+x-1)))))

;; plus-assoc: (= Nat (+ k (+ n m)) (+ (+ k n) m)))

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
  (Pi ((k Nat) (n Nat) (m Nat))
    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(define plus-assoc
  (lambda (k n m)
    (ind-Nat k
      (mot-plus-assoc n m)
      (base-plus-assoc n m)
      (step-plus-assoc n m))))

;; End of preamble

(claim <=
       (-> Nat Nat
           U))

(define <=
  (lambda (a b)
    (Sigma ((k Nat))
       (= Nat (+ k a) b))))

(claim <=-trans
  (Pi ((a Nat)
       (b Nat)
       (c Nat))
    (-> (<= a b)
        (<= b c)
        (<= a c))))

;; there exists k where k + a == b
;; there exists j where j + b == c
;; so we can get j + k + a == c
;; which is (j + k) + a == c
;; which means there exists a (j + k) where (j + k) + a == c
;; which is our goal

(claim sum-trans
  (Pi ((a Nat) (b Nat) (c Nat) (k Nat) (j Nat))
    (-> (= Nat (+ k a) b)
        (= Nat (+ j b) c)
        (= Nat (+ j (+ k a)) c))))

(define sum-trans
  (lambda (a b c k j)
    (lambda (k+a=b j+b=c)
      (replace (symm k+a=b)
        (lambda (here) (= Nat (+ j here) c))
        j+b=c))))

;; there exists (car a<=b) where (car a<=b) + a == b
;; there exists (car b<=c) where (car b<=c) + b == c
;; we need to swap things around for replace to work
;; (symm (cdr a<=b)) = (= Nat b (+ (car a<=b) a))
;; after replace we have
;; (= Nat
;;   (+
;;     (car b<=c)
;;     (+ (car a<=b) a))
;;   c)
;; but we need
;; (= Nat
;;   (+
;;     (+ (car b<=c) (car a<=b))
;;     a)
;;   c)
;; which requires associativity plus-assoc
;; (= Nat (+ k (+ n m)) (+ (+ k n) m))

(claim plus-trans
  (Pi ((a Nat) (b Nat) (c Nat) (k Nat) (j Nat))
    (-> (= Nat (+ k a) b)
        (= Nat (+ j b) c)
        (= Nat (+ (+ j k) a) c))))

(define plus-trans
  (lambda (a b c k j)
    (lambda (k+a=b j+b=c)
      (replace (plus-assoc j k a)
        (lambda (here) (= Nat here c))
        (sum-trans a b c k j k+a=b j+b=c)))))

(define <=-trans
  (lambda (a b c)
    (lambda (a<=b b<=c)
      (cons
        (+ (car b<=c) (car a<=b))
        (plus-trans a b c (car a<=b) (car b<=c) (cdr a<=b) (cdr b<=c))
          ))))