#lang pie

;; Define a function called plus-comm that states and proves that
;; + is commutative
;;
;; (claim plus-comm
;;  (Pi ((n Nat) (m Nat))
;;    (= Nat (+ n m) (+ m n))))
;;
;; Bonus: Write the solution using the trans elimiator instead of
;; the replace elimiator.
;; https://docs.racket-lang.org/pie/index.html#%28def._%28%28lib._pie%2Fmain..rkt%29._trans%29%29
;;
;; (trans target-1 target-2) -> (= X from to)
;; target-1 : (= X from middle)
;; target-2 : (= X middle to)

(claim + (-> Nat Nat Nat))

(define +
  (lambda (a b)
    (rec-Nat a
      b
      (lambda (_ b+a-1)
        (add1 b+a-1)))))

;; prove 0 + n = n + 0

(claim mot-zero+=+zero
  (-> Nat U))

(define mot-zero+=+zero
  (lambda (n)
    (= Nat (+ 0 n) (+ n 0))))

(claim step-zero+=+zero
  (Pi ((n-1 Nat))
  (-> (= Nat (+ 0 n-1) (+ n-1 0))
      (= Nat (+ 0 (add1 n-1)) (+ (add1 n-1) 0)))))

(define step-zero+=+zero
  (lambda (n-1)
    (lambda (zero+n-1=n-1+zero)
      (cong zero+n-1=n-1+zero (+ 1)))))

(claim zero+=+zero
  (Pi ((n Nat))
  (= Nat (+ 0 n) (+ n 0))))

(define zero+=+zero
  (lambda (n)
    (ind-Nat n
      mot-zero+=+zero
      (same 0)
      step-zero+=+zero)))

;; back to plus-comm

(claim mot-plus-comm
  (-> Nat Nat U))

(define mot-plus-comm
  (lambda (m n)
    (= Nat (+ n m) (+ m n))))

;;what the claim for step-plus-comm would look like:
;;(claim step-plus-comm
;;  (Pi ((m Nat) (n-1 Nat))
;;    (-> (= Nat (+ n-1 m) (+ m n-1))
;;        (= Nat (+ (add1 n-1) m) (+ m (add1 n-1))))))

;; because of this fact

(claim n+1+m=1+n+m
  (Pi ((m Nat) (n Nat))
    (= Nat (+ (add1 n) m) (add1 (+ n m)))))

(define n+1+m=1+n+m
  (lambda (m n)
    (same (add1 (+ n m)))))

;; this is the same type as step-plus-comm
;; (claim step-plus-comm
;;  (Pi ((m Nat) (n-1 Nat))
;;    (-> (= Nat (+ n-1 m) (+ m n-1))
;;        (= Nat (add1 (+ n-1 m)) (+ m (add1 n-1))))))

;; if we can prove this

(claim n+m+1=1+n+m
  (Pi ((m Nat) (n Nat))
    (= Nat (+ n (add1 m)) (add1 (+ n m)))))

;; then
;; this is the same type as step-plus-comm
;; (claim step-plus-comm
;;  (Pi ((m Nat) (n-1 Nat))
;;    (-> (= Nat (+ n-1 m) (+ m n-1))
;;        (= Nat (add1 (+ n-1 m)) (add1 (+ m n-1))))))
;; which means we simply need to cong add1

(claim mot-n+m+1=1+n+m
  (-> Nat Nat U))

(define mot-n+m+1=1+n+m
  (lambda (m n)
    (= Nat (+ n (add1 m)) (add1 (+ n m)))))

(claim step-mot-n+m+1=1+n+m
  (Pi ((m Nat) (n-1 Nat))
    (-> (= Nat (+ n-1 (add1 m)) (add1 (+ n-1 m)))
        (= Nat (+ (add1 n-1) (add1 m)) (add1 (+ (add1 n-1) m))))))

(define step-mot-n+m+1=1+n+m
  (lambda (m n-1)
    (lambda (n-1+m+1=1+n-1+m)
      (cong n-1+m+1=1+n-1+m (+ 1)))))

(define n+m+1=1+n+m
  (lambda (m n)
    (ind-Nat n
      (mot-n+m+1=1+n+m m)
      (same (add1 m))
      (step-mot-n+m+1=1+n+m m)
    )))

;; proved n + (add1 m) = add1 (n + m)

;; unfortunately we need to symmetric version

(claim 1+n+m=n+m+1
  (Pi ((m Nat) (n-1 Nat))
    (= Nat (add1 (+ m n-1)) (+ m (add1 n-1)))))

(define 1+n+m=n+m+1
  (lambda (m n-1)
    (symm (n+m+1=1+n+m n-1 m))))

;; now we have everything we need to complete step-plus-comm

(claim step-plus-comm
  (Pi ((m Nat) (n-1 Nat))
    (-> (= Nat (+ n-1 m) (+ m n-1))
        (= Nat (+ (add1 n-1) m) (+ m (add1 n-1))))))

;; so now we get to this, the fact n+1+m=1+n+m and the proof 1+n+m=n+m+1
;; (claim step-plus-comm
;;  (Pi ((m Nat) (n-1 Nat))
;;    (-> (= Nat (+ n-1 m) (+ m n-1))
;;        (= Nat (add1 (+ n-1 m)) (add1 (+ m n-1))))))

;; push-add1 will push the add1 down to n-1
(claim push-add1
  (Pi ((n-1 Nat) (m Nat))
  (->
   (= Nat (add1 (+ n-1 m)) (add1 (+ m n-1)))
   (= Nat (+ (add1 n-1) m) (+ m (add1 n-1))))))

;; here is the main proof we are going to need
;; (= Nat (add1 (+ m n-1)) (+ m (add1 n-1)))

(define push-add1
  (lambda (n-1 m)
    (lambda (1+n-1+m=1+m+n-1)
      (replace (1+n+m=n+m+1 m n-1)
        (lambda (toreplace) (= Nat (add1 (+ n-1 m)) toreplace))
        1+n-1+m=1+m+n-1))))

;; an alternative implementation of push-add1 using trans
(claim push-add1-trans
  (Pi ((n-1 Nat) (m Nat))
  (->
   (= Nat (add1 (+ n-1 m)) (add1 (+ m n-1)))
   (= Nat (+ (add1 n-1) m) (+ m (add1 n-1))))))

;; (trans target-1 target-2) -> (= X from to)
;; target-1 : (= X from middle)
;; target-2 : (= X middle to)
;; target-1: (= Nat (add1 (+ n-1 m)) (add1 (+ m n-1))
;; target-2: (= Nat (add1 (+ m n-1)) (+ m (add1 n-1))
;; result : (= Nat (add1 (+ n-1 m) (+ m (add1 n-1))
(define push-add1-trans
  (lambda (n-1 m)
    (lambda (1+n-1+m=1+m+n-1)
      (trans 1+n-1+m=1+m+n-1 (1+n+m=n+m+1 m n-1)))))

;; final implementation of step-plus-comm

(define step-plus-comm
  (lambda (m n-1)
    (lambda (n-1+m=m+n-1)
      (push-add1-trans n-1 m
        (cong n-1+m=m+n-1 (+ 1))))))

(claim plus-comm
  (Pi ((n Nat) (m Nat))
    (= Nat (+ n m) (+ m n))))

(define plus-comm
  (lambda (n m)
    (ind-Nat n
      (mot-plus-comm m)
      (zero+=+zero m)
      (step-plus-comm m))))
