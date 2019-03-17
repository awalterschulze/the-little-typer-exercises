#lang pie

;; Define a function called sumOfTwoEvensIsEven that states and proves that the sum
;; of two Even Nats is Even.

; (claim sumOfTwoEvensIsEven
;       (Π ([n Nat]
;           [m Nat])
;          (-> (Even n) (Even m)
;              (Even (+ n m)))))

(claim +
       (-> Nat Nat
           Nat))

(define +
  (λ (a b)
    (rec-Nat a
             b
             (λ (_ a+b-1)
               (add1 a+b-1)))))

(claim double
       (-> Nat
           Nat))

(define double
  (λ (n)
    (rec-Nat n
             0
             (λ (_ n+n-2)
               (+ 2 n+n-2)))))

(claim Even
       (-> Nat
           U))

(define Even
  (λ (n)
    (Σ ([half Nat])
       (= Nat n (double half)))))

;; End of Preamble

(claim double+
  (Pi ((n Nat)
       (m Nat))
    (= Nat (+ (double n) (double m)) (double (+ n m)))))

(claim mot-double+
  (Pi ((m Nat)
       (n Nat))
    U))

(define mot-double+
  (lambda (m n)
    (= Nat (+ (double n) (double m)) (double (+ n m)))))

(claim step-double+
  (Pi ((m Nat)
       (n-1 Nat))
    (->
     (= Nat (+ (double n-1) (double m)) (double (+ n-1 m)))
     (= Nat (+ (double (add1 n-1)) (double m)) (double (+ (add1 n-1) m))))))

(define step-double+
  (lambda (m n-1)
    (lambda (+double=double+)
      (cong +double=double+ (+ 2)))))

(define double+
  (lambda (n m)
    (ind-Nat n
      (mot-double+ m)
      (same (double m))
      (step-double+ m))))


(claim sumOfTwoEvensIsEven
  (Pi ((n Nat)
       (m Nat))
    (-> (Even n) (Even m)
      (Even (+ n m)))))

;; the cdr of (Even n) is, where half-n is car of (Even n)
;; (= Nat n (double half-n))
;; the cdr of (Even m) is, where half-m is car of (Even m)
;; (= Nat m (double half-m))
;; (same (+ n m))
;; (= Nat (+ n m) (+ n m))
;; we can use replace to get
;; (= Nat (+ n m) (+ (double half-n) (double half-m)))
;; then we need double+
;; (= Nat (+ (double n) (double m)) (double (+ n m)))

(define sumOfTwoEvensIsEven
  (lambda (n m)
    (lambda (even-n even-m)
      ;; car of even is half
      (cons (+ (car even-n) (car even-m))
        ;; (= Nat (+ n m) (double (+ (car even-n) (car even-m))))
        (replace (double+ (car even-n) (car even-m))
          (lambda (here) (= Nat (+ n m) here))
          ;; (= Nat (+ n m) (+ (double (car even-n)) (double (car even-m)))
          (replace (cdr even-m)
            (lambda (here) (= Nat (+ n m) (+ (double (car even-n)) here)))
            ;; (= Nat (+ n m) (+ (double (car even-n)) m))
            (replace (cdr even-n)
              (lambda (here) (= Nat (+ n m) (+ here m)))
              (same (+ n m)))))))))

