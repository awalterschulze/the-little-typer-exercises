#lang pie

;; Define a function called sumOfTwoOddsIsEven that states and proves that the sum of
;; two Odd Nats is Even.

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

(claim Odd
       (-> Nat
           U))

(define Odd
  (λ (n)
    (Σ ([haf Nat])
       (= Nat n (add1 (double haf))))))

;; End of Preamble

(claim double+add1
  (Pi ((n Nat)
       (m Nat))
    (= Nat (+ (add1 (double n)) (add1 (double m))) (double (add1 (+ n m))))))

(claim mot-double+add1
  (Pi ((m Nat)
       (n Nat))
    U))

(define mot-double+add1
  (lambda (m n)
    (= Nat (+ (add1 (double n)) (add1 (double m))) (double (add1 (+ n m))))))

(claim step-double+add1
  (Pi ((m Nat)
       (n-1 Nat))
    (->
     (= Nat (+ (add1 (double n-1)) (add1 (double m))) (double (add1 (+ n-1 m))))
     (= Nat (+ (add1 (double (add1 n-1))) (add1 (double m))) (double (add1 (+ (add1 n-1) m)))))))

(define step-double+add1
  (lambda (m n-1)
    (lambda (+double=double+)
      (cong +double=double+ (+ 2)))))

(define double+add1
  (lambda (n m)
    (ind-Nat n
      (mot-double+add1 m)
      (same (double (add1 m)))
      (step-double+add1 m))))


(claim sumOfTwoOddsIsEven
       (Π ([n Nat]
           [m Nat])
          (-> (Odd n) (Odd m)
              (Even (+ n m)))))

;; (cdr (Odd n)), where haf-n is (car (Odd n))
;; (= Nat n (add1 (double haf-n))
;; (cdr (Odd m)), where haf-m is (car (Odd m))
;; (= Nat m (add1 (double haf-m))
;; same (+ n m)
;; (= Nat (+ n m) (+ n m))
;; using replace we can get
;; (= Nat (+ n m) (+ (add1 (double haf-n) (add1 (double haf-m)))
;; now we can use double+add1 to get
;; (= Nat (+ n m) (double (add1 (+ haf-n haf-m))))

(define sumOfTwoOddsIsEven
  (lambda (n m)
    (lambda (odd-n odd-m)
      ;; car of odd is haf
      (cons (add1 (+ (car odd-n) (car odd-m)))
        ;; (= Nat (+ n m) (double (add1 (+ (car even-n) (car even-m)))))
        (replace (double+add1 (car odd-n) (car odd-m))
          (lambda (here) (= Nat (+ n m) here))
          ;; (= Nat (+ n m) (+ (add1 (double (car odd-n))) (add1 (double (car odd-m))))
          (replace (cdr odd-m)
            (lambda (here) (= Nat (+ n m) (+ (add1 (double (car odd-n))) here)))
            ;; (= Nat (+ n m) (+ (add1 (double (car odd-n))) m))
            (replace (cdr odd-n)
              (lambda (here) (= Nat (+ n m) (+ here m)))
              (same (+ n m)))))))))
