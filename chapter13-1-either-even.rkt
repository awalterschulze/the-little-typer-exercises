#lang pie

;; Define a function called nOrSuccnIsEven that states and proves that for all Nats n, either
;; n is Even or n+1 is Even.

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

(claim nOrSuccnIsEven
  (Pi ((n Nat))
    (Either (Even n) (Even (add1 n)))))

(claim mot-nat-even
  (Pi ((n Nat))
    U))

(define mot-nat-even
  (lambda (n)
    (Either (Even n) (Even (add1 n)))))

(claim step-nat-even
  (Pi ((n-1 Nat))
    (->
     (Either (Even n-1) (Even (add1 n-1)))
     (Either (Even (add1 n-1)) (Even (add1 (add1 n-1)))))))

(claim mot-either-even
  (Pi ((n-1 Nat)
       (e (Either (Even n-1) (Even (add1 n-1)))))
    U))

(define mot-either-even
  (lambda (n-1 e)
    (Either (Even (add1 n-1)) (Even (add1 (add1 n-1))))))

(claim left-either-even
  (Pi ((n-1 Nat)
       (e (Even n-1)))
       (Even (add1 (add1 n-1)))))

;; (cdr (Even n-1))
;; (= Nat n-1 (double half-n-1))
;; (= Nat (add1 (add1 n-1)) (add1 (add1 (double (half-n-1)))

(define left-either-even
  (lambda (n-1 e)
    (cons (add1 (car e))
      (cong (cdr e) (+ 2)))))

(claim right-either-even
  (Pi ((n-1 Nat)
       (e (Even (add1 n-1))))
       (Even (add1 n-1))))

(define right-either-even
  (lambda (n-1 e)
    e))

(define step-nat-even
  (lambda (n-1)
    (lambda (either-n-1)
      (ind-Either either-n-1
        (mot-either-even n-1)
        (lambda (left-even-n-1) (right (left-either-even n-1 left-even-n-1)))
        (lambda (right-even-1+n-1) (left (right-either-even n-1 right-even-1+n-1)))))))

(define nOrSuccnIsEven
  (lambda (n)
    (ind-Nat n
      mot-nat-even
      (left (cons 0 (same 0)))
      step-nat-even)))
