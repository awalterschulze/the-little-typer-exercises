#lang pie

;; Define a function called a=b->a+n=b+n that states and proves that
;; a = b implies a+n = b+n for all Nats a, b, n.

(claim + (-> Nat Nat Nat))

(define +
  (lambda (a b)
    (rec-Nat a
      b
      (lambda (_ b+a-1)
        (add1 b+a-1)))))

(claim a=b->a+n=b+n
  (Pi ((a Nat) (b Nat) (n Nat) (a=b (= Nat a b)))
    (= Nat (+ a n) (+ b n))))
    
(define a=b->a+n=b+n
  (lambda (a b n a=b)
    (cong a=b
      (the (-> Nat Nat)
        (lambda (x) (+ x n))))))
