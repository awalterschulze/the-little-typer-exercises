#lang pie

;; Define a function called zero+n=n that states and proves that
;; 0+n = n for all Nat n.

(claim + (-> Nat Nat Nat))

(define +
  (lambda (a b)
    (rec-Nat a
      b
      (lambda (_ b+a-1)
        (add1 b+a-1)))))

(claim zero+n=n
  (Pi ((n Nat))
    (= Nat (+ 0 n) n)))

(define zero+n=n
  (lambda (n)
    (same n)))