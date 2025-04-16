#lang pie

(claim step-+
  (-> Nat Nat Nat))
(define step-+
  (lambda (_ n-1) (add1 n-1)))

(claim +
  (-> Nat Nat Nat))
(define +
  (lambda (n m)
    (rec-Nat n
      m
      step-+)))

(claim *
  (-> Nat Nat Nat))
(define *
  (lambda (m n)
    (iter-Nat m
      zero
      (lambda (m-1) (+ n m-1)))))

(claim plus-assoc
 (Pi ((x Nat) (y Nat) (z Nat))
   (= Nat (+ x (+ y z)) (+ (+ x y) z))))
(define plus-assoc
  (lambda (x y z)
    (ind-Nat x
      (lambda (x) (= Nat (+ x (+ y z)) (+ (+ x y) z)))
      (same (+ y z))
      (lambda (_ ih) 
        (cong ih (+ 1))))))

(claim mul-distrib-right
  (Pi ((x Nat) (y Nat) (z Nat))
    (= Nat (* (+ x y) z) (+ (* x z) (* y z)))))
(define mul-distrib-right
  (lambda (x y z)
    (ind-Nat x
      (lambda (x) (= Nat (* (+ x y) z) (+ (* x z) (* y z))))
      (same (* y z))
      (lambda (x ih) (trans (cong ih (+ z)) (plus-assoc z (* x z) (* y z)))))))

(claim mul-assoc
  (Pi ((x Nat) (y Nat) (z Nat))
    (= Nat (* x (* y z)) (* (* x y) z))))
(define mul-assoc
  (lambda (x y z)
    (ind-Nat x
      (lambda (x) (= Nat (* x (* y z)) (* (* x y) z)))
      (same zero)
      (lambda (x ih) (trans (cong ih (+ (* y z))) (symm (mul-distrib-right y (* x y) z)))))))