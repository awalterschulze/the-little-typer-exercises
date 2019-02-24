#lang pie

;; Define a function called compose that takes (in addition to the type
;; arguments A, B, C) an argument of type (-> A B) and an argument of
;; type (-> B C) that and evaluates to a value of type (-> A C), the function
;; composition of the arguments.

(claim compose
  (Pi ((A U)
       (B U)
       (C U))
    (-> (-> A B) (-> B C) (-> A C))))

(define compose
  (lambda (A B C)
    (lambda (f g)
      (lambda (a)
        (g (f a))))))

(claim iszero?
  (-> Nat Atom))

(define iszero?
  (lambda (n)
    (rec-Nat n
      'zero
      (lambda (n-1 notzero)
        'notzero))))

(claim twin
  (-> Atom (Pair Atom Atom)))

(define twin
  (lambda (a)
    (cons a a)))

(claim double
  (-> Nat (Pair Atom Atom)))

(define double
  (compose Nat Atom (Pair Atom Atom) iszero? twin))

(check-same (Pair Atom Atom)
  (double 0) (cons 'zero 'zero))

(check-same (Pair Atom Atom)
  (double 100) (cons 'notzero 'notzero))
