#lang pie

;; Extend the definitions of kar and kdr (frame 4.42) so they work with arbirary
;; Pairs (instead of just for Pair Nat Nat).

;; # frame 4.42
;;
;; (define kar
;;   (lambda (p)
;;     (elim-Pair
;;       Nat Nat
;;       Nat
;;       p
;;       (lambda (a d)
;;         a))))
;;
;; (define kdr
;;   (lambda (p)
;;     (elim-Pair
;;       Nat Nat
;;       Nat
;;       p
;;       (lambda (a d)
;;         d))))

(claim elim-Pair
  (Pi ((A U)
       (D U)
       (X U))
    (-> (Pair A D)
        (-> A D X)
        X)))

(define elim-Pair
  (lambda (A D X)
    (lambda (p f)
      (f (car p) (cdr p)))))

(claim kar
  (Pi ((A U)
        (D U))
  (-> (Pair A D) A)))

(define kar
  (lambda (A D)
    (lambda (p)
      (elim-Pair
        A D
        A
        p
        (lambda (a d)
          a)))))

(check-same Nat (kar Nat Nat (cons 5 4)) 5)
(check-same Atom (kar Atom Nat (cons 'a 100)) 'a)
(check-same Atom (kar Atom (Pair Atom Atom) (cons 'a (cons 'b 'c))) 'a)

(claim kdr
  (Pi ((A U)
        (D U))
  (-> (Pair A D) D)))

(define kdr
  (lambda (A D)
    (lambda (p)
      (elim-Pair
        A D
        D
        p
        (lambda (a d)
          d)))))

(check-same Nat (kdr Nat Nat (cons 5 4)) 4)
(check-same Nat (kdr Atom Nat (cons 'a 100)) 100)
(check-same (Pair Atom Atom) (kdr Atom (Pair Atom Atom) (cons 'a (cons 'b 'c))) (cons 'b 'c))
