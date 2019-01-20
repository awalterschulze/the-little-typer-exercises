#lang pie

;; Define a function called filter-list which takes (in addition to the type
;; argument for the list element) one (-> E Nat) argument representing a
;; predicate and one (List E) argument.
;;
;; The function evaluates to a (List E) consisting of elements from the list
;; argument where the predicate is true.
;;
;; Consider the predicate to be false for an element if it evaluates to zero,
;; and true otherwise.

(claim step-filter
  (Pi ((E U))
    (-> (-> E Nat) E (List E) (List E) (List E))))

(define step-filter
  (lambda (E)
    (lambda (pred h _ filtered)
      (rec-Nat (pred h)
        filtered
        (lambda (n-1 _)
          (:: h filtered))))))

(claim filter-list
  (Pi ((E U))
    (-> (-> E Nat) (List E) (List E))))

(define filter-list
  (lambda (E)
    (lambda (pred es)
      (rec-List es
        (the (List E) nil)
        (step-filter E pred)))))

(claim not
  (-> Nat Nat))
(define not
  (lambda (n)
    (rec-Nat n
      1
      (lambda (n-1 notzero)
        0))))

(claim id
  (-> Nat Nat))
(define id
  (lambda (n) n))

(check-same (List Nat)
  (filter-list
    Nat
    id
    (:: 1 (:: 2 (:: 3 nil))))
  (:: 1 (:: 2 (:: 3 nil))))

(check-same (List Nat)
  (filter-list
    Nat
    id
    (:: 1 (:: 0 (:: 2 (:: 3 nil)))))
  (:: 1 (:: 2 (:: 3 nil))))

(check-same (List Nat)
  (filter-list
    Nat
    (lambda (n) (not (id n)))
    (:: 1 (:: 0 (:: 2 (:: 3 nil)))))
  (:: 0 nil))
