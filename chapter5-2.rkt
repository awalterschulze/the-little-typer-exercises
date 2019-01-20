#lang pie

;; Define a function called maybe-last which takes (in addition to the type
;; argument for the list element) one (List E) argument and one E argument and
;; evaluates to an E with value of either the last element in the list, or the
;; value of the second argument if the list is empty.

(claim step-Last
  (Pi ((E U))
  (-> E (List E) E E)))
(define step-Last
  (lambda (E)
  (lambda (e es last)
    (rec-List es
      e
      (lambda (e_ es_ last_)
        last)
      ))))

(claim maybe-Last
  (Pi ((E U))
  (-> (List E) E E)))

(define maybe-Last
  (lambda (E)
    (lambda (es default)
      (rec-List es
        default
        (step-Last E)))))

(check-same Nat (maybe-Last Nat (:: 1 (:: 2 (:: 3 nil))) 4) 3)
(check-same Nat (maybe-Last Nat nil 4) 4)
