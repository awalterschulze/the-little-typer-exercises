#lang pie

;; Define a function called same-cons that states and proves that
;; if you cons the same value to the front of two equal Lists then
;; the resulting Lists are also equal,
;; using replace, because this can also be done with cong,
;; but we are trying to practice chapter 9's replace.

(claim same-cons
  (Pi ((t U) (x t) (as (List t)) (bs (List t)))
    (-> (= (List t) as bs)
        (= (List t) (:: x as) (:: x bs)))))

;; replace with-new here-template where-old
;; with-new: (= old new)
;; here-template: (lambda (here) (... here ...))
;; where-old: (... old ... )
;; result: ( ... new ... )

;; result: (= (List t) (:: x as) (:: x bs)))
;; with-new: as=bs
;; here-template: (lambda (here) (= (List t) (:: x as) (:: x here)))
;; where-old: (= (List t) (:: x as) (:: x as))

(define same-cons
  (lambda (t x as bs)
    (lambda (as=bs)
      (replace as=bs
        (lambda (xs) (= (List t) (:: x as) (:: x xs)))
        (same (:: x as)))
        )))

