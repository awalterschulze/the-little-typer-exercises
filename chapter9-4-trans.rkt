#lang pie

;;; Implement [trans](https://docs.racket-lang.org/pie/index.html#%28def._%28%28lib._pie%2Fmain..rkt%29._trans%29%29) using replace:
;;; (claim mytrans (Pi ((t U) (x t) (y t) (z t)) (-> (= t x y) (= t y z) (= t x z))))

(claim mytrans (Pi ((t U) (x t) (y t) (z t)) (-> (= t x y) (= t y z) (= t x z))))
(define mytrans
  (lambda (t x y z x=y y=z)
    (replace y=z
      (lambda (w) (= t x w))
      x=y)))
