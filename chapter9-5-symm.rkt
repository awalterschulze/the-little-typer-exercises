#lang pie

;;; Implement symm using replace:
;;; (claim mysymm (Pi ((t U) (x t) (y t)) (-> (= t x y) (= t y x))))

(claim mysymm (Pi ((t U) (x t) (y t)) (-> (= t x y) (= t y x))))
(define mysymm
  (lambda (t x y eq)
    (replace eq
      (lambda (z) (= t z x))
      (same x) )))