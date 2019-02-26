#lang pie

;; Define a function called same-lists that states and proves that
;; if two values, e1 and e2, are equal and two lists, l1 and l2 are
;; equal then the two lists, (:: e1 l1) and (:: e2 l2) are also equal.

(claim same-lists
  (Pi ((t U) (e1 t) (e2 t) (l1 (List t)) (l2 (List t)))
    (-> (= (List t) l1 l2) (= t e1 e2)
        (= (List t) (:: e1 l1) (:: e2 l2)))))

;; replace with-new here-template where-old
;; with-new: (= old new)
;; here-template: (lambda (here) (... here ...))
;; where-old: (... old ... )
;; result: ( ... new ... )

(define same-lists
  (lambda (t e1 e2 l1 l2)
    (lambda (l1=l2 e1=e2)
      (replace l1=l2
        (lambda (l) (= (List t) (:: e1 l1) (:: e2 l)))
        (replace e1=e2
          (lambda (e) (= (List t) (:: e1 l1) (:: e l1)))
          (same (:: e1 l1)))))))

