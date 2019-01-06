#lang pie

;;; Rewrite the definition of + (in frame 3.27) using the rec-Nat eliminator instead of the iter-Nat eliminator.
;;;
;;; (define step-+
;;;   (lambda (sum-n-1)
;;;     (add1 sum-n-1)))
;;;
;;; (define +
;;;   (lambda (n j)
;;;     (iter-Nat n
;;;       j
;;;       step-+)))

(claim + (-> Nat Nat Nat))
(define +
  (lambda (i j)
    (rec-Nat j
      i
      (lambda (j-1 sum-j-1)
        (add1 sum-j-1)
      )
    )
  )
)

(check-same Nat (+ 1 3) 4)
(check-same Nat (+ 3 1) 4)
(check-same Nat (+ 3 0) 3)
(check-same Nat (+ 0 3) 3)
(check-same Nat (+ 100 200) 300)
