#lang pie

;;; Define a function called at-least-two? that takes one Nat argument and evaluates to an Atom.
;;; at-least-two? evaluates to 't if the Nat is greater than or equal to 2 otherwise it evaluates to 'nil.
;;; Note:
;;; The only Nat eliminator you should need in the body of at-least-two? is which-Nat.

(claim at-least-two (-> Nat Atom))
(define at-least-two
  (lambda (n)
    (which-Nat n
      'nil
      (lambda (n-1)
        (which-Nat n-1
          'nil
          (lambda (n-2) 't)
        )
      )
    )
  )
)

(check-same Atom (at-least-two 0) 'nil)
(check-same Atom (at-least-two 1) 'nil)
(check-same Atom (at-least-two 2) 't)
(check-same Atom (at-least-two 3) 't)
(check-same Atom (at-least-two 100) 't)
