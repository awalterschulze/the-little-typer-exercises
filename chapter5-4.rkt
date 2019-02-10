#lang pie

;; Define a function called sort-List-Nat which takes one (List Nat) argument
;; and evaluates to a (List Nat) consisting of the elements from the list
;; argument sorted in ascending order.

;; return 1 for less than and 0 for greater than
(claim < (-> Nat Nat Nat))
(define <
  (lambda (x)
    (rec-Nat x
      (the (-> Nat Nat) (lambda (_) 1))
      (lambda (x-1 res)
        (lambda (y)
          (rec-Nat y
            0
            (lambda (y-1 _)
              (res y-1)
            )
          )
        )
      )
    )
  )
)

(check-same Nat (< 2 4) 1)
(check-same Nat (< 4 2) 0)
(check-same Nat (< 1 0) 0)
(check-same Nat (< 0 1) 1)

(claim insert (-> (List Nat) Nat (List Nat)))
(define insert
  (lambda (list)
    (rec-List list
    (the (-> Nat (List Nat)) (lambda (i)
      (:: i nil)
      ))
    (lambda (e es insertInto)
      (lambda (i)
        (which-Nat (< i e)
          (:: e (insertInto i))
          (lambda (_) (:: i (insertInto e)))
          )
        )
      )
    )))

(check-same (List Nat)
  (insert nil 2) (:: 2 nil)
)
(check-same (List Nat)
  (insert (:: 1 (:: 3 (:: 4 nil))) 0) (:: 0 (:: 1 (:: 3 (:: 4 nil))))
)
(check-same (List Nat)
  (insert (:: 1 (:: 3 (:: 4 nil))) 5) (:: 1 (:: 3 (:: 4 (:: 5 nil))))
)
(check-same (List Nat)
  (insert (:: 1 (:: 3 (:: 4 nil))) 2) (:: 1 (:: 2 (:: 3 (:: 4 nil))))
)
(check-same (List Nat)
  (insert (:: 1 (:: 3 (:: 4 nil))) 3) (:: 1 (:: 3 (:: 3 (:: 4 nil))))
)

(claim sort (-> (List Nat) (List Nat)))
(define sort
  (lambda (ls)
  (rec-List ls
    (the (List Nat) nil)
    (lambda (e es res)
      (insert res e)
      )
    ))
)

(check-same (List Nat)
  (sort (:: 2 nil)) (:: 2 nil)
)
(check-same (List Nat)
  (sort (:: 0 (:: 1 (:: 3 (:: 4 nil))))) (:: 0 (:: 1 (:: 3 (:: 4 nil))))
)
(check-same (List Nat)
  (sort (:: 5 (:: 1 (:: 3 (:: 4 nil))))) (:: 1 (:: 3 (:: 4 (:: 5 nil))))
)
(check-same (List Nat)
  (sort (:: 2 (:: 1 (:: 3 (:: 4 nil))))) (:: 1 (:: 2 (:: 3 (:: 4 nil))))
)
(check-same (List Nat)
  (sort (:: 3 (:: 1 (:: 3 (:: 4 nil))))) (:: 1 (:: 3 (:: 3 (:: 4 nil))))
)
