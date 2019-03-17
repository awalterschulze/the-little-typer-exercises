#lang pie

;; Use ind-Vec to define a function called unzip that takes unzips
;; a vector of pairs into a pair of vectors.

; (claim unzip
;       (Π ([A U]
;           [B U]
;           [n Nat])
;          (-> (Vec (Pair A B) n)
;              (Pair (Vec A n) (Vec B n)))))

(claim unzip
  (Pi ((A U)
       (B U)
       (n Nat))
    (-> (Vec (Pair A B) n)
        (Pair (Vec A n) (Vec B n)))))

(claim mot-unzip
  (Pi ((A U)
       (B U)
       (k Nat))
    (-> (Vec (Pair A B) k)
        U)))

(define mot-unzip
  (lambda (A B k v)
    (Pair (Vec A k) (Vec B k))))
    
(claim step-unzip
  (Pi ((A U)
       (B U)
       (k Nat)
       (h (Pair A B))
       (t (Vec (Pair A B) k)))
    (->
      (Pair (Vec A k) (Vec B k))
      (Pair (Vec A (add1 k)) (Vec B (add1 k))))))

(define step-unzip
  (lambda (A B k h t)
    (lambda (pair-va-vb)
      (cons (vec:: (car h) (car pair-va-vb)) (vec:: (cdr h) (cdr pair-va-vb))))))
      
(define unzip
  (lambda (A B n)
    (lambda (v)
      (ind-Vec n v
        (mot-unzip A B)
        (the (Pair (Vec A 0) (Vec B 0)) (cons vecnil vecnil))
        (step-unzip A B)))))
      
        
