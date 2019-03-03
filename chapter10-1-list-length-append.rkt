#lang pie

;; Define a function called list-length-append-dist that states and proves that
;; if you append two lists, l1 and l2, and then the length of the result is
;; equal to the sum of the lengths of l1 and l2.

;(claim list-length-append-dist
;       (Π ([E U]
;           [l1 (List E)]
;           [l2 (List E)])
;          (= Nat
;             (length E (append E l1 l2))
;             (+ (length E l1) (length E l2)))))

(claim +
  (-> Nat Nat Nat))

(define +
  (lambda (x y)
    (rec-Nat x
      y
      (lambda (_ y+x-1)
        (add1 y+x-1)))))

(claim length
  (Pi ((E U))
    (-> (List E) Nat)))

(define length
  (lambda (_)
    (lambda (list)
      (rec-List list
        0
        (lambda (_ _ n-1)
          (add1 n-1))))))

(claim append
  (Pi ((E U))
    (-> (List E) (List E) (List E))))

(define append
  (lambda (_)
    (lambda (l1 l2)
      (rec-List l1
        l2
        (lambda (e1 _ es1:l2)
          (:: e1 es1:l2))))))

;; end of preamble

(claim list-length-append-dist
       (Pi ((E U)
            (l1 (List E))
            (l2 (List E)))
          (= Nat
             (length E (append E l1 l2))
             (+ (length E l1) (length E l2)))))

(claim mot-list
  (Pi ((E U))
    (-> (List E) (List E) U)))

(define mot-list
  (lambda (t l2 l1)
    (= Nat
      (length t (append t l1 l2))
      (+ (length t l1) (length t l2)))))

(claim step-list
  (Pi ((E U)
       (l2 (List E))
       (e E)
       (es (List E)))
    (-> (= Nat
          (length E (append E es l2))
          (+ (length E es) (length E l2)))
        (= Nat
          (length E (append E (:: e es) l2))
          (+ (length E (:: e es)) (length E l2))))))

;; add1-length-normal: just a normal form

(claim add1-length-normal
  (Pi ((E U)
       (e E)
       (es (List E)))
   (= Nat
      (length E (:: e es))
      (add1 (length E es)))))

(define add1-length-normal
  (lambda (t e es)
    (same (length t (:: e es)))))

;; prepend-normal: just a normal form

(claim prepend-normal
  (Pi ((E U)
       (e E)
       (l1 (List E))
       (l2 (List E)))
    (= (List E)
      (:: e (append E l1 l2))
      (append E (:: e l1) l2))))

(define prepend-normal
  (lambda (t e l1 l2)
    (same (:: e (append t l1 l2)))))

;; left-hand-result-normal: just a normal form
;; because of normal forms add1-length and prepend

(claim left-hand-result-normal
  (Pi ((E U)
       (e E)
       (l1 (List E))
       (l2 (List E)))
    (= Nat
       (length E (append E (:: e l1) l2))
       (add1 (length E (append E l1 l2))))))

(define left-hand-result-normal
  (lambda (t e l1 l2)
    (same (add1 (length t (append t l1 l2))))))

;; right-hand-result-normal; just a normal form

(claim right-hand-result-normal
  (Pi ((E U)
       (e E)
       (l1 (List E))
       (l2 (List E)))
    (= Nat
       (+ (length E (:: e l1)) (length E l2))
       (add1 (+ (length E l1) (length E l2))))))

(define right-hand-result-normal
  (lambda (t e l1 l2)
    (same (add1 (+ (length t l1) (length t l2))))))

;; step-list:
;; because the left-hand-result normal form = right-hand-result normal form
;; and this is equal to just adding 1 to each side of the equal of the almost answer
;; the step function is very simple

(define step-list
  (lambda (t l2 e es)
    (lambda (almost)
      (cong almost (+ 1)))))

;; base-list

(claim base-list
  (Pi ((E U)
       (l2 (List E)))
    (= Nat
       (length E (append E nil l2))
       (+ (length E nil) (length E l2)))))

(define base-list
  (lambda (t l2)
    (same (length t l2))))

;; put it all together

(define list-length-append-dist
  (lambda (t l1 l2)
    (ind-List l1
      (mot-list t l2)
      (base-list t l2)
      (step-list t l2))))
