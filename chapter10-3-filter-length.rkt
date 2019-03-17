#lang pie

;; Define a function called length-filter-list that states and proves that
;; filtering a list (in the sense of filter-list from Exercise 5.3) evaluates
;; to a a list no longer than the original list.

;(claim length-filter-list
;  (Pi ((E U)
;       (l (List E))
;       (p (-> E Nat)))
;       (<= (length E (filter-list E p l))
;           (length E l))))

;; plus

(claim +
  (-> Nat Nat Nat))

(define +
  (lambda (x y)
    (rec-Nat x
      y
      (lambda (_ y+x-1)
        (add1 y+x-1)))))

;; plus-assoc: (= Nat (+ k (+ n m)) (+ (+ k n) m)))

(claim mot-plus-assoc
  (-> Nat Nat Nat U))

(define mot-plus-assoc
  (lambda (n m k)
    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(claim base-plus-assoc
  (Pi ((n Nat) (m Nat))
    (= Nat (+ 0 (+ n m)) (+ (+ 0 n) m))))

(define base-plus-assoc
  (lambda (n m)
    (same (+ n m))))

(claim step-plus-assoc
  (Pi ((n Nat) (m Nat) (k-1 Nat))
    (-> (mot-plus-assoc n m k-1)
        (mot-plus-assoc n m (add1 k-1)))))

(define step-plus-assoc
  (lambda (n m k-1)
    (lambda (mot-plus-assoc-k-1)
      (cong mot-plus-assoc-k-1 (+ 1)))))

(claim plus-assoc
  (Pi ((k Nat) (n Nat) (m Nat))
    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

(define plus-assoc
  (lambda (k n m)
    (ind-Nat k
      (mot-plus-assoc n m)
      (base-plus-assoc n m)
      (step-plus-assoc n m))))

;; <=

(claim <=
  (-> Nat Nat U))

(define <=
  (lambda (a b)
    (Sigma ((k Nat))
       (= Nat (+ k a) b))))

;; length

(claim length
  (Pi ((E U))
      (-> (List E) Nat)))

(define length
  (lambda (_)
    (lambda (l)
      (rec-List l
        0
        (lambda (x xs r)
          (add1 r))))))

;; if

(claim if
  (Pi ((R U)
       (cond Nat)
       (then R)
       (else R))
    R))

(define if
  (lambda (_ cond then else)
    (rec-Nat cond
      else
      (lambda (_ _) then))))

;; filter

(claim step-filter
  (Pi ((E U))
    (-> (-> E Nat) E (List E) (List E) (List E))))

(define step-filter
  (lambda (E)
    (lambda (pred h _ filtered)
      (if (List E) (pred h)
        (:: h filtered)
        filtered))))

(claim filter-list
  (Pi ((E U))
    (-> (-> E Nat) (List E) (List E))))

(define filter-list
  (lambda (E)
    (lambda (pred es)
      (rec-List es
        (the (List E) nil)
        (step-filter E pred)))))

;; <= trans

(claim sum-trans
  (Pi ((a Nat) (b Nat) (c Nat) (k Nat) (j Nat))
    (-> (= Nat (+ k a) b)
        (= Nat (+ j b) c)
        (= Nat (+ j (+ k a)) c))))

(define sum-trans
  (lambda (a b c k j)
    (lambda (k+a=b j+b=c)
      (replace (symm k+a=b)
        (lambda (here) (= Nat (+ j here) c))
        j+b=c))))

(claim plus-trans
  (Pi ((a Nat) (b Nat) (c Nat) (k Nat) (j Nat))
    (-> (= Nat (+ k a) b)
        (= Nat (+ j b) c)
        (= Nat (+ (+ j k) a) c))))

(define plus-trans
  (lambda (a b c k j)
    (lambda (k+a=b j+b=c)
      (replace (plus-assoc j k a)
        (lambda (here) (= Nat here c))
        (sum-trans a b c k j k+a=b j+b=c)))))

(claim <=-trans
  (Pi ((a Nat)
       (b Nat)
       (c Nat))
    (-> (<= a b)
        (<= b c)
        (<= a c))))

(define <=-trans
  (lambda (a b c)
    (lambda (a<=b b<=c)
      (cons
        (+ (car b<=c) (car a<=b))
        (plus-trans a b c (car a<=b) (car b<=c) (cdr a<=b) (cdr b<=c))
          ))))

;; end of preamble

(claim length-filter-list
  (Pi ((E U)
       (l (List E))
       (p (-> E Nat)))
       (<= (length E (filter-list E p l))
           (length E l))))

;; there exists a `k` where (+ k (length E (filter-list E p l))) == (length E l)

(claim mot-filter-list
  (Pi ((E U)
       (p (-> E Nat))
       (l (List E)))
       U))

(define mot-filter-list
  (lambda (e p l)
    (<= (length e (filter-list e p l))
           (length e l))))


;; there exists a `k` where (+ k (length E (filter-list E p nil))) == (length E nil)
;; there exists a `k` where (+ k (length E (filter-list E p nil))) == zero
;; there exists a `k` where (+ k (length E nil)) == zero
;; there exists a `k` where (+ k zero) == zero
;; there exists a `0` where (+ 0 zero) == zero
;; (cons 0 (same 0))

(claim base-filter-list
  (Pi ((E U)
       (p (-> E Nat)))
    (<= (length E (filter-list E p nil))
           (length E nil))))

(define base-filter-list
  (lambda (e p)
    (cons zero (same zero))))

;; (= Nat (length E (:: e es)) (add1 (length E es)))

(claim length+1
  (Pi ((E U)
       (e E)
       (es (List E)))
    (= Nat (length E (:: e es)) (add1 (length E es)))))

(define length+1
  (lambda (E e es)
    (cong (the (= Nat (length E es) (length E es)) (same (length E es))) (+ 1))))

;; (= Nat (add1 (+ x y)) (+ x (add1 y)))

(claim plus1
  (Pi ((x Nat)
       (y Nat))
    (= Nat (+ (add1 x) y) (+ x (add1 y)))))

(define plus1
  (lambda (x y)
    (ind-Nat x
      (lambda (x) (= Nat (+ (add1 x) y) (+ x (add1 y))))
      (same (add1 y))
      (lambda (x)
        (lambda (x-1+1+y==x-1+y+1)
          (cong x-1+1+y==x-1+y+1 (+ 1)))))))

;; given
;; there exists a `k` where (+ k x) == y
;; we need to prove that
;; there exists a `k` where (+ k (add1 x)) == (add1 y)

;; take the cdr of x<=y
;; (+ (car x<=y) x) == y
;; cong add1
;; (add1 (+ (car x<=y) x) == (add1 y)
;; take left hand side
;; (add1 (+ (car x<=y) x)
;; we can use plus1
;; (= Nat (add1 (+ x y)) (+ x (add1 y)))
;; (+ (car x<=y) (add1 x))

(claim congadd1
  (Pi ((x Nat)
       (y Nat))
    (->
      (<= x y)
      (<= (add1 x) (add1 y)))))

(define congadd1
  (lambda (x y)
    (lambda (x<=y)
      (cons
        (car x<=y)
        (replace (plus1 (car x<=y) x)
          (lambda (here) (= Nat here (add1 y)))
          (cong (cdr x<=y) (+ 1)))))))

;; filter-list looks at the head before processing the rest of the list
;; (= Nat
;;   (length E (filter-list E p (:: e es)))
;;   (length E (if (List E) (p e)
;;                (:: e (filter-list E p es))
;;                (filter-list E p es)))

(claim normal-step-filter
  (Pi ((E U)
       (p (-> E Nat))
       (e E)
       (es (List E)))
  (= Nat
    (length E (filter-list E p (:: e es)))
    (length E (if (List E) (p e)
                (:: e (filter-list E p es))
                (filter-list E p es))))))

(define normal-step-filter
  (lambda (E p e es)
    (same (length E (filter-list E p (:: e es))))))

;; filter head is smaller
;; if we possibly only filter one element then the length of the filtered list is smaller than the original list.
;; we didn't use this, but it is useful to see that it is possible to do induction on the predicate

(claim filter-head-is-smaller
  (Pi ((E U)
       (es (List E))
       (e E)
       (bool Nat))
    (<=
       (length E
         (if (List E) bool
           (:: e es)
           es))
       (length E (:: e es)))))

(claim mot-filter-head-is-smaller
  (Pi ((E U)
       (es (List E))
       (e E)
       (bool Nat))
    U))

(define mot-filter-head-is-smaller
  (lambda (E es e bool)
    (<=
       (length E
         (if (List E) bool
           (:: e es)
           es))
       (length E (:: e es)))))

(claim step-filter-head-is-smaller
  (Pi ((E U)
       (es (List E))
       (e E)
       (bool-1 Nat))
    (->
      (<=
        (length E
         (if (List E) bool-1
           (:: e es)
           es))
       (length E (:: e es)))
      (<=
        (length E
         (if (List E) (add1 bool-1)
           (:: e es)
           es))
       (length E (:: e es))))))

(define step-filter-head-is-smaller
  (lambda (E es e bool-1)
    (lambda (_)
      (cons 0 (same (length E (:: e es)))))))

(define filter-head-is-smaller
  (lambda (E es e bool)
    (ind-Nat bool
      (mot-filter-head-is-smaller E es e)
      (cons 1 (same (length E (:: e es))))
      (step-filter-head-is-smaller E es e))))

;; step filter
;; we have
;; (<= (length E (filter-list E p es))
;;     (length E es))
;; using congadd1, we can get to
;; (<= (add1 (length E (filter-list E p es)))
;;     (add1 (length E es)))
;; using length+1, we can get to
;; (<= (length E (:: e (filter-list E p es)))
;;     (length E (:: e es)))

;; now we need show that
;; (<=
;;   (length E (filter-list E p (:: e es)))
;;   (length E (:: e (filter-list E p es)))

;; one step of execution for filter-list results in normal-step-filter
;; (= Nat
;;   (length E (filter-list E p (:: e es)))
;;   (length E (if (List E) (p e)
;;                (:: e (filter-list E p es))
;;                (filter-list E p es)))

;; we can replace (p e) with a Nat cond and do induction on that Nat.
;; (= Nat
;;   (length E (filter-list E p (:: e es)))
;;   (length E (if (List E) cond
;;                (:: e (filter-list E p es))
;;                (filter-list E p es)))

(claim length-e
  (Pi ((E U)
       (p (-> E Nat))
       (e E)
       (es (List E)))
    (<=
      (length E (filter-list E p (:: e es)))
      (length E (:: e (filter-list E p es))))))

(claim mot-e
  (Pi ((E U)
       (e E)
       (es (List E))
       (p (-> E Nat))
       (cond Nat))
    U))

(define mot-e
  (lambda (E e es p cond)
    (<=
      (length E
        (if (List E) cond
          (:: e (filter-list E p es))
          (filter-list E p es)))
      (length E (:: e (filter-list E p es))))))

(claim step-e
  (Pi ((E U)
       (e E)
       (es (List E))
       (p (-> E Nat))
       (cond-1 Nat))
    (->
      (<=
        (length E
          (if (List E) cond-1
            (:: e (filter-list E p es))
            (filter-list E p es)))
        (length E (:: e (filter-list E p es))))
      (<=
        (length E
          (if (List E) (add1 cond-1)
            (:: e (filter-list E p es))
            (filter-list E p es)))
        (length E (:: e (filter-list E p es)))))))

(define step-e
  (lambda (E e es p cond-1)
    (lambda (false-proof)
      (cons 0 (same (length E (:: e (filter-list E p es))))))))

(define length-e
  (lambda (E p e es)
    (ind-Nat (p e)
      (mot-e E e es p)
      (cons 1 (same (length E (:: e (filter-list E p es)))))
      (step-e E e es p))))

;; step filter
;; we have
;; (<= (length E (filter-list E p es))
;;     (length E es))
;; using congadd1, we can get to
;; (<= (add1 (length E (filter-list E p es)))
;;     (add1 (length E es)))
;; using length+1, we can get to
;; (<= (length E (:: e (filter-list E p es)))
;;     (length E (:: e es)))
;; and we have length-e
;; (<=
;;   (length E (filter-list E p (:: e es)))
;;   (length E (:: e (filter-list E p es)))
;; so we can using <=-trans to get
;; (<=
;;   (length E (filter-list E p (:: e es)))
;;   (length E (:: e es)))

(claim step-filter-list
  (Pi ((E U)
       (p (-> E Nat))
       (e E)
       (es (List E)))
    (->
      (<= (length E (filter-list E p es))
          (length E es))
      (<= (length E (filter-list E p (:: e es)))
          (length E (:: e es))))))

(define step-filter-list
  (lambda (E p e es)
    (lambda (filtered-es<=es)
       (<=-trans
          (length E (filter-list E p (:: e es))) ;; a
          (length E (:: e (filter-list E p es))) ;; b
          (length E (:: e es)) ;; c
          (length-e E p e es)
          (replace (symm (length+1 E e (filter-list E p es))) ;; (= Nat (length E (:: e es)) (add1 (length E es)))))
            (lambda (here) (Sigma ((k Nat)) ;; (= Nat (+ k a) b))
                             (= Nat
                                (+ k here)
                                (length E (:: e es))
                             )))
        ;; (<= (length E (:: e (filter-list E p es)))
        ;;     (length E (:: e es)))
            (replace (symm (length+1 E e es)) ;; (= Nat (length E (:: e es)) (add1 (length E es)))))
              (lambda (here) (Sigma ((k Nat)) ;; (= Nat (+ k a) b))
                               (= Nat
                                  (+ k (add1 (length E (filter-list E p es))))
                                  here
                               )))
        ;; (<= (add1 (length E (filter-list E p es)))
        ;;     (length E (:: e es)))
              (congadd1 (length E (filter-list E p es)) (length E es)
        ;; (<= (add1 (length E (filter-list E p es)))
        ;;     (add1 (length E es)))
                filtered-es<=es))))
        ;; (<= (length E (filter-list E p es)) (length E es))
        )))

(define length-filter-list
  (lambda (E l p)
    (ind-List l
      (mot-filter-list E p)
      (base-filter-list E p)
      (step-filter-list E p))))
      
