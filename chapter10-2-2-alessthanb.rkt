#lang pie

;; In the following exercises we'll use the function called <= that takes two
;; Nat arguments a, b and evaluates to a type representing the proposition
;; that a is less than or equal to b.

;(claim <=
;       (-> Nat Nat
;           U))
;
;(define <=
;  (λ (a b)
;    (Σ ([k Nat])
;       (= Nat (+ k a) b))))

;; Define a funciton called <=-simplify to state and prove that for all
;; Nats a, b, n we have that n+a <= b implies a <= b
;;
;; NB: You may need to use plus-assoc that was proved in Exercise 8.3.
;;
;; (claim plus-assoc
;;  (Pi ((n Nat) (m Nat) (k Nat))
;;    (= Nat (+ k (+ n m)) (+ (+ k n) m))))

;(claim <=-simplify
;       (Π ([a Nat]
;           [b Nat]
;           [n Nat])
;          (-> (<= (+ n a) b)
;              (<= a b))))

(claim +
  (-> Nat Nat Nat))

(define +
  (lambda (x y)
    (rec-Nat x
      y
      (lambda (_ y+x-1)
        (add1 y+x-1)))))

;; End of preamble

(claim <=
       (-> Nat Nat
           U))

(define <=
  (lambda (a b)
    (Sigma ((k Nat))
       (= Nat (+ k a) b))))

;; a+b+1 == a+1+b

(claim a+b+1==a+1+b
  (Pi ((a Nat) (b Nat))
    (= Nat
       (+ a (add1 b))
       (+ (add1 a) b))))

(claim mot-a+b+1==a+1+b
  (Pi ((a Nat) (b Nat))
    U))

(define mot-a+b+1==a+1+b
  (lambda (b a)
    (= Nat
      (+ a (add1 b))
       (+ (add1 a) b))))

(claim base-a+b+1==a+1+b
  (Pi ((b Nat))
    (= Nat
       (+ zero (add1 b))
       (+ (add1 zero) b))))

(define base-a+b+1==a+1+b
  (lambda (b)
    (same (add1 b))))

(claim step-a+b+1==a+1+b
  (Pi ((b Nat) (a-1 Nat))
    (-> (= Nat
          (+ a-1 (add1 b))
          (+ (add1 a-1) b))
        (= Nat
          (+ (add1 a-1) (add1 b))
          (+ (add1 (add1 a-1)) b)))))

(define step-a+b+1==a+1+b
  (lambda (b a)
    (lambda (a-1+b+1==a-1+1+b)
      (cong a-1+b+1==a-1+1+b (+ 1)))))

(define a+b+1==a+1+b
  (lambda (a b)
    (ind-Nat a
      (mot-a+b+1==a+1+b b)
      (base-a+b+1==a+1+b b)
      (step-a+b+1==a+1+b b))))
      

;; a+1<=b implies a<=b

(claim add1-smaller
  (Pi ((a Nat)
       (b Nat))
    (-> (<= (add1 a) b)
        (<= a b))))

;; we have
;; there exists k where k + (a + 1) == b
;; which is equivalent to
;; there exists (car a+1<=b) where (car a+1<=b) + (a + 1) == b
;; needs to be transform to
;; there exists (car a+1<=b) + 1 where ((car a+1<=b) + 1) + a == b
;; which is equal to
;; there exists k + 1 where (k + 1) + a == b
;; which is equal to
;; there exists k where k + a == b
;; which is our goal for add1-smaller

;; we also have a+b+1==a+1+b
;; which we can use to turn
;; (car a+1<=b) + (a + 1)
;; into
;; ((car a+1<=b) + 1) + a
;; (replace a+b+1==a+1+b
;;    (lambda (here) (= Nat here b))
;;    (= Nat (+ (car a+1<=b) (+ a 1)) b)

(define add1-smaller
  (lambda (a b)
    (lambda (a+1<=b)
      (cons
        (add1 (car a+1<=b))
        (replace (a+b+1==a+1+b (car a+1<=b) a)
          (lambda (here) (= Nat here b))
          (cdr a+1<=b))))))

;; a+n<=b implies a<=b

(claim <=-simplify
       (Pi ((a Nat)
            (b Nat)
            (n Nat))
          (-> (<= (+ n a) b)
              (<= a b))))

(claim mot-simplify
  (Pi ((a Nat)
       (b Nat)
       (n Nat))
       U))

(define mot-simplify
  (lambda (a b n)
    (-> (<= (+ n a) b)
        (<= a b))))

(claim step-simplify
  (Pi ((a Nat)
       (b Nat)
       (n-1 Nat))
    (->
        (-> (<= (+ n-1 a) b)
          (<= a b))
        (-> (<= (+ (add1 n-1) a) b)
          (<= a b)))))

;; we have a function that expects
;; (<= (+ n-1 a) b)
;; and returns our goal
;; (<= a b)
;; we also have another input into our function
;; (<= (+ (add1 n-1) a) b)
;; which is equal to
;; (<= (add1 (+ n-1 a)) b)
;; we also have add1-smaller
;; (-> (<= (add1 a) b) (<= a b)))
;; which we can use to get
;; (<= (+ n-1 a) b)
;; which we can then pass to our first function to get our goal

(define step-simplify
  (lambda (a b n-1)
    (lambda (n-1+a<=b-implies-a<=b)
      (lambda (n-1+1+a<=b)
        (n-1+a<=b-implies-a<=b
          (add1-smaller (+ n-1 a) b n-1+1+a<=b))))))

(define <=-simplify
  (lambda (a b n)
    (ind-Nat n
      (mot-simplify a b)
      (lambda (zero+a<=b) zero+a<=b)
      (step-simplify a b))))
