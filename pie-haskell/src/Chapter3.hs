-- | 
-- # Chapter 3 Exercises
module Chapter3 (checks) where

import Pie.Nat
import Pie.Check (checkSame)

checks :: IO ()
checks = do
    checks1
    checks2
    checks3a
    checks3b

-- ## 3.1
-- Define a function called at-least-two? that takes one Nat argument and evaluates to an Atom.
-- at-least-two? evaluates to 't if the Nat is greater than or equal to 2 otherwise it evaluates to 'nil.
-- Note:
-- The only Nat eliminator you should need in the body of at-least-two? is which-Nat.

-- (claim at-least-two (-> Nat Atom))
-- (define at-least-two
--   (lambda (n)
--     (which-Nat n
--       'nil
--       (lambda (n-1)
--         (which-Nat n-1
--           'nil
--           (lambda (n-2) 't)
--         )
--       )
--     )
--   )
-- )

atLeastTwo :: Nat -> Bool
atLeastTwo n =
    whichNat n False (\n_1 ->
        whichNat n_1 False (\n_2 ->
            True    
        )    
    )

-- (check-same Atom (at-least-two 0) 'nil)
-- (check-same Atom (at-least-two 1) 'nil)
-- (check-same Atom (at-least-two 2) 't)
-- (check-same Atom (at-least-two 3) 't)
-- (check-same Atom (at-least-two 100) 't)

checks1 :: IO ()
checks1 = do
    checkSame "atLeastTwo" (atLeastTwo (fromInt 0)) False
    checkSame "atLeastTwo" (atLeastTwo (fromInt 1)) False
    checkSame "atLeastTwo" (atLeastTwo (fromInt 2)) True
    checkSame "atLeastTwo" (atLeastTwo (fromInt 3)) True
    checkSame "atLeastTwo" (atLeastTwo (fromInt 100)) True

-- ## 3.2
-- Rewrite the definition of + (in frame 3.27) using the rec-Nat eliminator instead of the iter-Nat eliminator.
--
-- (define step-+
--   (lambda (sum-n-1)
--     (add1 sum-n-1)))
--
-- (define +
--   (lambda (n j)
--     (iter-Nat n
--       j
--       step-+)))

-- (claim + (-> Nat Nat Nat))
-- (define +
--   (lambda (i j)
--     (rec-Nat j
--       i
--       (lambda (j-1 sum-j-1)
--         (add1 sum-j-1)
--       )
--     )
--   )
-- )

stepPlusIter :: Nat -> Nat
stepPlusIter xy_1 = add1 xy_1

plusIter :: Nat -> Nat -> Nat
plusIter x y =
    iterNat x y stepPlusIter

stepPlusRec :: Nat -> Nat -> Nat
stepPlusRec _ xy_1 = add1 xy_1

plusRec :: Nat -> Nat -> Nat
plusRec x y =
    recNat x y stepPlusRec

-- (check-same Nat (+ 1 3) 4)
-- (check-same Nat (+ 3 1) 4)
-- (check-same Nat (+ 3 0) 3)
-- (check-same Nat (+ 0 3) 3)
-- (check-same Nat (+ 100 200) 300)

checks2 :: IO ()
checks2 = do
    checkSame "plusIter1" (plusIter (fromInt 1) (fromInt 3)) (fromInt 4)
    checkSame "plusIter2" (plusIter (fromInt 3) (fromInt 1)) (fromInt 4)
    checkSame "plusIter3" (plusIter (fromInt 3) (fromInt 0)) (fromInt 3)
    checkSame "plusIter4" (plusIter (fromInt 0) (fromInt 3)) (fromInt 3)
    checkSame "plusIter5" (plusIter (fromInt 100) (fromInt 200)) (fromInt 300)
    checkSame "plusRec1" (plusRec (fromInt 1) (fromInt 3)) (fromInt 4)
    checkSame "plusRec2" (plusRec (fromInt 3) (fromInt 1)) (fromInt 4)
    checkSame "plusRec3" (plusRec (fromInt 3) (fromInt 0)) (fromInt 3)
    checkSame "plusRec4" (plusRec (fromInt 0) (fromInt 3)) (fromInt 3)
    checkSame "plusRec5" (plusRec (fromInt 100) (fromInt 200)) (fromInt 300)

-- ## 3.3
-- Define a function called exp that takes two Nat arguments and evaluates to a Nat.
-- exp evaluates to the exponentiation, a^b, of the two passed arguments.

-- (claim step-* (-> Nat Nat Nat Nat))
-- (define step-*
--   (lambda (j j-1 mul-1)
--     (+ j mul-1)
--     ))

-- (claim * (-> Nat Nat Nat))
-- (define *
--   (lambda (i j)
--     (rec-Nat i
--       0
--       (step-* j)
--     )
--   )
-- )

plus :: Nat -> Nat -> Nat
plus = plusRec

stepTimes :: Nat -> Nat -> Nat -> Nat
stepTimes y x_1 res = (plus y res)

times :: Nat -> Nat -> Nat
times x y = recNat x zero (stepTimes y)

-- (check-same Nat (* 3 4) 12)
-- (check-same Nat (* 0 4) 0)
-- (check-same Nat (* 4 0) 0)
-- (check-same Nat (* 4 100) 400)

checks3a :: IO ()
checks3a = do
    checkSame "multiply1" (times (fromInt 3) (fromInt 4)) (fromInt 12)
    checkSame "multiply2" (times (fromInt 0) (fromInt 4)) (fromInt 0)
    checkSame "multiply3" (times (fromInt 4) (fromInt 0)) (fromInt 0)
    checkSame "multiply4" (times (fromInt 4) (fromInt 100)) (fromInt 400)

-- (claim step-^ (-> Nat Nat Nat Nat))
-- (define step-^
--   (lambda (j j-1 pow-1)
--     (* j pow-1)
--   ))

-- (claim ^ (-> Nat Nat Nat))
-- (define ^
--   (lambda (i j)
--     (rec-Nat j
--       1
--       (step-^ i)
--       )))

power :: Nat -> Nat -> Nat
power x y =
    recNat y (add1 zero) (stepPower x)

stepPower :: Nat -> Nat -> Nat -> Nat
stepPower x y_1 res = times x res

-- (check-same Nat (^ 0 2) 0)
-- (check-same Nat (^ 2 0) 1)
-- (check-same Nat (^ 2 2) 4)
-- (check-same Nat (^ 2 3) 8)
-- (check-same Nat (^ 10 3) 1000)

checks3b :: IO ()
checks3b = do
    checkSame "power1" (power (fromInt 0) (fromInt 2)) (fromInt 0)
    checkSame "power2" (power (fromInt 2) (fromInt 0)) (fromInt 1)
    checkSame "power3" (power (fromInt 2) (fromInt 2)) (fromInt 4)
    checkSame "power4" (power (fromInt 2) (fromInt 3)) (fromInt 8)
    checkSame "power5" (power (fromInt 10) (fromInt 3)) (fromInt 1000)