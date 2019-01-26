-- | 
-- # Chapter 3 Exercises
module Chapter3 where

import Pie.Nat
import Pie.Check (checkSame)

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

-- checks2 :: IO ()
-- checks2 =
    -- checkSame "plusIter" (plusIter (fromInt 1) (fromInt 3)) (fromInt 4)
    -- checkSame "plusIter" (plusIter (fromInt 3) (fromInt 1)) (fromInt 4)
    -- checkSame "plusIter" (plusIter (fromInt 3) (fromInt 0)) (fromInt 3)
    -- checkSame "plusIter" (plusIter (fromInt 0) (fromInt 3)) (fromInt 3)
    -- checkSame "plusIter" (plusIter (fromInt 100) (fromInt 200)) (fromInt 300)
    -- checkSame "plusRec" (plusRec (fromInt 1) (fromInt 3)) (fromInt 4)
    -- checkSame "plusRec" (plusRec (fromInt 3) (fromInt 1)) (fromInt 4)
    -- checkSame "plusRec" (plusRec (fromInt 3) (fromInt 0)) (fromInt 3)
    -- checkSame "plusRec" (plusRec (fromInt 0) (fromInt 3)) (fromInt 3)
    -- checkSame "plusRec" (plusRec (fromInt 100) (fromInt 200)) (fromInt 300)
