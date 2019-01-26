module Chapter5 where

import Pie.Check (checkSame)
import Pie.List
import Pie.Nat

checks :: IO ()
checks = do
    checks1

-- ## 5.1
-- Define a function called sum-List that takes one List Nat argument and
-- evaluates to a Nat, the sum of the Nats in the list.

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

stepPlusRec :: Nat -> Nat -> Nat
stepPlusRec _ xy_1 = add1 xy_1

plusRec :: Nat -> Nat -> Nat
plusRec x y = recNat x y stepPlusRec

-- (claim step-sum
--   (-> Nat (List Nat) Nat Nat))
-- (define step-sum
--   (lambda (e es sum)
--     (+ e sum)))

-- (claim sum-List
--   (-> (List Nat) Nat))
-- (define sum-List
--   (lambda (es)
--     (rec-List es
--       0
--       step-sum)))

sumList :: List Nat -> Nat
sumList ls = recList ls
    zero
    stepSum

stepSum :: Nat -> List Nat -> Nat -> Nat
stepSum e _ total = plusRec e total

-- (check-same Nat (sum-List (:: 1 (:: 2 (:: 3 nil)))) 6)
-- (check-same Nat (sum-List nil) 0)

checks1 :: IO ()
checks1 = do
    checkSame "sum1" (sumList (
            (fromInt 1 <:> (fromInt 2 <:> (fromInt 3 <:> nil)))
        )) (fromInt 6)
    checkSame "sum2" (sumList nil) (fromInt 0)
