-- | 
-- # Chapter 4 Exercises
module Chapter4 (checks) where

import Pie.Nat
import Pie.Check (checkSame)

checks :: IO ()
checks = checks1

-- # 4.1
-- Define a function called max that takes two Nat arguments and evaluates to a Nat.
-- max evaluates to the larger of the two passed arguments.

-- A solution adopted from Ayman Osman:
-- https://gist.github.com/aymanosman/9db1db11755ef4b4759c4ee9a4bdb515
-- Here we deconstruct both x and y at the "same" time.

-- (claim max (-> Nat Nat Nat))
-- (define max
--   (lambda (x)
--     (rec-Nat x
--       (the (-> Nat Nat) (lambda (y) y))
--       (lambda (x-1 max-x-1)
--         (lambda (y) 
--           (add1 (rec-Nat y
--             x-1
--             (lambda (y-1 _)
--               (max-x-1 y-1)
--             ))))))))

maxNat :: Nat -> Nat -> Nat
maxNat x = recNat x (\y -> y) stepMaxX

stepMaxX :: Nat -> (Nat -> Nat) -> (Nat -> Nat)
stepMaxX x_1 max_x_1_or_ y = recNat y (add1 x_1) (stepMaxY max_x_1_or_)

stepMaxY :: (Nat -> Nat) -> Nat -> Nat -> Nat
stepMaxY max_x_1_or_ y_1 _ = add1 (max_x_1_or_ y_1)

-- (check-same Nat (max 2 4) 4)
-- (check-same Nat (max 4 2) 4)
-- (check-same Nat (max 0 1) 1)
-- (check-same Nat (max 1 0) 1)
-- (check-same Nat (max 19 20) 20)
-- (check-same Nat (max 19 19) 19)

checks1 :: IO ()
checks1 = do
    checkSame "max1" (maxNat (fromInt 2) (fromInt 4)) (fromInt 4)
    checkSame "max2" (maxNat (fromInt 4) (fromInt 2)) (fromInt 4)
    checkSame "max3" (maxNat (fromInt 0) (fromInt 1)) (fromInt 1)
    checkSame "max4" (maxNat (fromInt 1) (fromInt 0)) (fromInt 1)
    checkSame "max5" (maxNat (fromInt 19) (fromInt 20)) (fromInt 20)
    checkSame "max6" (maxNat (fromInt 20) (fromInt 19)) (fromInt 20)