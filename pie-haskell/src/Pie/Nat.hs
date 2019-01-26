{-# LANGUAGE StandaloneDeriving #-}

module Pie.Nat (
  Nat, add1, zero
  , whichNat, iterNat, recNat
  , fromInt, toInt
) where

import Pie.Check
import Data.Functor.Foldable (cata, unfix, Fix(..), para)

data NatF r =
    SuccF r
    | ZeroF
    deriving (Eq, Show)

type Nat = Fix NatF

-- create fix point that repeatedly provides the fix point as the type parameter.
-- newtype Fix f = Fix (f (Fix f))
-- example value for Nat: Fix (SuccF (Fix SuccF (Fix SuccF (Fix ZeroF))))

instance Functor NatF where
    fmap f (SuccF r) = SuccF (f r)
    fmap _ ZeroF = ZeroF

-- unfix goes down one level
-- unfix :: Fix f -> f (Fix f)
-- unfix :: Fix NatF -> NatF (Fix NatF)
-- unfix (Fix SuccF (Fix SuccF (Fix ZeroF))) = SuccF (Fix SuccF (Fix ZeroF))
-- unfix (Fix SuccF (Fix ZeroF)) = SuccF SuccF (Fix ZeroF)
-- unfix (Fix ZeroF) = ZeroF

-- instance (Eq a) => Check (NatF a) where
--     checkSame :: String -> (NatF a) -> (NatF a) -> IO ()
--     checkSame s x y =
--         if x == y
--         then return ()
--         else putStrLn "what"

add1 :: Nat -> Nat
add1 n = Fix (SuccF n)

zero :: Nat
zero = Fix ZeroF

fromInt :: Int -> Nat
fromInt i = foldl (\n _ -> add1 n) zero [1..i]

toInt :: Nat -> Int
toInt (Fix ZeroF) = 0
toInt (Fix (SuccF n_1)) = 1 + toInt (n_1)

whichNat :: Nat -> r -> (Nat -> r) -> r
whichNat (Fix ZeroF) z _ = z
whichNat (Fix (SuccF n_1)) _ f = f n_1

-- cata :: (Base t a -> a) -> t -> a
-- type Base (Fix f) = f
-- => cata :: (f a -> a) -> Fix f -> a
-- => cata :: (NatF a -> a) -> Fix NatF -> a
-- => cata :: (NatF r -> r) -> Nat -> r
type Algebra f r = f r -> r
-- => cata :: Algebra NatF r -> Nat -> r

-- unfix :: Fix f -> f (Fix f)
-- unfix (Fix f) = f

-- https://blog.sumtypeofway.com/recursion-schemes-part-2/
cata' :: Functor f => (f a -> a) -> Fix f -> a
cata' algebra fixed = 
    let -- go down one level
        unfixed = unfix fixed -- :: f (Fix f)
        -- create curried algebra to apply to lower level
        cata_algebra = cata' algebra -- :: Fix f -> a
        -- apply algebra to lower level
        fa = fmap cata_algebra unfixed -- :: f a
        -- apply algebra to result
    in algebra fa

-- iterNat :: Nat -> r -> (r -> r) -> r
-- iterNat (Fix ZeroF) z _ = z
-- iterNat (Fix (SuccF n_1)) z f = f (iterNat n_1 z f)

iterNat :: Nat -> r -> (r -> r) -> r
iterNat n base step = cata (iterFAlgebra base step) n

iterFAlgebra :: r -> (r -> r) -> NatF r -> r
iterFAlgebra base step ZeroF = base
iterFAlgebra base step (SuccF r) = step r

-- para :: (Base t (t, a) -> a) -> t -> a
-- type Base (Fix f) = f
-- => para :: (f (Fix f, a) -> a) -> Fix f -> a
-- => para :: (NatF (Nat, a) -> a) -> Nat -> a
-- => para :: (NatF (Nat, r) -> r) -> Nat -> r
type RAlgebra f r = f (Fix f, r) -> r
-- => para :: RAlgebra NatF r -> Nat -> r

-- fmap :: (a -> b) -> f a -> f b
-- => fmap :: (Fix f -> a) -> f (Fix f) -> f a
para' :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para' algebra fixed =
    let -- go down one level
        unfixed = unfix fixed            -- :: f (Fix f)
        -- create curried algebra to apply to lower level
        para_algebra = para' algebra     -- :: Fix f -> a
        -- apply algebra to lower level
        fa = fmap para_algebra unfixed   -- :: f a
        -- combine result with current level's value
        ffa = fmap (\a -> (fixed, a)) fa -- :: f (Fix f, a)
        -- apply algebra to current level and result
    in algebra ffa

-- recNat :: Nat -> r -> (Nat -> r -> r) -> r
-- recNat (Fix ZeroF) z _ = z
-- recNat (Fix (SuccF n_1)) z f = f n_1 (recNat n_1 z f)

recNat :: Nat -> r -> (Nat -> r -> r) -> r
recNat n base step = para (recRAlgebra base step) n

recRAlgebra :: r -> (Fix NatF -> r -> r) -> NatF (Fix NatF, r) -> r
recRAlgebra base step ZeroF = base
recRAlgebra base step (SuccF (n, r)) = step n r
