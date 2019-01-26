{-# LANGUAGE FlexibleInstances #-}

module Pie.Nat (
  Nat, add1, zero
  , whichNat, iterNat, recNat
  , fromInt, toInt
) where

import Pie.Check
import Data.Functor.Foldable (cata, unfix, Fix(..), para)

-- | 
-- NatF is the parametric version of the Nat data type:
-- data Nat = Succ Nat | Zero
data NatF r =
    SuccF r
    | ZeroF

-- This allows us to define a fixpoint of this recursive data structure:
newtype Nat = Nat (Fix NatF)

instance Show Nat where
    show = show . toInt

instance Eq Nat where
    (==) x y = toInt x == toInt y

-- The fix point repeatedly provides the fix point as the type parameter.
-- newtype Fix f = Fix (f (Fix f))
-- example value for Nat: Fix (SuccF (Fix SuccF (Fix SuccF (Fix ZeroF))))

-- Next we define fmap to make NatF a functor.
-- This can be derived, but is implemented here for transparency.
instance Functor NatF where
    fmap f (SuccF r) = SuccF (f r)
    fmap _ ZeroF = ZeroF

add1 :: Nat -> Nat
add1 (Nat n) = Nat $ Fix $ SuccF n

zero :: Nat
zero = Nat $ Fix ZeroF

fromInt :: Int -> Nat
fromInt i = foldl (\n _ -> add1 n) zero [1..i]

whichNat :: Nat -> r -> (Nat -> r) -> r
whichNat (Nat (Fix ZeroF)) z _ = z
whichNat (Nat (Fix (SuccF n_1))) _ f = f (Nat n_1)

-- unfix is defined in Data.Functor.Foldable
-- unfix is used to go down one level.
-- unfix :: Fix f -> f (Fix f)
-- unfix (Fix f) = f
-- unfix is defined for out Nat as:
-- unfix :: Fix NatF -> NatF (Fix NatF)
-- Here is an example:
-- unfix (Fix SuccF (Fix SuccF (Fix ZeroF))) = SuccF (Fix SuccF (Fix ZeroF))
-- unfix (Fix SuccF (Fix ZeroF)) = SuccF (Fix ZeroF)
-- unfix (Fix ZeroF) = ZeroF

-- catamorphism's type from Data.Functor.Foldable:
-- cata :: (Base t a -> a) -> t -> a
-- Base is defined for a Fix data type:
-- type Base (Fix f) = f
-- This means for the Fix type, cata is defined as:
-- => cata :: (f a -> a) -> Fix f -> a
-- For our fix type Nat, cata is defined as:
-- => cata :: (NatF a -> a) -> Fix NatF -> a
-- => cata :: (NatF r -> r) -> Nat -> r
-- type FAlgebra f r = f r -> r
-- => cata :: FAlgebra NatF r -> Nat -> r

-- implementation derived from:  https://blog.sumtypeofway.com/recursion-schemes-part-2/
-- cata is a bottom up morphism over a functor.
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

-- iterNat implemented with a Fix, but without a recursion scheme
-- iterNat :: Nat -> r -> (r -> r) -> r
-- iterNat (Fix ZeroF) z _ = z
-- iterNat (Fix (SuccF n_1)) z f = f (iterNat n_1 z f)

-- | iterNat is a catamorphism.
iterNat :: Nat -> r -> (r -> r) -> r
iterNat (Nat n) base step = cata' (iterFAlgebra base step) n

-- iterFAlgebra simply returns base or applies step, 
-- based on whether the natural number is zero or a successor.
iterFAlgebra :: r -> (r -> r) -> NatF r -> r
iterFAlgebra base step ZeroF = base
iterFAlgebra base step (SuccF r) = step r

-- | toInt transforms a Nat into an Int for debugging/testing purposes.
toInt :: Nat -> Int
toInt n = iterNat n 0 (+1)

-- paramorphism's type from Data.Functor.Foldable
-- para :: (Base t (t, a) -> a) -> t -> a
-- type Base (Fix f) = f
-- => para :: (f (Fix f, a) -> a) -> Fix f -> a
-- => para :: (NatF (Nat, a) -> a) -> Nat -> a
-- => para :: (NatF (Nat, r) -> r) -> Nat -> r
-- type RAlgebra f r = f (Fix f, r) -> r
-- => para :: RAlgebra NatF r -> Nat -> r

-- implemenation derived from types.
para' :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para' algebra fixed =
    let -- go down one level
        unfixed = unfix fixed            -- :: f (Fix f)
        -- create curried algebra to apply to lower level
        para_algebra = para' algebra     -- :: Fix f -> a
        -- apply algebra to lower level
        -- fmap :: (a -> b) -> f a -> f b
        -- => fmap :: (Fix f -> a) -> f (Fix f) -> f a
        fa = fmap para_algebra unfixed   -- :: f a
        -- combine result with current level's value
        ffa = fmap (\a -> (fixed, a)) fa -- :: f (Fix f, a)
        -- apply algebra to current level and result
    in algebra ffa

-- recNat implemented with a Fix, but without a recursion scheme
-- recNat :: Nat -> r -> (Nat -> r -> r) -> r
-- recNat (Fix ZeroF) z _ = z
-- recNat (Fix (SuccF n_1)) z f = f n_1 (recNat n_1 z f)

-- | recNat is a paramorphism.
recNat :: Nat -> r -> (Nat -> r -> r) -> r
recNat (Nat n) base step = para' (recRAlgebra base step) n

-- recRAlgebra simply returns base or applies step, 
-- based on whether the natural number is zero or a successor.
recRAlgebra :: r -> (Nat -> r -> r) -> NatF (Fix NatF, r) -> r
recRAlgebra base step ZeroF = base
recRAlgebra base step (SuccF (n, r)) = step (Nat n) r
