{-# LANGUAGE FlexibleInstances #-}

module Pie.Nat (
  Nat, add1, zero
  , whichNat, iterNat, recNat
  , fromInt, toInt
  , tests
) where

import Data.Functor.Foldable (cata, unfix, Fix(..), para)
import qualified Test.Tasty                    as T
import qualified Test.Tasty.HUnit              as T
import qualified Data.Map.Strict               as M
import Debug.Trace (trace)

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

-- implementation derived from:  
-- https://blog.sumtypeofway.com/recursion-schemes-part-2/
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
iterNat' :: Nat -> r -> (r -> r) -> r
iterNat' (Nat (Fix ZeroF)) z _ = z
iterNat' (Nat (Fix (SuccF n_1))) z f = f (iterNat' (Nat n_1) z f)

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

-- implemenation derived from types (with a bug) and fixed by looking at
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/
para' :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para' algebra fixed =
    let -- go down one level
        unfixed = unfix fixed            -- :: f (Fix f)
        -- create curried algebra to apply to lower level
        para_algebra = para' algebra     -- :: Fix f -> a
        both t = (t, para_algebra t)     -- :: Fix f -> (Fix f, a)
        -- apply algebra to lower level
        -- fmap :: (a -> b) -> f a -> f b
        -- => fmap :: (Fix f -> (Fix f, a)) -> f (Fix f) -> f (Fix f, a)
        ffa = fmap both unfixed   -- :: f (Fix f, a)
        -- apply algebra to current level and result
    in algebra ffa

-- recNat' implemented with a Fix, but without a recursion scheme
recNat' :: Nat -> r -> (Nat -> r -> r) -> r
recNat' (Nat (Fix ZeroF)) z _ = z
recNat' (Nat (Fix (SuccF n_1))) z f = f (Nat n_1) (recNat' (Nat n_1) z f)

-- | recNat is a paramorphism.
recNat :: Nat -> r -> (Nat -> r -> r) -> r
recNat (Nat n) base step = para' (recRAlgebra base step) n

-- recRAlgebra simply returns base or applies step, 
-- based on whether the natural number is zero or a successor.
recRAlgebra :: r -> (Nat -> r -> r) -> NatF (Fix NatF, r) -> r
recRAlgebra base step ZeroF = base
recRAlgebra base step (SuccF (n, r)) = step (Nat n) r

-- anamorphism is the opposite of catamorphism.
-- anamorphism's type from Data.Functor.Foldable:
-- ana :: (a -> Base t a) -> a -> t
-- Base is defined for a Fix data type:
-- type Base (Fix f) = f
-- This means for the Fix type, ana is defined as:
-- => ana :: (a -> f a) -> a -> Fix f
-- For our fix type Nat, ana is defined as:
-- => ana :: (a -> NatF a) -> a -> Fix NatF
-- => ana :: (r -> NatF r) -> r -> Nat

-- implemenation derived from:
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/
-- cata f = out >>> fmap (cata f) >>> f
-- ana f = In <<< fmap (ana f) <<< f
ana' :: Functor f => (a -> f a) -> a -> Fix f
ana' coalgebra a = 
    let 
        fa = coalgebra a -- :: f a
        ana_coalgebra = ana' coalgebra -- :: a -> Fix f
        unfixed = fmap ana_coalgebra fa -- :: f (Fix f)
    in Fix unfixed

-- | fromInt implemented using an anamorphism.
fromInt :: Int -> Nat
fromInt i = Nat $ ana' stepFromInt i

stepFromInt :: Int -> NatF Int
stepFromInt 0 = ZeroF
stepFromInt n = SuccF (n - 1)

-- implementation derived from:
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/
-- apomorphism is the opposite of paramorphism
-- see how the either (sum) is the opposite of the tuple (product)
apo' :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo' coalgebra a =
    let ffa = coalgebra a -- :: f (Either (Fix f) a)
        apo_coalgebra = apo' coalgebra -- :: a -> Fix f
        fromEither e = -- :: Either (Fix f) a -> Fix f
            case e of 
                (Left fixed) -> fixed
                (Right a') -> apo_coalgebra a'
        fff = fmap fromEither ffa -- :: f (Fix f)
    in Fix fff

-- | fromInt' implemented using an apomorphism.
fromInt' :: Int -> Nat
fromInt' i = Nat $ apo' stepFromInt' i

-- | interned contains a map from Int to Nat
-- Only one Nat needs to exist for every number.
-- here we memorize just a few to showcase apomorphism.
interned :: M.Map Int Nat
interned = M.fromList [
        (5, add1 (add1 (add1 (add1 (add1 zero)))))
        , (10, fromInt 10)
    ]

stepFromInt' :: Int -> NatF (Either (Fix NatF) Int)
stepFromInt' n = case M.lookup (n - 1) interned of
    -- short circuit and rather return already existing value.
    (Just (Nat exists)) -> 
        trace "\nshort circuit and return interned 5, instead of recursing\n"
        SuccF (Left (exists)) 
    Nothing -> if n == 0
        then ZeroF
        else SuccF (Right (n - 1))

tests = T.testGroup "apomorphism"
    [
        T.testCase "3" $ T.assertEqual "3" (add1 (add1 (add1 zero))) (fromInt' 3)
        , T.testCase "0" $ T.assertEqual "0" zero (fromInt' 0)
        , T.testCase "5" $ T.assertEqual "5" (fromInt 5) (fromInt' 5)
        -- these should all use the intern value.
        , T.testCase "6" $ T.assertEqual "6" (fromInt 6) (fromInt' 6)
        , T.testCase "7" $ T.assertEqual "7" (fromInt 7) (fromInt' 7)
        , T.testCase "8" $ T.assertEqual "8" (fromInt 8) (fromInt' 8)
    ]