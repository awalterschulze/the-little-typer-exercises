module Pie.List where

import Data.Functor.Foldable (cata, unfix, Fix(..), para)

-- | 
-- ListF is the parametric version of the List data type:
-- data List a = Cons a (List a) | Nil
data ListF a r =
    ConsF a r
    | NilF

-- This allows us to define a fixpoint of this recursive data structure:
newtype List a = List (Fix (ListF a))

instance (Show a) => Show (List a) where
    show = show . toList

instance (Eq a) => Eq (List a) where
    (==) x y = toList x == toList y

-- Next we define fmap to make ListF a functor.
-- This can be derived, but is implemented here for transparency.
instance Functor (ListF a) where
    fmap f (ConsF a r) = ConsF a (f r)
    fmap _ NilF = NilF

nil :: List a
nil = List (Fix NilF)

(<:>) :: a -> List a -> List a
(<:>) e (List ls) = List (Fix (ConsF e ls))

recList' :: List a -> b -> (a -> List a -> b -> b) -> b
recList' (List (Fix NilF)) base step = base
recList' (List (Fix (ConsF e es))) base step =
    step e (List es) (recList' (List es) base step)

-- paramorphism's type from Data.Functor.Foldable
-- para :: (Base t (t, a) -> a) -> t -> a
-- type Base (Fix f) = f
-- => para :: (f (Fix f, a) -> a) -> Fix f -> a
-- => para :: (f (Fix f, r) -> r) -> Fix f -> r
-- => para :: (ListF a (List a, r) -> r) -> List a -> r

-- | recList is a paramorphism.
recList :: List a -> r -> (a -> List a -> r -> r) -> r
recList (List ls) base step = para (recRAlgebra base step) ls

-- recRAlgebra simply returns base or applies step, 
-- based on whether the list is empty.
recRAlgebra :: r -> (a -> List a -> r -> r) -> ListF a (Fix (ListF a), r) -> r
recRAlgebra base step NilF = base
recRAlgebra base step (ConsF e (es, r)) = step e (List es) r

toList :: List a -> [a]
toList ls = recList ls [] (\e _ es -> e:es)