module Pie.Equal (
    same
    , cong
    , Equal
) where

data Equal x = Equal x x

same :: x -> Equal x
same e = Equal e e

cong :: (Equal x) -> (x -> y) -> (Equal y)
cong (Equal from to) f = Equal (f from) (f to)

instance Functor Equal where
    fmap f e = cong e f

-- # functor laws
-- 1. fmap id = id
-- 2. fmap (f . g)  ==  fmap f . fmap g

-- 1. fmap id = id
-- fmap id (Equal from to) 
-- = cong (Equal from to) id
-- = Equal (id from) (id to)
-- = Equal from to
-- = id (Equal from to)

-- 2. fmap (f . g)  ==  fmap f . fmap g
-- fmap (f . g) (Equal from to)
-- = cong (Equal from to) (f . g)
-- = Equal ((f . g) from) ((f . g) to)
-- = Equal (f (g from)) (f (g to))
-- = cong (Equal (g from) (g to)) f
-- = fmap f (Equal (g from) (g to))
-- = fmap f (fmap g (Equal from to))
-- = (fmap f . fmap g) (Equal from to)
