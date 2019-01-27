module Chapter5 where

import Pie.Check (checkSame)
import Pie.List
import Pie.Nat

checks :: IO ()
checks = do
    checks1
    checks2
    checks3
    checks4a
    checks4b
    checks4c

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

onetwothree :: List Nat
onetwothree = fromInt 1 <:> (fromInt 2 <:> (fromInt 3 <:> nil))

checks1 :: IO ()
checks1 = do
    checkSame "sum1" (sumList (onetwothree)) (fromInt 6)
    checkSame "sum2" (sumList nil) (fromInt 0)


-- ## 5.2
-- Define a function called maybe-last which takes (in addition to the type
-- argument for the list element) one (List E) argument and one E argument and
-- evaluates to an E with value of either the last element in the list, or the
-- value of the second argument if the list is empty.

-- (claim step-Last
--   (Pi ((E U))
--   (-> E (List E) E E)))
-- (define step-Last
--   (lambda (E)
--   (lambda (e es last)
--     (rec-List es
--       e
--       (lambda (e_ es_ last_)
--         last)
--       ))))

-- (claim maybe-Last
--   (Pi ((E U))
--   (-> (List E) E E)))
-- (define maybe-Last
--   (lambda (E)
--     (lambda (es default)
--       (rec-List es
--         default
--         (step-Last E)))))

maybeLast :: List a -> a -> a
maybeLast ls = recList ls
    (\def -> def)
    stepLast

stepLast :: a -> List a -> (a -> a) -> (a -> a)
stepLast head tail res = (\_ -> res head)

-- (check-same Nat (maybe-Last Nat (:: 1 (:: 2 (:: 3 nil))) 4) 3)
-- (check-same Nat (maybe-Last Nat nil 4) 4)

checks2 :: IO ()
checks2 = do
    checkSame "maybeLast1" (maybeLast onetwothree (fromInt 4)) (fromInt 3)
    checkSame "maybeLast2" (maybeLast nil (fromInt 4)) (fromInt 4)

-- # 5.3
-- ;; Define a function called filter-list which takes (in addition to the type
-- ;; argument for the list element) one (-> E Nat) argument representing a
-- ;; predicate and one (List E) argument.
-- ;;
-- ;; The function evaluates to a (List E) consisting of elements from the list
-- ;; argument where the predicate is true.
-- ;;
-- ;; Consider the predicate to be false for an element if it evaluates to zero,
-- ;; and true otherwise.

-- (claim step-filter
--   (Pi ((E U))
--     (-> (-> E Nat) E (List E) (List E) (List E))))

-- (define step-filter
--   (lambda (E)
--     (lambda (pred h _ filtered)
--       (rec-Nat (pred h)
--         filtered
--         (lambda (n-1 _)
--           (:: h filtered))))))

-- (claim filter-list
--   (Pi ((E U))
--     (-> (-> E Nat) (List E) (List E))))

-- (define filter-list
--   (lambda (E)
--     (lambda (pred es)
--       (rec-List es
--         (the (List E) nil)
--         (step-filter E pred)))))

filterList :: (a -> Nat) -> List a -> List a
filterList pred ls = recList ls
    nil
    (stepFilter pred)

stepFilter :: (a -> Nat) -> a -> List a -> List a -> List a
stepFilter pred e _ res =
    recNat (pred e)
    res
    (\_ _ -> e <:> res)

-- (claim not
--   (-> Nat Nat))
-- (define not
--   (lambda (n)
--     (rec-Nat n
--       1
--       (lambda (n-1 notzero)
--         0))))

-- (claim id
--   (-> Nat Nat))
-- (define id
--   (lambda (n) n))

-- (check-same (List Nat)
--   (filter-list
--     Nat
--     id
--     (:: 1 (:: 2 (:: 3 nil))))
--   (:: 1 (:: 2 (:: 3 nil))))

-- (check-same (List Nat)
--   (filter-list
--     Nat
--     id
--     (:: 1 (:: 0 (:: 2 (:: 3 nil)))))
--   (:: 1 (:: 2 (:: 3 nil))))

-- (check-same (List Nat)
--   (filter-list
--     Nat
--     (lambda (n) (not (id n)))
--     (:: 1 (:: 0 (:: 2 (:: 3 nil)))))
--   (:: 0 nil))

oneZeroTwoThree :: List Nat
oneZeroTwoThree = fromInt 1 <:> (fromInt 0 <:> (fromInt 2 <:> (fromInt 3 <:> nil)))

notZero :: Nat -> Nat
notZero n = recNat n
    (add1 zero)
    (\_ _ -> zero)

checks3 :: IO ()
checks3 = do
    checkSame "filterList1" (filterList id onetwothree) onetwothree
    checkSame "filterList2" (filterList id oneZeroTwoThree) onetwothree
    checkSame "filterList3" (filterList notZero oneZeroTwoThree) (zero <:> nil)


-- # 5.4
-- Define a function called sort-List-Nat which takes one (List Nat) argument
-- and evaluates to a (List Nat) consisting of the elements from the list
-- argument sorted in ascending order.

-- solution adopted from https://gist.github.com/aymanosman/b49dc2b8e1f06c9b22f1d5a607b2df26

lessThan :: Nat -> Nat -> Bool
lessThan n = recNat n
    (\m -> True)
    (\n_1 lessThanN_1 m ->
        recNat m
        False
        (\m_1 _ -> lessThanN_1 m_1)
    )

checks4a :: IO ()
checks4a = do
    checkSame "lessThan1" (lessThan (fromInt 4) (fromInt 2)) False
    checkSame "lessThan2" (lessThan (fromInt 2) (fromInt 4)) True
    checkSame "lessThan3" (lessThan (fromInt 1) (fromInt 0)) False
    checkSame "lessThan4" (lessThan (fromInt 0) (fromInt 1)) True

insert :: List Nat -> Nat -> List Nat
insert ls = recList ls
    (\x -> x <:> nil)
    (\e _ insertIfLess x ->
        if (lessThan x e)
        then x <:> insertIfLess e
        else e <:> insertIfLess x
    )

onethreefour :: List Nat
onethreefour = fromInt 1 <:> (fromInt 3 <:> (fromInt 4 <:> nil))

onetwothreefour :: List Nat
onetwothreefour = fromInt 1 <:> (fromInt 2 <:> (fromInt 3 <:> (fromInt 4 <:> nil)))

checks4b :: IO ()
checks4b = do
    checkSame "insert1" (insert nil (fromInt 2)) ((fromInt 2) <:> nil)
    checkSame "insert2" (insert onethreefour (fromInt 0)) ((fromInt 0) <:> onethreefour)
    checkSame "insert3" (insert onethreefour (fromInt 5)) (fromInt 1 <:> (fromInt 3 <:> (fromInt 4 <:> (fromInt 5 <:> nil))))
    checkSame "insert4" (insert onethreefour (fromInt 2)) onetwothreefour
    checkSame "insert5" (insert onethreefour (fromInt 3)) (fromInt 1 <:> (fromInt 3 <:> (fromInt 3 <:> (fromInt 4 <:> nil))))

insertSort :: List Nat -> List Nat
insertSort ls = recList ls
    nil
    (\e _ sorted ->
        insert sorted e
    )

checks4c :: IO ()
checks4c = do
    checkSame "insertSort1" (insertSort nil) nil
    checkSame "insertSort2" (insertSort ((fromInt 0) <:> onethreefour)) ((fromInt 0) <:> onethreefour)
    checkSame "insertSort3" (insertSort ((fromInt 5) <:> onethreefour)) (fromInt 1 <:> (fromInt 3 <:> (fromInt 4 <:> (fromInt 5 <:> nil))))
    checkSame "insertSort4" (insertSort ((fromInt 2) <:> onethreefour)) onetwothreefour
    checkSame "insertSort5" (insertSort ((fromInt 3) <:> onethreefour)) (fromInt 1 <:> (fromInt 3 <:> (fromInt 3 <:> (fromInt 4 <:> nil))))
