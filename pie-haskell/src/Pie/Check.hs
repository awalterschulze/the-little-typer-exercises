module Pie.Check 
    (checkSame) 
where

checkSame :: (Eq a, Show a) => String -> a -> a -> IO ()
checkSame s x y =
    if x == y
    then return ()
    else putStrLn $ "FAIL: " ++ s ++ ": <" ++ show x ++ "> /= <" ++ show y ++ ">"