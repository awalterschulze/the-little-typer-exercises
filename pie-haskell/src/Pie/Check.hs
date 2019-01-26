module Pie.Check 
    (checkSame) 
where

checkSame :: (Show a) => String -> a -> a -> IO ()
checkSame s x y =
    if (show x) == (show y) 
    then return ()
    else putStrLn $ s ++ ": <" ++ show x ++ "> /= <" ++ show y ++ ">"