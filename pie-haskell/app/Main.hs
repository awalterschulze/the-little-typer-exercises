module Main where

import qualified Chapter3
import qualified Chapter4
import qualified Chapter5

main :: IO ()
main = do
    Chapter3.checks
    Chapter4.checks
    Chapter5.checks
