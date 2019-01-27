module Main where

import qualified Pie.Nat
import qualified Test.Tasty as T

main :: IO ()
main = T.defaultMain $ T.testGroup "Spec.hs" [
        Pie.Nat.tests
    ]
