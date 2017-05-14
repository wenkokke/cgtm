{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import BitSucc
import Test.QuickCheck

prop_stupidSuccCorrect :: Positive Int -> Property
prop_stupidSuccCorrect (Positive n) =
  property $ stupidSucc n == succ n

main = quickCheck prop_stupidSuccCorrect
