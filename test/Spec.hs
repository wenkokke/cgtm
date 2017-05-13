{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import BitSucc
import Test.QuickCheck

prop_stupidSuccCorrect (p :: Positive Int) =
  let n = getPositive p in
    stupidSucc n == succ n

main = quickCheck prop_stupidSuccCorrect
