{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import BitSucc
import Data.TM.Export.IO
import Test.QuickCheck
import Test.QuickCheck.Monadic

stupidSuccIO :: Int -> IO Int
stupidSuccIO n = do
  let b = toBits n
  b' <- runAsCG bitSuccTM b
  let n' = fromBits b'
  return n'

prop_stupidSuccIOCorrect :: Positive Int -> Property
prop_stupidSuccIOCorrect (Positive n) = monadicIO $ do
  sn <- run (stupidSuccIO n)
  assert $ sn == succ n

main = quickCheck prop_stupidSuccIOCorrect
