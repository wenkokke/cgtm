{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import BitSucc
import Data.Enumerable
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (concat)
import qualified Data.Text.IO as T (hPutStrLn,writeFile)
import Data.TM
import Data.TM.Export
import System.IO (hClose,hFlush)
import System.Process


asCohorts :: (CG t) => [t] -> Text
asCohorts bs =
  T.concat $ zipWith (<>) (repeat "\"<Cell>\"\t") (map toCG bs)

runTMasCG :: (Enumerable q, Enumerable i, Enumerable t, CG q, CG t)
          => TM q i t -> [i] -> IO [i]
runTMasCG tm@TM{..} bs = do
  _
  (Just hIn, _, _, p) <-
    createProcess (proc "vislcg3" ["-g",fTM]){std_in = CreatePipe}
  T.hPutStrLn hIn (asCohorts (map toTape bs))
  hClose hIn
  _

main :: IO ()
main = do _ <- runTMasCG binSuccTM [Zero,Zero,One]
          return ()
