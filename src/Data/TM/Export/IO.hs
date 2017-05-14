{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TM.Export.IO where

import Control.Arrow ((>>>))
import Data.Enumerable
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Data.Text.IO (hPutStrLn,hGetContents)
import Data.TM
import Data.TM.Export
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process

toCohorts :: (ToCG t) => [t] -> Text
toCohorts bs =
  T.unlines $ zipWith (<>) (repeat "\"<Cell>\"\n\t") (map toCG bs)

fromCohorts :: (FromCG t) => Text -> [t]
fromCohorts = T.lines
          >>> map T.strip
          >>> filter (not . T.null)
          >>> filter (/="\"<Cell>\"")
          >>> map fromCG

runVislCG3 :: FilePath -> Text -> IO Text
runVislCG3 grammar input = do
  (Just hIn, Just hOut, Just hErr, p) <-
    createProcess (proc "vislcg3" ["-g",grammar])
      {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hPutStrLn hIn input
  hClose hIn
  exit <- waitForProcess p
  case exit of
    ExitSuccess -> do
      out <- hGetContents hOut
      hClose hOut
      hClose hErr
      return out
    ExitFailure _ -> do
      hClose hOut
      err <- hGetContents hErr
      error $ T.unpack err

runAsCG :: (Enumerable q, Enumerable i, Enumerable t, FromCG t, ToCG q, ToCG t)
        => TM q i t -> [i] -> IO [i]
runAsCG tm@TM{..} input = do
  withSystemTempFile "tm.rlx" $ \tmpfile tmphandle -> do
    T.hPutStrLn tmphandle (toCG tm)
    hClose tmphandle
    output <- runVislCG3 tmpfile (toCohorts (map toTape input))
    return $ catMaybes $ map fromTape $ fromCohorts output

