{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TM.Export where

import Data.Enumerable
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.TM
import Data.Text (Text)
import qualified Data.Text as T

class CG a where toCG :: a -> Text

-- |Print anything with a `Show` instance between quotes.
reading :: (CG a) => a -> Text
reading x = "\"" <> toCG x <> "\""

-- |Generate a CG rule which adds the start state to the top of the input.
startState :: (CG q) => TM q i t -> Text
startState TM{..} =
  "ADDCOHORT (\"<State>\" \""<> toCG start <>"\")"
  <> " BEFORE (\"<Cell>\") IF (-1 (>>>));"

-- |Generate a CG rule which removes any leading blank cells.
cleanUp :: (Enumerable i, CG t) => TM q i t -> Text
cleanUp (TM{..} :: TM q i t) =
  "REMCOHORT (\"<Cell>\" "<> reading blank <>")"
  <> " IF (NOT -1* "<> anySym <>");"
  where
    anySym = T.concat $ intersperse " OR " $ map cell (enum :: [i])
    cell i = "(\"<Cell>\" \""<> toCG (toTape i) <>"\")"

-- |Generate a sequence of CG rules which correspond to a single transition
--  rule in the Turing machine.
mkRule :: (CG q, CG t) => TM q i t -> (q, t) -> (q, t, Direction) -> [Text]
mkRule TM{..} (q, t) (q', t', lr) = case lr of
  L -> let extendTape =
             "ADDCOHORT "<> blankCell <>" BEFORE "<> oldState
             <>" IF (1 "<> oldCell <>" LINK -2 (>>>));"
           moveInst
             | q' == accept =
               "# ACCEPT"
             | q' == reject =
               "# REJECT"
             | otherwise =
               "ADDCOHORT "<> newState <>" BEFORE (\"<Cell>\")"
               <>" IF (1 "<> oldState <>" LINK 1 "<> oldCell <>");"
           writeInst =
             "ADDCOHORT "<> newCell <>" AFTER "<> oldCell
             <>" IF (-1 "<> oldState <>");"
       in ["",comment,extendTape,moveInst,writeInst]
  R -> let extendTape =
             "ADDCOHORT "<> blankCell <>" AFTER "<> oldState
             <>" IF (0 (<<<));"
           moveInst
             | q' == accept =
               "# ACCEPT"
             | q' == reject =
               "# REJECT"
             | otherwise = 
             "ADDCOHORT "<> newState <>" BEFORE (\"<Cell>\")"
             <>" IF (-2 "<> oldState <>" LINK 1 "<> oldCell <>");"
           writeInst = 
             "ADDCOHORT "<> newCell <>" AFTER "<> oldCell
             <>" IF (-1 "<> oldState <>");"
       in ["",comment,extendTape,moveInst,writeInst]
  where
    comment   = "# ("<> reading q <>", "<> reading t <>")"
                <>" -> ("<> reading q' <>", "<> reading t' <>", "
                <> T.pack (show lr) <>")"
    blankCell = "(\"<Cell>\" "<> reading blank <>")"
    newCell   = "(\"<Cell>\" "<> reading t' <>")"
    oldCell   = "(\"<Cell>\" "<> reading t <>" \"OLD\")"
    newState  = "(\"<State>\" "<> reading q' <>")"
    oldState  = "(\"<State>\" "<> reading q <>" \"OLD\")"

-- |Generate the full sequence of transition rules for the Turing machine.
mkRules :: (Enumerable q, Enumerable t, CG q, CG t)
        => TM q i t -> [Text]
mkRules (tm@TM{..} :: TM q i t) = do
  q <- enum :: [q]
  t <- enum :: [t]
  if q == accept || q == reject
    then []
    else mkRule tm (q, t) (next (q, t))

-- |Generate the full CG which corresponds to the Turing machine.
mkCG :: (Enumerable q, Enumerable i, Enumerable t, CG q, CG t)
     => TM q i t -> Text
mkCG tm@TM{..} = T.unlines $
  [ "BEFORE-SECTIONS"
  , startState tm
  , ""
  , "SECTION"
  , "ADD (\"<State>\" \"OLD\") (\"<State>\");"
  , "ADD (\"<Cell>\" \"OLD\") (\"<Cell>\") IF (-1 (\"<State>\" \"OLD\"));"
  ]
  <>
  mkRules tm
  <>
  [ ""
  , "REMCOHORT (\"<State>\" \"OLD\");"
  , "REMCOHORT (\"<Cell>\" \"OLD\");"
  , ""
  , "AFTER-SECTIONS"
  , cleanUp tm
  ]

instance (Enumerable q, Enumerable i, Enumerable t, CG q, CG t) =>
         CG (TM q i t) where
  toCG = mkCG
