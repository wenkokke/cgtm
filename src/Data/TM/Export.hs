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

class ToCG a where toCG :: a -> Text

class FromCG a where fromCG :: Text -> a

-- |Generate a CG rule which adds the start state to the top of the input.
startState :: (ToCG q) => TM q i t -> Text
startState TM{..} =
  "ADDCOHORT (\"<State>\" "<> toCG start <>")"
  <> " BEFORE (\"<Cell>\") IF (-1 (>>>));"

-- |Generate a CG rule which removes any leading blank cells.
cleanUp :: (Enumerable i, ToCG t) => TM q i t -> Text
cleanUp (TM{..} :: TM q i t) = T.unlines
  [ "REMCOHORT (\"<Cell>\" "<> toCG blank <>")"
    <>" IF (NOT -1* "<> anySym <>");"
  , "REMCOHORT (\"<Cell>\" "<> toCG blank <>")"
    <>" IF (NOT  1* "<> anySym <>");"
  ]
  where
    anySym = T.concat $ intersperse " OR " $ map cell (enum :: [i])
    cell i = "(\"<Cell>\" "<> toCG (toTape i) <>")"

-- |Generate a sequence of CG rules which correspond to a single transition
--  rule in the Turing machine.
mkRule :: (ToCG q, ToCG t)
       => TM q i t -> (q, t) -> (q, t, Direction) -> [Text]
mkRule TM{..} (q, t) (q', t', lr) = case lr of
  L -> let moveInst
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
       in ["",comment,moveInst,writeInst]
  R -> let moveInst
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
       in ["",comment,moveInst,writeInst]
  where
    comment   = "# ("<> toCG q <>", "<> toCG t <>")"
                <>" -> ("<> toCG q' <>", "<> toCG t' <>", "
                <> T.pack (show lr) <>")"
    newCell   = "(\"<Cell>\" "<> toCG t' <>")"
    oldCell   = "(\"<Cell>\" "<> toCG t <>" \"OLD\")"
    newState  = "(\"<State>\" "<> toCG q' <>")"
    oldState  = "(\"<State>\" "<> toCG q <>" \"OLD\")"

-- |Generate the full sequence of transition rules for the Turing machine.
mkRules :: (Enumerable q, Enumerable t, ToCG q, ToCG t)
        => TM q i t -> [Text]
mkRules (tm@TM{..} :: TM q i t) = do
  q <- enum :: [q]
  t <- enum :: [t]
  if q == accept || q == reject
    then []
    else mkRule tm (q, t) (next (q, t))

-- |Generate the full CG which corresponds to the Turing machine.
mkCG :: (Enumerable q, Enumerable i, Enumerable t, ToCG q, ToCG t)
     => TM q i t -> Text
mkCG tm@TM{..} = T.unlines $
  [ "BEFORE-SECTIONS"
  , startState tm
  , ""
  , "SECTION"
  , "ADDCOHORT (\"<Cell>\" "<> toCG blank <>") BEFORE (\"<State>\")"
    <>" IF (-1 (>>>));"
  , "ADDCOHORT (\"<Cell>\" "<> toCG blank <>") AFTER (\"<Cell>\")"
    <>" IF (0 (<<<) LINK -1 (\"<State>\"));"
  , ""
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

instance (Enumerable q, Enumerable i, Enumerable t, ToCG q, ToCG t) =>
         ToCG (TM q i t) where
  toCG = mkCG
