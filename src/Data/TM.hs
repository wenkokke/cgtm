{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Data.TM where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import Data.Sequence (ViewL(..),ViewR(..),(><),(|>),(<|),viewl,viewr,empty,fromList)

data Direction = L | R deriving (Eq,Enum,Show)

data TM q i t = (Eq q) => TM
  { start    :: q
  , accept   :: q
  , reject   :: q
  , blank    :: t
  , toTape   :: i -> t
  , fromTape :: t -> Maybe i
  , next     :: (q , t) -> (q , t, Direction)
  }

type Tape t = Seq t
type TMState q t = (Tape t, q, Tape t)

-- |Access the next cell on a tape, extending the tape where neccessary.
firstCell :: TM q i t -> Tape t -> (t, Tape t)
firstCell TM{..} (viewl -> EmptyL) = (blank, empty)
firstCell TM{..} (viewl -> a :< v) = (a, v)

-- |Access the last cell on a tape, extending the tape where neccessary.
lastCell :: TM q i t -> Tape t -> (Tape t, t)
lastCell TM{..} (viewr -> EmptyR) = (empty, blank)
lastCell TM{..} (viewr -> v :> a) = (v, a)

-- |Convert a list of input symbols to an initial state for a TM.
startTMState :: TM q i t -> [i] -> TMState q t
startTMState TM{..} u = (empty, start, fromList (map toTape u))

-- |Compute the next state of the TM given the current state.
stepTM :: TM q i t -> TMState q t -> (TMState q t, Bool)
stepTM tm@TM{..} (u, q, v) =
  let (a, v') = firstCell tm v
      (u', b) = lastCell tm u
      (q', a', lr) = next (q, a)
      halt = q' == accept || q' == reject
  in case lr of
       L -> ((u', q', b <| a' <| v'), halt)
       R -> ((u |> a', q', v'), halt)

-- |Run a TM.
runTM :: TM q i t -> [i] -> TMState q t
runTM tm@TM{..} s = iter (startTMState tm s)
  where
    iter tmstate =
      let (tmstate', halt) = stepTM tm tmstate in
        if halt then tmstate' else iter tmstate'

-- |Evaluate the result of a TM.
evalTM :: TM q i t -> [i] -> [i]
evalTM tm@TM{..} s =
  let (u, _, v) = runTM tm s in
    catMaybes . map fromTape . toList $ u >< v

-- -}
