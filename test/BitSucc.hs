{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BitSucc where

import Data.Bits
import Data.Enumerable
import Data.Text (pack)
import Data.TM
import Data.TM.Export

-- * Bitwise Successor function on Turing Machine

data Bit = Zero | One deriving (Eq, Show)

instance Enumerable Bit where
  enum = [Zero,One]

instance CG BitOrBlank where
  toCG Nothing     = "_"
  toCG (Just Zero) = "0"
  toCG (Just One)  = "1"

fromBits :: (Integral a, Bits a) => [Bit] -> a
fromBits = foldr ($) 0 . map (flip setBit . fst) .
           filter ((==One) . snd) . zip [0..]

toBits :: (Integral a, Bits a) => a -> [Bit]
toBits n = map (toBit . testBit n) [0..floor (logBase 2 (fromIntegral n))]
  where
    toBit :: Bool -> Bit
    toBit True  = One
    toBit False = Zero

type BitOrBlank = Maybe Bit

data BitSuccState = State0 | State1 | State2 | Halt deriving (Eq, Show)

instance Enumerable BitSuccState where
  enum = [State0,State1,State2,Halt]

instance CG BitSuccState where
  toCG = pack . show

-- |TM for the binary successor function.
binSuccTM :: TM BitSuccState Bit BitOrBlank
binSuccTM = TM{..}
  where
    start    = State0
    accept   = Halt
    reject   = Halt
    blank    = Nothing
    toTape   = Just
    fromTape = id

    next :: (BitSuccState, BitOrBlank) -> (BitSuccState, BitOrBlank, Direction)
    next (State0, Nothing)   = (State1, Nothing  , R)
    next (State0, Just Zero) = (State0, Just Zero, L)
    next (State0, Just One)  = (State0, Just One , L)
    next (State1, Nothing)   = (State2, Just One , L)
    next (State1, Just Zero) = (State2, Just One , R)
    next (State1, Just One)  = (State1, Just Zero, R)
    next (State2, Nothing)   = (Halt  , Nothing  , R)
    next (State2, Just Zero) = (State2, Just Zero, L)
    next (State2, Just One)  = (State2, Just One , L)
    next (Halt  , _)         = error "TM: `next` should not be called from the halt state"

stupidSucc :: (Integral a, Bits a) => a -> a
stupidSucc = fromBits . evalTM binSuccTM . toBits

