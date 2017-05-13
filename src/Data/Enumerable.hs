module Data.Enumerable where

class Enumerable a where enum :: [a]

instance Enumerable a => Enumerable (Maybe a) where
  enum = Nothing : map Just enum
