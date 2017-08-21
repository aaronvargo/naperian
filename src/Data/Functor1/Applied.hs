module Data.Functor1.Applied where

import Data.Functor1

newtype Applied a f = Applied { runApplied :: f a } deriving (Show, Eq)

instance Functor1 (Applied a) where
  map1 f = Applied . f . runApplied
