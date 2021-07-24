module Data.Tree.BallTree.Internal where

data Ball a b = Ball
    { center :: a
    , radius :: b } deriving Show
