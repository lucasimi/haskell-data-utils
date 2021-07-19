module Data.Tree.BallTree where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

data BinTree a = BinTree
    { value :: a
    , left  :: BinTree a
    , right :: BinTree a }

data Ball a b = Ball
    { center :: a
    , radius :: b }

data BallTree a b = BallTree
    { metric :: a -> a -> b 
    , tree   :: BinTree (Ball a b) }

ballTreeST :: VM.MVector s a -> (a -> a -> b) -> ST s ()
ballTreeST vec m = undefined