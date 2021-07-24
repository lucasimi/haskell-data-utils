module Data.Tree.BallTree.Inplace where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Vector.Mutable as VM
import Data.Vector.Sort
import Data.Tree.BallTree.Internal

data Cell a b
    = Empty
    | Leaf a
    | BallCell (Ball a b)
    deriving Show

data BallTree a b = BallTree 
    { tree :: V.Vector (Cell a b)
    , root :: Int }
    deriving Show

left :: Int -> Int 
left n = 2 * n + 1

right :: Int -> Int 
right n = 2 * n + 2

parent :: Int -> Int 
parent n = (n - 1) `div` 2

getLeft :: BallTree a b -> BallTree a b
getLeft (BallTree t r) = BallTree t (left r)

getRight :: BallTree a b -> BallTree a b
getRight (BallTree t r) = BallTree t (right r)

ballTree :: Ord b => V.Vector a -> (a -> a -> b) -> BallTree a b
ballTree vec m = runST $ do
    let n = 2 ^ ceiling (logBase 2 (fromIntegral $ V.length vec))
    vec' <- V.thaw vec
    bt <- VM.replicate (2 * n) Empty
    ballTreeST vec' bt 0 m 
    bt' <- V.freeze bt
    return BallTree 
        { tree = bt'
        , root = 0 }

ballTreeST :: Ord b => VM.MVector s a -> VM.MVector s (Cell a b) -> Int -> (a -> a -> b) -> ST s ()
ballTreeST vec bt i m
    | VM.length vec == 0 = do
        VM.write bt i Empty
    | VM.length vec < 2  = do
        p <- VM.read vec 0
        VM.write bt i (Leaf p)
    | otherwise          = do
        let n     = VM.length vec
            pivot = 0
            mid   = n `div` 2
        center <- VM.read vec pivot
        sortAtST vec mid (m center)
        furth  <- VM.read vec mid
        VM.write bt i (BallCell $ Ball { center = center, radius = m center furth })
        ballTreeST (VM.slice 0 mid vec) bt (left i) m
        ballTreeST (VM.slice mid (n - mid) vec) bt (right i) m 

ballSearch :: (Ord a, RealFloat b) => BallTree a b -> (a -> a -> b) -> Ball a b -> S.Set a
ballSearch bt m (Ball p eps) = case tree bt V.! root bt of
    Empty               -> S.empty 
    Leaf c              -> if m p c < eps then S.singleton c else S.empty 
    BallCell (Ball c r) -> S.union lft rgt
      where
        lft = if m p c > r + eps
            then S.empty 
            else ballSearch (getLeft bt) m (Ball p eps)
        rgt = if m p c <= r - eps
            then S.empty 
            else ballSearch (getRight bt) m (Ball p eps) 

treeSize :: BallTree a b -> Int 
treeSize b = V.length $ tree b