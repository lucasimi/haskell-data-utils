module Data.Tree.BallTree where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Vector.Mutable as VM
import System.Random.MWC
import System.Random.Internal
import Data.Vector.Sort

import Data.Tree.BallTree.Internal

data BinTree a b 
    = Empty
    | Leaf b
    | Node    
        { value :: a
        , left  :: BinTree a b
        , right :: BinTree a b } deriving Show

ball :: Ord b => (a -> a -> b) -> Ball a b -> V.Vector a -> V.Vector a
ball m b = V.filter $ \x -> m (center b) x < radius b

type BallTree a b = BinTree (Ball a b) a

ballTreeST :: Ord b => VM.MVector s a -> (a -> a -> b) -> ST s (BallTree a b)
ballTreeST vec m
    | VM.length vec == 0 = return Empty
    | VM.length vec < 2  = do
        p <- VM.read vec 0
        return $ Leaf p
    | otherwise          = do
        let n     = VM.length vec
            mid   = n `div` 2
            pivot = 0
        center <- VM.read vec pivot
        sortAtST vec mid (m center)
        furth  <- VM.read vec mid
        lft    <- ballTreeST (VM.slice 0 mid vec) m
        rgt    <- ballTreeST (VM.slice mid (n - mid) vec) m 
        return $ Node 
            { value = Ball 
                { center = center
                , radius = m center furth }
            , left  = lft
            , right = rgt }

ballTree :: Ord b => V.Vector a -> (a -> a -> b) -> BallTree a b
ballTree vec m = runST $ do
    vec' <- V.thaw vec
    ballTreeST vec' m

ballSearch :: (Ord a, RealFloat b) => BallTree a b -> (a -> a -> b) -> Ball a b -> S.Set a
ballSearch Empty _ _                                = S.empty  
ballSearch (Leaf c) m (Ball p eps)                  = if m p c < eps then S.singleton c else S.empty
ballSearch (Node (Ball c r) lft rgt) m (Ball p eps) = S.union lftSearch rgtSearch
  where
    lftSearch = if m p c > r + eps 
        then S.empty 
        else ballSearch lft m (Ball p eps)
    rgtSearch = if m p c <= r - eps 
        then S.empty 
        else ballSearch rgt m (Ball p eps)

treeSize :: BinTree a b -> Int 
treeSize b = treeSizeIter b 0
  where
    treeSizeIter Empty acc        = acc
    treeSizeIter (Leaf _) acc     = 1 + acc
    treeSizeIter (Node _ l r) acc = treeSizeIter r (treeSizeIter r acc)