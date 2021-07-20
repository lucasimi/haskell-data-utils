module Data.Tree.BallTree where

import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Vector.Mutable as VM
import System.Random
import Data.Vector.Sort

data BinTree a b 
    = Empty
    | Leaf b
    | Node    
        { value :: a
        , left  :: BinTree a b
        , right :: BinTree a b }

data Ball a b = Ball
    { center :: a
    , radius :: b }

ball :: Ord b => (a -> a -> b) -> Ball a b -> V.Vector a -> V.Vector a
ball m b = V.filter $ \x -> m (center b) x < radius b

type BallTree a b = BinTree (Ball a b) a

ballTreeST :: (RandomGen g, Ord b) => g -> VM.MVector s a -> (a -> a -> b) -> ST s (BallTree a b)
ballTreeST g vec m
    | VM.length vec == 0 = return Empty
    | VM.length vec < 2  = do
        p <- VM.read vec 0
        return $ Leaf p
    | otherwise          = do
        let (pivot, g') = uniformR (0, VM.length vec - 1) g
            mid = VM.length vec `div` 2
        center <- VM.read vec pivot
        sortAtST vec mid (m center)
        furth <- VM.read vec mid
        lft <- ballTreeST g' (VM.slice 0 mid vec) m
        rgt <- ballTreeST g' (VM.slice mid (VM.length vec - mid) vec) m 
        return $ Node 
            { value = Ball 
                { center = center
                , radius = m center furth }
            , left  = lft
            , right = rgt }

ballTree :: Ord b => V.Vector a -> (a -> a -> b) -> BallTree a b
ballTree vec m = runST $ do
    vec' <- V.thaw vec
    ballTreeST (mkStdGen 42) vec' m

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