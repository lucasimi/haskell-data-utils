module Data.Vector.Sort where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import System.Random
import Data.Vector.Utils

sortAtST :: Ord b => VM.MVector s a -> Int -> Int -> Int -> (a -> b) -> ST s ()
sortAtST vec start end k f = do
    higher <- partitionAtST vec start end k f
    if higher == k 
        then return ()
        else if k < higher 
            then sortAtST vec start higher k f
            else sortAtST vec higher end k f 

sortAt :: Ord b => V.Vector a -> Int -> Int -> Int -> (a -> b) -> V.Vector a
sortAt vec start end k f = runST $ do
    vec' <- V.thaw vec
    sortAtST vec' start end k f
    V.freeze vec'

quicksortST :: (RandomGen g, Ord b) => g -> VM.MVector s a -> Int -> Int -> (a -> b) -> ST s g
quicksortST g vec start end f = if end - start < 2
    then return g
    else do 
        let (pivot, g') = uniformR (start, end - 1) g
        sortAtST vec start end pivot f
        g'' <- quicksortST g' vec start pivot f
        quicksortST g'' vec pivot end f
        return g''

quicksort :: Ord b => V.Vector a -> Int -> Int -> (a -> b) -> V.Vector a
quicksort vec start end f = runST $ do
    vec' <- V.thaw vec
    quicksortST (mkStdGen 42) vec' start end f
    V.freeze vec'
