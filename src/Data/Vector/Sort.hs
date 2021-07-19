module Data.Vector.Sort where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST
import System.Random
import Data.Vector.Utils

sortAtST :: Ord b => VM.MVector s a -> Int -> (a -> b) -> ST s ()
sortAtST vec k f = do
    higher <- partitionAtST vec k f
    if higher == k 
        then return ()
        else if k < higher 
            then sortAtST (VM.slice 0 higher vec) k f
            else sortAtST (VM.slice higher (VM.length vec - higher) vec) (k - higher) f 

sortAt :: Ord b => V.Vector a -> Int -> (a -> b) -> V.Vector a
sortAt vec k f = runST $ do
    vec' <- V.thaw vec
    sortAtST vec' k f
    V.freeze vec'

quicksortST :: (RandomGen g, Ord b) => g -> VM.MVector s a -> (a -> b) -> ST s g
quicksortST g vec f = if VM.length vec < 2
    then return g
    else do 
        let (pivot, g') = uniformR (0, VM.length vec - 1) g
        sortAtST vec pivot f
        g'' <- quicksortST g' (VM.slice 0 pivot vec) f
        quicksortST g'' (VM.slice pivot (VM.length vec - pivot) vec) f
        return g''

quicksort :: Ord b => V.Vector a -> (a -> b) -> V.Vector a
quicksort vec f = runST $ do
    vec' <- V.thaw vec
    quicksortST (mkStdGen 42) vec' f
    V.freeze vec'
