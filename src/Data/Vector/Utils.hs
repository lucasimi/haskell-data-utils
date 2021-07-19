module Data.Vector.Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Mutable as VM
import Control.Monad
import Control.Monad.ST
import Data.STRef

partitionAtST :: Ord b => VM.MVector s a -> Int -> (a -> b) -> ST s Int
partitionAtST vec i f = do
    VM.swap vec 0 i
    value <- VM.read vec 0
    higherRef <- newSTRef 1
    forM_ [1 .. (VM.length vec - 1)] $ \j -> do
        val <- VM.read vec j
        when (f val <= f value) $ do
            higher <- readSTRef higherRef
            VM.swap vec higher j
            modifySTRef higherRef (+1)
    higher <- readSTRef higherRef
    VM.swap vec 0 (higher - 1)
    return $ higher - 1

partitionAt :: Ord b => V.Vector a -> Int -> (a -> b) -> Int
partitionAt vec k f = runST $ do
    vec' <- V.thaw vec
    partitionAtST vec' k f

