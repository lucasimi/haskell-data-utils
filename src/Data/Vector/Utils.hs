module Data.Vector.Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as VG
import qualified Data.Vector.Mutable as VM
import Control.Monad
import Control.Monad.ST
import Data.STRef

partitionAtST :: Ord b => VM.MVector s a -> Int -> Int -> Int -> (a -> b) -> ST s Int
partitionAtST vec start end i f = do
    VM.swap vec start i
    value <- VM.read vec start
    higherRef <- newSTRef $ start + 1
    forM_ [(start + 1) .. (end - 1)] $ \j -> do
        val <- VM.read vec j
        when (f val <= f value) $ do
            higher <- readSTRef higherRef
            VM.swap vec higher j
            modifySTRef higherRef (+1)
    higher <- readSTRef higherRef
    VM.swap vec start (higher - 1)
    return $ higher - 1

partitionAt :: Ord b => V.Vector a -> Int -> Int -> Int -> (a -> b) -> Int
partitionAt vec start end k f = runST $ do
    vec' <- V.thaw vec
    partitionAtST vec' start end k f

