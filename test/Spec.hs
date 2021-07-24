import Test.QuickCheck
import Test.QuickCheck.Instances.Vector

import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Vector.Sort
import Data.Tree.BallTree.Internal
import qualified Data.Tree.BallTree as BT
import qualified Data.Tree.BallTree.Inplace as BTI

main :: IO ()
main = do
    putStrLn "Testing sortAt"
    quickCheck sortAtOrder
    putStrLn "Testing quicksort"
    quickCheck quicksortOrder
    putStrLn "Testing ballTree"
    quickCheck ballTreeBTRetrieval
    putStrLn "Testing ballTree inplace"
    quickCheck ballTreeBTIRetrieval

sortAtOrder :: V.Vector Float -> Bool 
sortAtOrder vec = and [isSorted (sort i) i j | i <- ids, j <- ids]
  where
    n      = V.length vec
    ids    = [0..n - 1]
    sort i = sortAt vec i id
    isSorted vec' i j
        | i <= j    = vec' V.! i <= vec' V.! j
        | otherwise = vec' V.! i >= vec' V.! j

quicksortOrder :: V.Vector Float -> Bool 
quicksortOrder vec = and [isSorted (qs vec) i j | i <- ids, j <- ids]
  where
    n    = V.length vec
    ids  = [0..n - 1]
    qs v = quicksort vec id
    isSorted vec' i j
        | i <= j    = vec' V.! i <= vec' V.! j
        | otherwise = vec' V.! i >= vec' V.! j

ballTreeBTRetrieval :: V.Vector Float -> Float -> Bool
ballTreeBTRetrieval vec r = and [search (x + s/2) s == BT.ballSearch t m (Ball (x + s/2) s) | x <- V.toList vec]
  where
    s            = 0.0001 + abs r
    m            = \x y -> abs $ x - y
    t            = BT.ballTree vec m
    search x' r' = S.fromList $ V.toList $ V.filter (\y -> m x' y <= r') vec

ballTreeBTIRetrieval :: V.Vector Float -> Float -> Bool
ballTreeBTIRetrieval vec r = and [search (x + s/2) s == BTI.ballSearch t m (Ball (x + s/2) s) | x <- V.toList vec]
  where
    s            = 0.0001 + abs r
    m            = \x y -> abs $ x - y
    t            = BTI.ballTree vec m
    search x' r' = S.fromList $ V.toList $ V.filter (\y -> m x' y <= r') vec