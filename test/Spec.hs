import Test.QuickCheck
import Test.QuickCheck.Instances.Vector

import qualified Data.Vector as V
import Data.Vector.Sort

main :: IO ()
main = do
    putStrLn "Testing sortAt"
    quickCheck sortAtOrder
    quickCheck quicksortOrder

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
