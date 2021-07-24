module Main where

import Data.Tree.BallTree
import qualified Data.Vector as V
import System.TimeIt
import Data.Vector.Sort
import Control.Monad

main :: IO ()
main = do
    putStrLn "Running test"
    runTestBallTree
    --runTestQuickcheck
    --runSortAt

data Point a = Point 
    { pointId          :: Int 
    , coordinates :: V.Vector a }

instance Eq (Point a) where
  Point x _ == Point y _ = x == y

instance Ord (Point a) where
  Point x _ <= Point y _ = x < y 

runSortAt :: IO ()
runSortAt = do
    forM_ [0..8] $ \k -> part k
  where
    part k = do
        let n  = 2 ^ k
            mid = round n `div` 2
            d  = dataset 128 (round n)
            pt = sortAt d mid (\(Point _ x) -> - V.sum x)
        (t, p) <- timeItT $ print $ V.length pt
        putStrLn $ "n: " ++ show n ++ ", ratio: " ++ show (t / n) ++ ",\t time: " ++ show t ++ "\n"

runTestBallTree :: IO ()
runTestBallTree = do
    forM_ [0..17] $ \k -> tree k
  where
    tree k = do
        let n  = 2 ^ k
            d  = dataset 128 (round n)
            tr = ballTree d euclideanDistance
        (t, ts) <- timeItT $ print $ treeSize tr
        putStrLn $ "n: " ++ show n ++ ", ratio: " ++ show (t / (n * log n)) ++ ",\t time: " ++ show t ++ "\n"

euclideanDistance :: RealFloat a => Point a -> Point a -> a
euclideanDistance (Point _ p) (Point _ q) = sqrt $ V.sum $ V.map (\(x, y) -> (x - y) ** 2) (V.zip p q)

dataset :: Int -> Int -> V.Vector (Point Float)
dataset dim size = V.fromList $ map point [1..size]
  where
    point i = Point i (V.fromList $ map (const $ fromIntegral i) [1..dim]) 

runTestQuickcheck :: IO ()
runTestQuickcheck = do
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 1))
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 2))
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 3))
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 4))
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 5))
    timeIt $ putStrLn $ "size " ++ show (V.length (qk 6))
  where
    qk n = quicksort (V.reverse $ V.fromList [1..10**n]) id
