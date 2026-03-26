module LearnHaskell.Random.RandomLogic where

import System.Random (randomR, StdGen)

type Point = (Int, Int)
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)

makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
