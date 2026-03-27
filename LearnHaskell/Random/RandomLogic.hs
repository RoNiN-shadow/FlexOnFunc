module LearnHaskell.Random.RandomLogic where

import System.Random (uniformR, StdGen)
import Data.Sequence (Seq(..))

type Point = (Int, Int)
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)

makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint BoardInfo{width = w, height = h} g =
    let
        (x, g1) = uniformR (0, w - 1) g
        (y, g2) = uniformR (0, h - 1) g1
        p = (x, y)
    in (p, g2)

inSeq :: Point -> Seq Point -> Bool
inSeq _ Empty = False 
inSeq p (x:<|xs)
    | p == x    = True
    | otherwise = inSeq p xs
