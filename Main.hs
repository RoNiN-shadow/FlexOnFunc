module Main where
import LearnHaskell.Random.RandomLogic (inSeq)
import Data.Sequence (fromList)

main :: IO ()
main =
    let
      seqq = fromList [(0,1), (2,4), (4,5)]
      p   = (2,4)
      p1  = (5,4)
    in putStrLn $ show(inSeq p seqq) ++ " " ++ show(inSeq p1 seqq)
    
