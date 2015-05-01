import System.Random
import Control.Monad

import Piece

printBlock :: Block -> IO ()
printBlock x = putStrLn $ show x

nextRand :: StdGen -> (Int, StdGen)
nextRand y = randomR (0,6) y :: (Int, StdGen)

main :: IO ()
main = forever $ do
  trash <- getLine
  seed <- getStdGen
  let x = nextRand seed
  setStdGen $ snd x
  printBlock $ make $ get $ fst x
