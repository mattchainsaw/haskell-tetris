module Util
  where

import Prelude hiding (Left, Right)
import System.CPUTime
import System.IO
import System.Random
import Control.Concurrent

----- CONSTANTS -----
boardRows, boardCols, bStartX, bStartY, enterX, enterY :: Int
boardRows = 16
boardCols = 10
bStartX = 4
bStartY = 5
enterX = 0
enterY = 4
blockRepresentation :: String
blockRepresentation = "##"

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White | Black deriving (Show, Eq)

data Direction = Left | Right | Down | Rotate deriving (Show, Eq)

tern :: Bool -> a -> a -> a
tern b y n = if b then y else n

time :: IO Integer
time = getCPUTime

howLong :: (Fractional a, Integral t) => t -> t -> a
howLong t1 t2 = (fromIntegral (t2 - t1)) / (10^12)

colorString :: String -> Color -> String
colorString s Red     = "\x1b[31m" ++ s
colorString s Green   = "\x1b[32m" ++ s
colorString s Yellow  = "\x1b[33m" ++ s
colorString s Blue    = "\x1b[34m" ++ s
colorString s Magenta = "\x1b[35m" ++ s
colorString s Cyan    = "\x1b[36m" ++ s
colorString s White   = "\x1b[37m" ++ s
colorString s Black   = "\x1b[30m" ++ s

slowPrint :: Int -> String -> IO ()
slowPrint _ [] = do
  putChar '\n'
  return ()
slowPrint n (x:xs) = do
  putChar x
  pause n
  slowPrint n xs

repeatIO :: Int -> IO () -> IO ()
repeatIO 0 _ = do
  return ()
repeatIO n io = do
  io
  repeatIO (n-1) io

pause :: Int -> IO ()
pause t = do
  hFlush stdout
  threadDelay t -- 1000000 = 1 sec
  return ()

blockSpeed :: Int -> Float
blockSpeed 1  = 0.25
blockSpeed 2  = 0.22
blockSpeed 3  = 0.19
blockSpeed 4  = 0.16
blockSpeed 5  = 0.13
blockSpeed 6  = 0.1
blockSpeed 7  = 0.08
blockSpeed 8  = 0.06
blockSpeed 9  = 0.04
blockSpeed 10 = 0.02
blockSpeed _  = blockSpeed 1

nextRand :: StdGen -> (Int, StdGen)
nextRand y = randomR (0,6) y :: (Int, StdGen)

getRand :: IO Int
getRand = do
  seed <- getStdGen
  let x = nextRand seed
  setStdGen $ snd x
  return $ fst x
