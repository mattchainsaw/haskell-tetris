module Main
  where

import Util hiding (Color(..))
import Board

import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Concurrent

initialize :: IO Int
initialize = do
  setTitle "Terminal Tetris"
  resetScreen
  diff <- selectDifficulty
  drawBoard
  return diff

resetScreen :: IO ()
resetScreen = do
  clearScreen >> setSGR [Reset] >> setCursorPosition 0 0
  setSGR [SetColor Foreground Vivid White]
  setSGR [SetColor Background Dull Black]

selectDifficulty :: IO Int
selectDifficulty = do
  cursorDownLine 1
  slowPrint 20000 "    Ready to play Tetris?"
  slowPrint 20000 "    To quit at anytime, please press the \"Q\" key."
  slowPrint 20000 "    Select a difficulty (1-10):"
  putStr    "    "
  x <- getLine
  return $ read x 

showLevelAndScore :: Int -> Int -> IO ()
showLevelAndScore lvl score = do 
  setCursorPosition 3 30
  setSGR [SetColor Foreground Vivid White]
  putStr $ "Level: " ++ show lvl
  setCursorPosition 4 30
  putStr $ "Score: " ++ show score
  setSGR [SetColor Foreground Dull Black]
  hideCursor

drawBoard :: IO ()
drawBoard = do
  resetScreen
  drawBottom
  drawSides
  return ()
    where
      drawBottom = do
        setCursorPosition 20 4
        putStr "+--------------------+"
      drawSides = do
        repeatIO 16 (do
                    cursorUpLine 1
                    putStr "    |                    |")

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering -- Thanks to Jake! No more pleb.
  diff <- initialize
  x <- getRand
  t <- time
  tern (diff == -1)
       (return ())
       (gameLoop diff 0 (newBoard $ pick x) t)
  return ()

possibleAction :: Handle -> IO a -> IO (Maybe a)
possibleAction hnd x = hReady hnd >>= f
  where f True = x >>= return . Just
        f _    = return Nothing

------------------ GAME ACTIONS --------------------------

updateScreen :: Int -> Int -> Board -> IO ()
updateScreen lvl scr (Board _ b) = do
  paint b 0
  showLevelAndScore lvl scr
  return ()
    where
      paint [] _ = do
        setCursorPosition 32 0
        return ()
      paint (x:xs) n = do
        setCursorPosition (bStartX + n) bStartY
        putStr $ rowToString x
        paint xs (n+1)

play :: Int -> Int -> Board -> IO ()
play diff score b = do
  x <- getRand
  let bo = addPiece (pick x) b
  theTime <- time
  updateScreen diff score bo
  gameLoop diff score bo theTime

gameLoop :: Int -> Int -> Board -> Integer -> IO ()
gameLoop diff scr b t = do
  pause $ 1000000 `div` 40 -- 40 fps
  setCursorPosition 32 0
  ch <- possibleAction stdin getChar
  tern (ch == Just 'q') (return ()) (pause 1)
  newTime <- time
  let speed = tern (howLong t newTime > (blockSpeed diff)) True False
  let whichTime = tern speed newTime t
  let newB = updateBoard b ch speed
  updateScreen diff scr newB
  if (check Down newB)
     then gameLoop diff scr newB whichTime
     else do
       let didYouGetARow = removeCompleteRows newB
       play (tern (fst didYouGetARow && diff < 10) (diff+1) diff) scr $ snd didYouGetARow
