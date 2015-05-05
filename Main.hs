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
  if x == "q" || x == "Q"
     then return (-1)
     else return $ read x

drawBoard :: IO ()
drawBoard = do
  resetScreen
  drawBottom
  drawSides
  return ()
    where
      drawBottom = do
        setCursorPosition 20 4
        putStr "----------------------"
      drawSides = do
        repeatIO 16 (do
                    cursorUpLine 1
                    putStr "    |                    |")

main :: IO ()
main = do
  diff <- initialize
  x <- getRand
  t <- time
  tern (diff == -1)
       (return ())
       (gameLoop diff (newBoard $ pick x) t)
  return ()

possibleAction :: Handle -> IO a -> IO (Maybe a)
possibleAction hnd x = hReady hnd >>= f
  where f True = x >>= return . Just
        f _    = return Nothing

------------------ GAME ACTIONS --------------------------

updateScreen :: Board -> IO ()
updateScreen (Board _ b) = do
  paint b 0
  return ()
    where
      paint [] _ = do
        setCursorPosition 32 0
        return ()
      paint (x:xs) n = do
        setCursorPosition (bStartX + n) bStartY
        putStr $ rowToString x
        paint xs (n+1)

play :: Int -> Board -> IO ()
play diff b = do
  x <- getRand
  let bo = addPiece (pick x) b
  theTime <- time
  updateScreen bo
  gameLoop diff bo theTime

gameLoop :: Int -> Board -> Integer -> IO ()
gameLoop diff b t = do
  pause $ 1000000 `div` 60 -- fps (hopefully)
  setCursorPosition 32 0
  ch <- possibleAction stdin getChar
  tern (ch == Just 'q') (return ()) (pause 1)
  newTime <- time
  let speed = tern (howLong t newTime > 0.25) True False
  let whichTime = tern speed newTime t
  let newB = updateBoard b ch speed
  updateScreen newB
  if (check Down newB)
     then gameLoop diff newB whichTime
     else play diff $ removeCompleteRows newB
