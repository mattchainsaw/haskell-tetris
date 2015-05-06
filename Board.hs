module Board 
  where

import Prelude hiding (Left, Right)

import Util

----- BLOCK -----

data Block = Block { pos :: (Int, Int)
                   , col :: Color} deriving Eq

instance Show Block where
  show b = colorString blockRepresentation $ col b

value :: Block -> Bool
value (Block _ Black) = False
value _ = True

moveBlock :: Direction -> Block -> Block
moveBlock Left   (Block (x,y) c) = Block (x,y-1) c
moveBlock Right  (Block (x,y) c) = Block (x,y+1) c
moveBlock Down   (Block (x,y) c) = Block (x+1,y) c
moveBlock Rotate b               = b

----- SHAPE -----

data Shape = Shape Block Block Block Block deriving (Eq, Show)

shapeInfo :: Shape -> String
shapeInfo (Shape q w e r) = work [q,w,e,r]
  where
    work [] = ""
    work (x:xs) = (show $ pos x) ++ work xs

moveShape :: Direction -> Shape -> Shape
moveShape Rotate s = rotate s
moveShape dir (Shape q w e r) = fromL $ map (moveBlock dir) [q,w,e,r]
  where
    fromL [a,s,d,f] = Shape a s d f

pick :: Int -> Shape
pick 0 = shapeI
pick 1 = shapeO
pick 2 = shapeT
pick 3 = shapeL
pick 4 = shapeJ
pick 5 = shapeS
pick 6 = shapeZ

shapeI, shapeO, shapeT, shapeL, shapeJ, shapeS, shapeZ :: Shape
shapeI = Shape (Block (enterX, enterY) Yellow)
               (Block (enterX, enterY-1) Yellow)
               (Block (enterX, enterY+1) Yellow)
               (Block (enterX, enterY+2) Yellow)
shapeO = Shape (Block (enterX, enterY) Cyan)
               (Block (enterX, enterY+1) Cyan)
               (Block (enterX+1, enterY+1) Cyan)
               (Block (enterX+1, enterY) Cyan)
shapeT = Shape (Block (enterX, enterY) White)
               (Block (enterX+1, enterY) White)
               (Block (enterX, enterY+1) White)
               (Block (enterX, enterY-1) White)
shapeL = Shape (Block (enterX, enterY) Red)
               (Block (enterX, enterY+1) Red)
               (Block (enterX, enterY-1) Red)
               (Block (enterX+1, enterY-1) Red)
shapeJ = Shape (Block (enterX, enterY) Blue)
               (Block (enterX, enterY+1) Blue)
               (Block (enterX, enterY-1) Blue)
               (Block (enterX+1, enterY+1) Blue)
shapeS = Shape (Block (enterX, enterY) Magenta)
               (Block (enterX, enterY+1) Magenta)
               (Block (enterX+1, enterY) Magenta)
               (Block (enterX+1, enterY-1) Magenta)
shapeZ = Shape (Block (enterX, enterY) Green)
               (Block (enterX+1, enterY) Green)
               (Block (enterX, enterY-1) Green)
               (Block (enterX+1, enterY+1) Green)

rotate :: Shape -> Shape
rotate (Shape q w e r) = Shape q (pivot w) (pivot e) (pivot r)
  where pivot p = Block ((snd $ pos p) + (fst $ pos q) - (snd $ pos q)
                       , (fst $ pos q) + (snd $ pos q) - (fst $ pos p)) $ col p

----- BOARD -----

data Board = Board { focus :: Shape
                   , board :: [[Block]] } deriving Eq

instance Show Board where
  show (Board _ b) = showB b
    where 
      showB [] = ""
      showB (x:xs) = showB' x ++ showB xs
      showB' [] = "\n"
      showB' (x:xs) = show x ++ showB' xs

rowToString :: [Block] -> String
rowToString [] = ""
rowToString (b:bs) = show b ++ rowToString bs

activeBlocks :: Board -> Int
activeBlocks b = find 0 $ board b
  where 
    find n [] = n
    find n (x:xs) = find (reallyFind n x) xs
    reallyFind a [] = a
    reallyFind a (x:xs) = reallyFind ((tern (col x == Black) 0 1) + a) xs

newBoard :: Shape -> Board
newBoard starter = addPiece starter (Board starter nb)
  where
    nb = [[ Block (x,y) Black | x <- [0.. boardCols-1]] | y <- [0..boardRows-1]]

addPiece :: Shape -> Board -> Board
addPiece (Shape q w e r) b =
  (Board (Shape q w e r) (board $ foldl addBlock b [q, w, e, r]))

addBlock :: Board -> Block -> Board
addBlock brd blk = changeBlock (pos blk) (col blk) brd

removePiece :: Shape -> Board -> Board
removePiece (Shape q w e r) brd =
     (Board (Shape q w e r) (board $ foldl removeBlock brd [q,w,e,r]))

removeBlock :: Board -> Block -> Board
removeBlock brd blk = changeBlock (pos blk) Black brd

changeBlock :: (Int, Int) -> Color -> Board -> Board
changeBlock (x,y) blkColor b =
  tern (x < 0 || x >= boardRows || y < 0 || y >= boardCols)
       b
       (Board (focus b) (setter (x,y) $ board b))
         where
           setter (0,c) (s:ss)  = otherSetter c s : ss
           setter (r,c) (s:ss)  = s : setter ((r-1),c) ss
           otherSetter 0 (s:ss) = (Block (x,y) blkColor) : ss
           otherSetter c (s:ss) = s : otherSetter (c-1) ss

getBlock :: (Int, Int) -> Board -> Bool
getBlock (x,y) b = (getCol (x,y) $ board b)
  where
    getCol _ [] = True
    getCol (0,c) (s:ss)  = otherGetter c s
    getCol (r,c) (s:ss)  = getCol ((r-1),c) ss
    otherGetter _ []     = True
    otherGetter 0 (s:ss) = tern (col s == Black) False True
    otherGetter c (s:ss) = otherGetter (c-1) ss

updateBoard :: Board -> Maybe Char -> Bool -> Board
updateBoard b ch bool = tern bool (update Down b) (wut ch b)

update :: Direction -> Board -> Board
update dir bd = tern (check dir bd)
    (addPiece (moveShape dir (focus bd)) $ removePiece (focus bd) bd) bd

check :: Direction -> Board -> Bool
check dir b = ((activeBlocks b) ==
  (activeBlocks $ addPiece (moveShape dir (focus b)) $ removePiece (focus b) b)) &&
    inBounds dir (focus b)

inBounds :: Direction -> Shape -> Bool
inBounds dir (Shape q w e r) =
  False `elem` (map checkB [q,w,e,r])
    where
      checkB b = checkP $ pos $ moveBlock dir b
      checkP (_,boardCols) = False
      checkP (_,-1)        = False
      checkP (boardRows,_) = False
      checkP  _            = True

wut :: Maybe Char -> Board -> Board
wut ch b = case ch of
           Just 'a' -> update Left b
           Just 'd' -> update Right b
           Just 's' -> update Down b
           Just 'w' -> update Rotate b
           _        -> b

removeCompleteRows :: Board -> Board
removeCompleteRows (Board p b) = normalize $ Board p $ fixIt b
  where
    fixIt [] = []
    fixIt (x:xs) = fixRow x : fixIt xs
    fixRow r = tern (False `elem` (map (\x -> tern (Black == col x) False True) r))
                    r
                    [ Block pts Black | pts <- (map pos r)]

-- O (n^2) bleh
normalize :: Board -> Board
normalize (Board p b) = Board p $ reverse $ swapper $ reverse b
  where
    swapper [] = []
    swapper (x:[]) = x : []
    swapper (x:xs) = tern ((gone x) && (not $ allGone xs)) (swapper $ downer xs) (x : swapper xs)
    gone r = tern (True `elem` (map (\x -> tern (Black == col x) False True) r)) False True
    allGone rs = tern (False `elem` (map gone rs)) False True
    downer (x:[]) = (map (moveBlock Down) x) : [Block p Black | p <- map pos x] : []
    downer (x:xs) = (map (moveBlock Down) x) : downer xs

-- test
--      removeCompleteRows $ foldl addBlock (newBoard $ pick 1) [ Block (10,y) Util.Cyan | y <- [0..9]]
