-------------------------------------------------------------------------------
-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand, Thomas Buick, Bertille Chevillote - 2015/03/05
-------------------------------------------------------------------------------
import Data.List
import Data.Maybe
import Data.Char
import System.IO

-------------------
-- STRUCTURE
-------------------

-- Creating the two colors for the players
data Color = Red | Yellow deriving (Eq)
instance Show Color where
	show Red 	= " ● "
	show Yellow = " ○ "

-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color deriving (Eq)
instance Show Cell where 
	show Empty 		= " . "
	show (Filled c) = show c
 
-- Creating aliases types for Columns and Grid
type Column = [Cell]
type Grid = [Column]

-- Function to print a grid in Terminal
printGrid::Grid->String
printGrid g = unlines $ (map (concatMap show) (transpose g) ++ [printColNums g])

printColNums:: Grid -> String
printColNums grid = let legals = legalMoves grid in
                       concatMap (\x -> if x `elem` legals then " "++show x++" " else "   ")
                           [1..((length grid)+1)]

-------------------
-- PLAYING
-------------------
-- Add a token in the specified column
addToken::  Column -> Color ->  Column
addToken column color = let (empties,fulls) = span (==Empty) column in 
								tail (empties ++ [Filled color] ++ fulls) 

-- play in the specified column
play::Grid -> Color -> Int -> Grid
play grid color num = (take (num-1) grid ) ++ [addToken (grid!!(num-1)) color] ++ (drop num grid)

gameEven::Grid -> [Int] -> Grid
gameEven grid [] = grid
gameEven grid (num:xs) = gameOdd (play grid Yellow num) xs 

gameOdd::Grid -> [Int] -> Grid
gameOdd grid [] = grid
gameOdd grid (num:xs) = gameEven (play grid Red num) xs 

replace number item list = left ++ (item:right) where (left, (_:right)) = splitAt number list

-- Return the columns where we can legaly play
legalMoves::Grid -> [Int]
legalMoves g = map fst (filter ((==Empty).head.snd) (zip [1..] g))

-------------------
-- CHECK FOR WINNER
-------------------

-- Summarize return a list of color, and the number of consecutive equals colors
-- For instance : ..R.YY => [(.,2), (R,1), (.,1), (Y, 2)]
_summarize::Column -> Cell -> Int -> [(Cell, Int)] -> [(Cell, Int)]
_summarize (currentColor:xs) baseColor number list = 
	if currentColor == baseColor
		then _summarize xs baseColor (number+1) list
		else _summarize xs currentColor 1 ((baseColor, number):list)
_summarize [] baseColor number list = ((baseColor, number):list)

summarize::Column->[(Cell,Int)]
summarize column = _summarize column (column!!0) 0 []

-- Diagonalize return a list with the composants of diagonals
-- For instance diagonalize [[1,2],[3,4]] == [[1], [2, 3], [4]]
addNothing::Column->Int->Bool->Column
addNothing column 0 _ = column
addNothing column index reverse = 
	if reverse
		then addNothing ([Empty]++column) (index-1) reverse
		else addNothing (column++[Empty]) (index-1) reverse

_diagonalize::Grid->Int->Grid
_diagonalize [] _ = []
_diagonalize g index = 
	if index == length g
		then g
		else _diagonalize (replace index (addNothing (addNothing (g!!index) index True) ((length g)-index-1) False) g) (index+1) 

diagonalize::Grid->Grid
diagonalize g = _diagonalize g 0


-- Getting all alignments (horizontal, vertical, diagonals) in a single list
allAlignments::Grid->[Column]
allAlignments grid = concatMap ($ grid) [diagonalize, diagonalize.(map reverse), id, transpose]

-- Won function return a color if there is a winner, and Nothing if there is no winner yet
_won:: Column -> Maybe Color
_won col | length col < 4 = Nothing
_won (c:cs) | c==Empty || (or$map (/=c)$take (4-1) cs) = _won cs
_won (Filled c:_) = Just c

won:: Grid -> Maybe Color
won = listToMaybe.catMaybes.(map _won).allAlignments

-------------------
-- MAIN PROGRAM
-------------------

-- Creating the initial empty Grid
initial::Grid
initial = replicate 7 (replicate 6 Empty)

-- Generic player
class Contestant a where
  move :: a -> Grid -> IO Int -- His play move
  color :: a -> Color         -- His color

data Human = Human Color

-- FIXME digitToInt crash if you enter a letter, or a special caracter
instance Contestant Human where
  move hum grid = do
    moveChar <- getChar
    let moveInt = digitToInt moveChar in 
      if moveInt `elem` (legalMoves grid) then
        return moveInt
      else do
        putStrLn "This is not a legal move!"
        move hum grid

  color (Human col) = col

-- Main game loop
loop::(Contestant a,Contestant b)=>Grid->a->b->IO()
loop grid a b = do
  if null $ legalMoves grid then
    putStrLn "No winner this time!"
  else do
    putStrLn $ printGrid grid
    amove <- move a grid
    let newgrid = play grid (color a) amove  in do
      case won newgrid of
        Just color -> putStrLn (printGrid newgrid ++ "\n" ++ show color ++ "has won !")
        Nothing    -> loop newgrid b a


main :: IO () -- Main function
main = do
  hSetBuffering stdin NoBuffering -- Don't wait to press Enter for input
  hSetEcho stdin False -- Remove the echo on terminal for input character
  loop initial (Human Red) (Human Yellow) -- Starting loop
