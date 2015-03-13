-------------------------------------------------------------------------------
-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand, Thomas Buick, Bertille Chevillote - 2015/03/05
-------------------------------------------------------------------------------
import Data.List

-------------------
-- STRUCTURE
-------------------

-- Creating the two colors for the players
data Color = Red | Yellow deriving Eq

-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color deriving Eq
instance Show Cell where 
	show Empty 			 = " . "
	show (Filled Red) 	 = " ● "
	show (Filled Yellow) = " ○ "
 
-- Creating aliases types for Columns and Grid
type Column = [Cell]
type Grid = [Column]

-- Function to print a grid in Terminal
printGrid::Grid->String
printGrid g = tail $ concatMap (('\n' :) . (concatMap show)) (transpose g)

-- Creating the initial empty Grid
emptyColumn = replicate 6 Empty
grid = replicate 7 emptyColumn


-------------------
-- PLAYING
-------------------

-- adding a token in a column
_addToken::Column->Column->Color->Column
_addToken temp [] color = temp
_addToken temp (Empty:xs) color = concat [temp, (Filled color:xs)]
_addToken temp (x:xs) color = addToken (temp++[x]) xs color

addToken::Column->Color->Column
addToken column color = _addToken [] column color

replace number item list = left ++ (item:right) where (left, (_:right)) = splitAt number list

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

-------------------
-- MAIN PROGRAM
-------------------

main = putStr $ printGrid grid