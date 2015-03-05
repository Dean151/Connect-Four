------------------------------------------------------------
-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand - 2015/03/05
------------------------------------------------------------
import Data.List

-------------------
-- STRUCTURE
-------------------

-- Creating the two colors for the players
data Color = Red | Yellow deriving Eq

-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color deriving Eq
instance Show Cell where 
	show Empty 			 = "   "
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
addToken::Column->Column->Color->Column
addToken temp [] color = temp
addToken temp (Empty:xs) color = concat [temp, (Filled color:xs)]
addToken temp (x:xs) color = addToken (temp++[x]) xs color

replace number item list = left ++ (item:right) where (left, (_:right)) = splitAt number list

-------------------
-- CHECK FOR WINNER
-------------------

-- Summarize return a list of color, and the number of consecutive equals colors
-- For instance : ..R.YY => [(.,2), (R,1), (.,1), (Y, 2)]
summarizeHelper::Column -> Cell -> Int -> [(Cell, Int)] -> [(Cell, Int)]
summarizeHelper (currentColor:xs) baseColor number list = 
	if currentColor == baseColor
		then summarizeHelper xs baseColor (number+1) list
		else summarizeHelper xs currentColor 1 ((baseColor, number):list)
summarizeHelper [] baseColor number list = ((baseColor, number):list)

summarize::Column->[(Cell,Int)]
summarize column = summarizeHelper column (column!!0) 0 []

-- Diagonalize return a list with the composants of diagonals
-- For instance diagonalize [[1,2],[3,4]] == [[1], [2, 3], [4]]
addNothing::Column->Int->Bool->Column
addNothing column 0 _ = column
addNothing column index reverse = 
	if reverse
		then addNothing ([Empty]++column) (index-1) reverse
		else addNothing (column++[Empty]) (index-1) reverse

diagonalizeHelper::Grid->Int->Grid
diagonalizeHelper [] _ = []
diagonalizeHelper g index = 
	if index == length g
		then g
		else diagonalizeHelper (replace index (addNothing (addNothing (g!!index) index True) ((length g)-index-1) False) g) (index+1) 

diagonalize::Grid->Grid
diagonalize g = diagonalizeHelper g 0

-------------------
-- MAIN PROGRAM
-------------------

main = putStr $ printGrid grid