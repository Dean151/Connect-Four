------------------------------------------------------------
-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand - 2015/03/05
------------------------------------------------------------
import Data.List

-- Creating the two colors for the players
data Color = Red | Yellow deriving (Show)

-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color
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

-- adding a token in a column
addToken::Column->Column->Color->Column
addToken temp [] color = temp
addToken temp (Empty:xs) color = concat [temp, (Filled color:xs)]
addToken temp (x:xs) color = addToken (temp++[x]) xs color



main = putStr $ printGrid grid