------------------------------------------------------------
-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand - 2015/03/05
------------------------------------------------------------

-- Creating the two colors for the players
data Color = Red | Yellow deriving (Show)

-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color
instance Show Cell where 
	show Empty 			 = "   "
	show (Filled Red) 	 = " R "
	show (Filled Yellow) = " Y "
 
-- Creating aliases types for Columns and Grid
type Column = [Cell]
type Grid = [Column]

-- Creating the starting empty Grid
emptyColumn = replicate 6 Empty
grid = replicate 7 emptyColumn
