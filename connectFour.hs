-- Connect Four Game in Haskell Programming Language
-- Created by Thomas Durand - 2015/03/05

-- Creating the two colors for the players
data Color = Red | Yellow
-- A cell can be empty, or filled with one of the colors
data Cell = Empty | Filled Color

-- Creating aliases types for Columns and Grid
type Column = [Cell]
type Grid = [Column]

