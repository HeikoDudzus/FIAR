module Player
( Player (..)
, getSymbol
, getName
) where

data Player = Player String Char

getSymbol :: Player -> Char
getSymbol (Player _ c) = c

getName :: Player -> String
getName (Player name _) = name
