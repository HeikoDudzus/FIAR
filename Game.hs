module Game
( StateOfGame(..)
, getBoard
, getContrahents
) where

import Data.Array
import Contrahents
data StateOfGame = StateOfGame Contrahents (Array (Int,Int) Char)

getBoard :: StateOfGame -> Array (Int,Int) Char
getBoard (StateOfGame _ arr) = arr

getContrahents :: StateOfGame -> Contrahents
getContrahents (StateOfGame c _) = c
