module Board
( generateBoard
, printBoard
) where
import Data.Array
import Data.List
import Data.List.Split

generateBoard dim = array ((1,1),(dim,dim)) (zip listOfIndices listOfFields)
  where
    listOfFields = replicate (dim^2) '.'
    listOfIndices = range ((1,1),(dim,dim))

printBoard arr = putStrLn $ unlines $ chunksOf dim $ toList arr
  where dim = dimension arr

toList arr = map (arr !) $ range ((1,1),(dim,dim))
  where dim = dimension arr

dimension arr = upper
  where ((_,_),(upper,_)) = bounds arr
