import Control.Monad.State
import Data.Array
import Board
import Player
import Contrahents
import Game

dim = 7 -- Dimension des Spielfeldes


-- Spielstein mit Symbol 'symbol' wird in Spalte 'col' des Arrays 'arr'
-- geworfen. Rueckgabe: (Erfolg, Position des Spielsteins, neues Array)
putInSlot ::
  Char -> Int -> Array (Int,Int) Char
  -> (Bool, (Int,Int), Array (Int,Int) Char)
putInSlot symbol col arr = setField symbol (1,col) arr


-- Es wird versucht, den Spielstein mit dem Symbol symbol auf das Feld (y,x)
-- zu setzen. Dabei wird versucht, den Stein rekursiv nach unten zu reichen,
-- bis man auf ein besetztes Feld stoesst.
-- Rueckgabe: (Erfolg, Position des Spielsteins, neues Array)
setField ::
  Char -> (Int,Int) -> Array (Int,Int) Char
  -> (Bool, (Int,Int), Array (Int,Int) Char)
setField symbol (y,x) arr
  | not $ inRange ((1,1),(dim,dim)) (y,x) = (False, (0,0), arr) -- out of range
  | arr !(y,x) /= '.' = (False, (0,0), arr)             -- schon belegt
  | recSuccess = (True, pos, updatedArray)              -- Stein faellt tiefer
  | otherwise = (True, (y,x), arr // [((y,x), symbol)]) -- kann auf dieses Feld
  where
    (recSuccess,pos,updatedArray) = setField symbol (y+1,x) arr


-- rekursive Zaehlung der Spielsteine in Richtung (dy,dx)
recursivecount ::
  Char -> (Int,Int) -> (Int,Int) -> Array (Int,Int) Char -> Int
recursivecount symbol (y,x) (dy,dx) arr
  | not $ inRange ((1,1),(dim,dim)) (y,x) = 0
  | arr ! (y,x) /= symbol = 0
  | otherwise = 1 + recursivecount symbol (y+dy,x+dx) (dy,dx) arr


-- Zaehlung benachbarter gleicher Spielsteine in verschiedenen Richtungen
-- ausgehend von Feld (y,x) im Array arr
-- Rueckgabe: Anzahl
count :: (Int,Int) -> Array (Int,Int) Char -> Int
count (y,x) arr
  | not $ inRange ((1,1),(dim,dim)) (y,x) = 0
  | otherwise = maximum counts
  where
    counts = map (\vec -> recursivecount symbol (y,x) vec arr +
                          recursivecount symbol (y,x) (flip vec) arr - 1)
                 [(0,1),(1,1),(1,0),(1,-1)]
    flip (y,x) = (-y,-x)
    symbol = arr ! (y,x)


gameloop ::  StateT StateOfGame IO ()
gameloop = do
  -- Spielstatus holen
  s <- get
  -- Spielstatus auswerten
  let b = getBoard s
  let con = getContrahents s
  let player = getActive con
  let symbol = getSymbol player
  let name = getName player
  -- IO: Spielzug
  liftIO $ printBoard b
  liftIO $ putStr $  name ++ ", Einwurf in Spalte: "
  input <- liftIO getLine
  -- Spielzug auswerten
  let col = (read input :: Int)
  let (success, pos, ub) = putInSlot symbol col b
  let number = count pos ub
  -- Spielstatus speichern
  put $ if success
        then (StateOfGame (next con) ub) -- neuer Spielstatus
        else s                           -- Spielstaus unveraendert
  if number >= 4
    then do
      liftIO $ printBoard ub
      liftIO $ putStrLn $ "Der Gewinner ist: " ++ name
    else do
      gameloop

main = do
  let b = generateBoard dim
  putStrLn "Spieler 1, wie lautet dein Name?"
  name1 <- getLine
  let player1 = Player name1 'X'
  putStrLn "Spieler 2, wie lautet dein Name?"
  name2 <- getLine
  let player2 = Player name2 'O'
  let contrahents = Contrahents player1 player2
  let gs = StateOfGame contrahents b
  execStateT gameloop gs



-- for testing purposes

a0 = generateBoard dim
(suc1,(y1,x1),a1) = putInSlot 'X' 4 a0
(suc2,(y2,x2),a2) = putInSlot 'X' 5 a1
(suc3,(y3,x3),a3) = putInSlot 'X' 5 a2
(suc4,(y4,x4),a4) = putInSlot 'X' 6 a3
(suc5,(y5,x5),a5) = putInSlot 'X' 6 a4
(suc6,(y6,x6),a6) = putInSlot 'X' 6 a5
(suc7,(y7,x7),a7) = putInSlot 'X' 7 a6
(suc8,(y8,x8),a8) = putInSlot 'X' 7 a7
(suc9,(y9,x9),a9) = putInSlot 'X' 7 a8
(suc10,(y10,x10),a10) = putInSlot 'X' 7 a9

h = Player "Heiko" 'X'
m = Player "Mona" 'O'

c = Contrahents h m

s = StateOfGame c a0
