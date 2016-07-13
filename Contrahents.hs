module Contrahents
( Contrahents(..)
, getActive
, next
) where

import Player
data Contrahents = Contrahents Player Player

getActive :: Contrahents -> Player
getActive (Contrahents player1 _) = player1

next :: Contrahents -> Contrahents
next (Contrahents player1 player2) = Contrahents player2 player1
