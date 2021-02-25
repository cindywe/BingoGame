-- CPSP 312 - 2020 : Haskell Project by Xindi Wei, Nawaratt Tonrungroj

module BingoDice
  (rollADice
  ) where

import System.Random

rollADice :: IO Int 
rollADice = getStdRandom (randomR (1,6))