-- CPSP 312 - 2020 : Haskell Project by Xindi Wei, Nawaratt Tonrungroj

module PlayBingo where

import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

import BingoEngine


type Action = Int 

-- Game state is a tuple containing record of selected numbers/dice rolls of each player. 
-- First element in the tuple is computer's record, second element is player's record
start_state = ([],[])

start =
  do
    putStrLn ("####### Welcome to Bingo!!! #######")
    play start_state

play :: ([Int], [Int]) -> IO ()
play state =
  do
    putStrLn ("How many number do you want to select?")
    putStrLn ("5 | 10 | 15")
    numselection <- getLine
    if(not(numselection `elem` ["5","10","15"]))
      then play state
    else
      do
        let numselectionint = read numselection :: Int
        putStrLn ("Who starts? y = you | c = computer | other key = exit game")
        whostart <- getLine
        if whostart == "y"
          then person_play state numselectionint whostart
        else if whostart ==  "c"
          then computer_play state numselectionint whostart
        else exit


person_play :: (Ord a, Show a, Num a) => ([Int], [Int]) -> a -> [Char] -> IO ()
person_play state numselection whostart =
  do
    let playerstate = snd state
        computerstate = fst state
        playermatchedcells = getMatchedCells playerstate
        computermatchedcells = getMatchedCells computerstate
    if(whostart == "y" && numselection <= 0)
      then endgame state
    else
      do
        putStrLn ("--------------------------")
        putStrLn ("You've picked: " ++ show playerstate)
        putStrLn ("You've matched: " ++ show playermatchedcells)
        putStrLn ("Computer's picked: " ++ show computerstate)
        putStrLn ("Computer's matched: " ++ show computermatchedcells)
        putStrLn ("--------------------------")
        putStrLn ("# selection remains: " ++ show numselection)
        putStrLn ("Select a number from 1 - 6 | x = exit game")
        numpicked <- getLine
        if numpicked == "x"
          then exit
        else
          --let action = (readMaybe numpicked :: Maybe Action) in
          if (not(numpicked `elem` ["1","2","3","4","5","6"]))
            then person_play state numselection whostart
          else
            let numpickedint = read numpicked :: Int
                newState = (computerstate, playerstate ++ [numpickedint])
            in computer_play newState (numselection-1) whostart
    


computer_play :: (Ord a, Show a, Num a) => ([Int], [Int]) -> a -> [Char] -> IO ()
computer_play state numselection whostart =
  do
    let playerstate = snd state
        computerstate = fst state
    if(whostart == "c" && numselection <= 0)
      then endgame state
    else
      -- TODO: update to random number
      do
        putStrLn ("Computer picks "++show 3)

        let newState = (computerstate ++ [3], playerstate)
        person_play newState numselection whostart



endgame :: ([Int], [Int]) -> IO ()
endgame state =
  do
    let playerstate = snd state
        computerstate = fst state 
        playertotalbingo = getTotalBingo playerstate
        computertotalbingo = getTotalBingo computerstate
    putStrLn("Selected numbers: you = " ++ show playerstate ++ " | computer = " ++ show computerstate)
    putStrLn ("================================")
    putStrLn("****** Bingo stars earned: you = " ++ show playertotalbingo ++ " | computer = " ++ show computertotalbingo ++ " ******")
    putStrLn ("================================")
    putStrLn ("p = play again | other key = exit game")

    line <- getLine
    if line == "p"
      then play start_state
      else exit


exit = putStrLn ("Bye, see you again!!!")


