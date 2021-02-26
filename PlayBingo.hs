-- CPSP 312 - 2020 : Haskell Project by Xindi Wei, Nawaratt Tonrungroj

module PlayBingo where

import System.IO
import Text.Read   (readMaybe)
import Data.Maybe   (fromJust)

import BingoEngine

import BingoDice

-- To play game:
-- cabal repl -b random,containers
-- :l PlayBingo
-- start

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
    putStrLn ("How many rounds do you want to play?")
    putStrLn ("5 | 10 | 15")
    numselection <- getLine
    if(not(numselection `elem` ["5","10","15"]))
      then play state
    else
      do
        let numselectionint = read numselection :: Int -- how many rounds
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
        playermatchedcells = getMatchedCells playerstate -- here get MatchedCells calls BingoEngue
        computermatchedcells = getMatchedCells computerstate
    if(whostart == "y" && numselection <= 0)
      then endgame state
    else
      do
        putStrLn ("--------------------------")
        putStrLn (get_player_state_string playerstate playermatchedcells)
        putStrLn (get_computer_state_string computerstate computermatchedcells)
        putStrLn ("--------------------------")
        putStrLn ("# selection remains: " ++ show numselection)
        putStrLn ("t = toss the dice | p = pick a number manually from [1-6] | x = exit game")
        isToss <- getLine
        if isToss == "x"
          then exit
        else
          if isToss == "t"
            then do 
              numTossed <- rollADice -- result of roll a dice
              putStrLn ("You tossed "++show numTossed)
              let newState = (computerstate, playerstate ++ [numTossed]) 
               in computer_play newState (numselection-1) whostart
        else
           do
              putStrLn ("Pick a Number from 1 to 6 ")
              numpicked <- getLine
              putStrLn ("You picked "++show numpicked)
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
      do
        numTossed <- rollADice -- result of roll a dice
        putStrLn ("Computer tossed "++show numTossed)
        let newState = (computerstate ++ [numTossed], playerstate)
        person_play newState numselection whostart



endgame :: ([Int], [Int]) -> IO ()
endgame state =
  do
    let playerstate = snd state
        computerstate = fst state 
        playermatchedcells = getMatchedCells playerstate
        computermatchedcells = getMatchedCells computerstate
        playertotalbingo = getTotalBingo playerstate
        computertotalbingo = getTotalBingo computerstate
    putStrLn ("--------------------------")
    putStrLn (get_player_state_string playerstate playermatchedcells)
    putStrLn (get_computer_state_string computerstate computermatchedcells)
    putStrLn ("***************************")
    putStrLn("****** Bingo stars earned: you = " ++ show playertotalbingo ++ " | Computer = " ++ show computertotalbingo ++ " ******")
    putStrLn ("***************************")
    putStrLn ("Here is the Bingo Card you played")
    putStrLn (getBingoCard)
    putStrLn ("================================")
    putStrLn ("p = play again | other key = exit game")

    line <- getLine
    if line == "p"
      then play start_state
      else exit


exit = putStrLn ("Bye, see you again!!!")

get_player_state_string state matchedcells = "Your numbers: " ++ show state ++ "\nYou've matched: " ++ show matchedcells

get_computer_state_string state matchedcells = "Computers's numbers: " ++ show state ++ "\nComputer's matched: " ++ show matchedcells













