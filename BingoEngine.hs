-- CPSP 312 - 2020 : Haskell Project by Xindi Wei, Nawaratt Tonrungroj

module BingoEngine
  (getTotalBingo,
   getMatchedCells,
   allCondsString,
   getCondsStringByRow,
   getBingoCard
   ) where

import Data.Map (fromListWith, toList) 
import Data.List (intercalate, intersperse, transpose)
import Prelude hiding (Left, Right)


-- getTotalBingo returns the total number of Bingo
-- input: list of dice rolling result/selected numbers
getTotalBingo :: Num a => [Int] -> a
getTotalBingo dicerollresult = 
    let checkedResults = checkBingo (extractMatchedCondBool(checkCond allConds matchedCondWithIdx dicerollresult))
    in  foldr(\result count -> if result then count + 1 else count) 0 checkedResults

-- getMatchedCells returns all matched conditions/cells
-- input: list of dice rolling result/selected numbers
getMatchedCells :: [Int] -> [[Char]]
getMatchedCells [] = []
getMatchedCells dicerollresult = [allCondsString !! idx | idx <- (extractIndexOfMatchConds (checkCond allConds matchedCondWithIdx dicerollresult))]


allCondsString = 
    ["get '1' exactly once", "get odd number more than 4 times", "get even number at least once", 
    "get same number at least three times", "get multiples of 3 at least twice", "get sum of more than 20", 
    "get two sequential numbers at least twice", "get prime number more than twice", "get '6' more than once"]

getCondsStringByRow rowIdx = [allCondsString !! (((rowIdx - 1) * 3) + col) | col <- [0..2]]

allConds :: [[Int] -> Bool]
allConds = [check_condR1C1, check_condR1C2, check_condR1C3, check_condR2C1, check_condR2C2, check_condR2C3, check_condR3C1, check_condR3C2, check_condR3C3]

condR1C1 = "get '1' exactly once"
condR1C2 = "get odd number more than 4 times"
condR1C3 = "get even number at least once"
condR2C1 = "get same number at least three times"
condR2C2 = "get multiples of 3 at least twice"
condR2C3 = "get sum of more than 20"
condR3C1 = "get two sequential numbers at least twice"
condR3C2 = "get prime number more than twice"
condR3C3 = "get '6' more than once"

getBingoCard = tablefy [" ","BingoCard"," "] [[condR1C1,condR1C2,condR1C3],[condR2C1,condR2C2,condR2C3],[condR3C1,condR3C2,condR3C3]]

tablefy h rs
    | any (/= length h) (map length rs) = error "Tablefy.tablefy: Differences in length"
    | otherwise                         = table
    where
        table  = unlines $ insert' sep (header:rows)
        widths = map (maximum . map length) (transpose (h:rs))
        sep    = insert "+" $ map (flip replicate '-' . (+2)) widths
        header = mkRow Center h
        rows   = map (mkRow Left) rs
        mkRow a       = insert "|" . zipWith (mkCell a) widths
        mkCell a n xs = " " ++ pad a n ' ' xs ++ " "
insert :: [a] -> [[a]] -> [a]
insert x xs = intercalate x ([] : xs ++ [[]])

-- | Version of 'insert' that uses 'intersperse' instead of 'intercalate'.
--
--   >>> insert' "#" ["Alpha","Beta","Gamma"]
--   >>> ["#", "Alpha", "#", "Beta", "#", "Gamma", "#"]
insert' :: [a] -> [[a]] -> [[a]]
insert' x xs = intersperse x ([] : xs ++ [[]])

-- | Alignment is a simple sum type containing the different modes of padding.
data Alignment = Left | Right | Center deriving Eq

pad :: Alignment -> Int -> a -> [a] -> [a]
pad a n x xs
    | n < 1          = error "Tablefy.pad: Length must not be smaller than one"
    | n <= length xs = take n xs
    | a == Left      = xs ++ replicate (n - length xs) x
    | a == Right     = replicate (n - length xs) x ++ xs
    | a == Center    = let (space, extra) = quotRem (n - length xs) 2
                       in replicate space x ++ xs ++ replicate (space + extra) x
-- Check if getting '1' exactly once
-- input: list of dice rolling result/selected numbers
check_condR1C1 :: (Eq a, Num a) => [a] -> Bool
check_condR1C1 dicerollresult = 
    let count1 = foldr(\ roll count -> if roll == 1 then count + 1 else count) 0 dicerollresult in
    if count1 == 1 then True else False

-- check if getting odd number more than 4 times
-- input: list of dice rolling result/selected numbers
check_condR1C2 :: Integral a => [a] -> Bool
check_condR1C2 dicerollresult = 
    let countOdd = foldr(\ roll count -> if roll `mod` 2 /= 0 then count + 1 else count) 0 dicerollresult in
    if countOdd > 4 then True else False

-- check if getting even number at least once
-- input: list of dice rolling result/selected numbers
check_condR1C3 :: Integral a => [a] -> Bool
check_condR1C3 dicerollresult = 
    let countEven = foldr(\ roll count -> if roll `mod` 2 == 0 then count + 1 else count) 0 dicerollresult in
    if countEven >= 1 then True else False

-- check if getting same number at least three times
-- input: list of dice rolling result/selected numbers
check_condR2C1 :: Ord a => [a] -> Bool
check_condR2C1 dicerollresult =
    let getfrequencies = countunique dicerollresult in
    foldr(\ freq acc -> (snd freq >= 3) || acc) False getfrequencies
    
-- check if getting multiples of 3 at least twice
-- input: list of dice rolling result/selected numbers
check_condR2C2 :: Integral a => [a] -> Bool
check_condR2C2 dicerollresult =
    let countMul3 = foldr(\ roll count -> if roll `mod` 3 == 0 then count + 1 else count) 0 dicerollresult in
    if countMul3 >= 2 then True else False

-- check if getting sum of more than 20
-- input: list of dice rolling result/selected numbers
check_condR2C3 :: (Ord a, Num a) => [a] -> Bool
check_condR2C3 dicerollresult = 
    let sum = foldr(\ roll count -> count + roll) 0 dicerollresult in
    if sum > 20 then True else False

-- check if getting two sequential numbers at least twice
-- input: list of dice rolling result/selected numbers
check_condR3C1 :: (Eq a, Num a) => [a] -> Bool
check_condR3C1 dicerollresult = 
    let pair_adjacent_num = get_pair_adjacent_num dicerollresult in
    let countSeq = foldr(\ num count -> if (fst num == snd num - 1) then count + 1 else count) 0 pair_adjacent_num in 
    if countSeq >= 2 then True else False

--check if getting prime number more than twice
-- input: list of dice rolling result/selected numbers
check_condR3C2 :: (Eq a, Num a) => [a] -> Bool
check_condR3C2 dicerollresult =
    let countPrime = foldr(\ roll count -> if roll == 2 || roll == 3 || roll == 5 then count + 1 else count) 0 dicerollresult in
    if countPrime > 2 then True else False

-- check if getting '6' more than once
-- input: list of dice rolling result/selected numbers
check_condR3C3 :: (Eq a, Num a) => [a] -> Bool
check_condR3C3 dicerollresult = 
    let count6 = foldr(\ roll count -> if roll == 6 then count + 1 else count) 0 dicerollresult in
    if count6 > 1 then True else False


-- count frequencies of each unique number in the list, and return a list of (num, freq) tuple
-- input: list of dice rolling result/selected numbers
-- citation: https://stackoverflow.com/questions/10398698/haskell-counting-how-many-times-each-distinct-element-in-a-list-occurs
countunique :: (Ord a) => [a] -> [(a, Int)]
countunique [] = []
countunique dicerollresult = toList (fromListWith (+) [(x, 1) | x <- dicerollresult])

-- get a tuple of all adjacent numbers
-- input: list of dice rolling result/selected numbers
-- citation: https://stackoverflow.com/questions/43021548/sequential-numbers-in-a-list-haskell
get_pair_adjacent_num :: [b] -> [(b, b)]
get_pair_adjacent_num (h:t) = zip (h:t) t


-- matchedResultWithIdx contains matched results for all conditions/cells.
-- First element of the tuple is the matched result (True = matched, False = not matched)
-- Second element of the tuple is the index of the condition in allConds
matchedCondWithIdx = [(False,0), (False,1), (False,2), (False,3), (False,4), (False,5), (False,6), (False,7), (False,8)]

-- checkBingo returns a list of boolean containing bingo result for each row, column and diagonal (total 8 possible bingo)
-- input: list of booleans indicating the matched result for each condition
checkBingo :: [Bool] -> [Bool]
checkBingo matchedCondBool = (checkRow matchedCondBool 1 1) : (checkRow matchedCondBool 2 1) : (checkRow matchedCondBool 3 1) : 
                       (checkCol matchedCondBool 1 1) : (checkCol matchedCondBool 1 2) : (checkCol matchedCondBool 1 3) : 
                       (checkDiagonalDown matchedCondBool 1 1) : (checkDiagonalUp matchedCondBool 3 1) : []

-- checkCond checks whether the dice rolls/picked numbers match each condition
-- input: allConds for (condh:condt), matchedCondWithIdx for (matchh:matcht), list of dice rolling result/selected numbers
checkCond :: [[a] -> Bool] -> [(Bool, b)] -> [a] -> [(Bool, b)]
checkCond [] _ _ = []
checkCond _ [] _ = []
checkCond _ _ [] = []
checkCond (condh:condt) (matchh:matcht) dicerollresult = ((fst matchh || condh dicerollresult), snd matchh) : (checkCond condt matcht dicerollresult)

-- extractMatchedCondBool returns a list of booleans indicating the matched result for each condition
-- input: matchedCondWithIdx
extractMatchedCondBool :: Foldable t => t (a, b) -> [a]
extractMatchedCondBool matchedCondWithIdx = foldr(\tuple booleans -> fst tuple : booleans) [] matchedCondWithIdx

-- extractIndexOfMatchConds returns a list of indexes for matched conditions
-- input: matchedCondWithIdx
extractIndexOfMatchConds :: [(Bool, a)] -> [a]
extractIndexOfMatchConds matchedCondWithIdx = [snd tuple | tuple <- matchedCondWithIdx, fst tuple]

-- getCellByIdx returns string of row number and column number
-- input: an index of the condition in allConds
getCellByIdx :: (Show a, Integral a) => a -> [Char]
getCellByIdx idx = "row" ++ show ((idx `div` 3) + 1) ++ " col" ++ show ((idx `mod` 3) + 1)

-- translateCellToIdx returns an index of the condition in allConds
-- input: row index, column index
translateCellToIdx :: Num a => a -> a -> a
translateCellToIdx rowIdx colIdx = ((rowIdx-1) * 3) + (colIdx-1)

-- check if all conditions/cells in the row are matched
-- input: list of booleans indicating the matched result for each condition
checkRow :: [Bool] -> Int -> Int -> Bool
checkRow _ _ 4 = True
checkRow matchedCondBool rowIdx colIdx = (matchedCondBool !! (translateCellToIdx rowIdx colIdx)) && (checkRow matchedCondBool rowIdx (colIdx+1))

-- check if all conditions/cells in the column are matched
-- input: list of booleans indicating the matched result for each condition
checkCol :: [Bool] -> Int -> Int -> Bool
checkCol _ 4 _ = True
checkCol matchedCondBool rowIdx colIdx = (matchedCondBool !! (translateCellToIdx rowIdx colIdx)) && (checkCol matchedCondBool (rowIdx+1) colIdx)

-- check if all conditions/cells in the diagonal (row1 col1, row2 col2, row3, col3) are matched
-- input: list of booleans indicating the matched result for each condition
checkDiagonalDown :: [Bool] -> Int -> Int -> Bool
checkDiagonalDown _ 4 4 = True
checkDiagonalDown matchedCondBool rowIdx colIdx = (matchedCondBool !! (translateCellToIdx rowIdx colIdx)) && (checkDiagonalDown matchedCondBool (rowIdx+1) (colIdx+1))

-- check if all conditions/cells in the diagonal (row3 col1, row2 col2, row1, col3) are matched
-- input: list of booleans indicating the matched result for each condition
checkDiagonalUp :: [Bool] -> Int -> Int -> Bool
checkDiagonalUp _ 0 4 = True
checkDiagonalUp matchedCondBool rowIdx colIdx = (matchedCondBool !! (translateCellToIdx rowIdx colIdx)) && (checkDiagonalUp matchedCondBool (rowIdx-1) (colIdx+1))

