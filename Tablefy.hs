module Tablefy
  (tablefy) 
  where

import Data.List (intercalate, intersperse, transpose)
import Prelude hiding (Left, Right)

-- Print tables
-- citation: https://codereview.stackexchange.com/questions/171992/pretty-printed-tables-in-haskell
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
