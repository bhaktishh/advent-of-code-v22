module Day06 
    ( part1, part2
    ) where

import Data.List.Split ( divvy )
import Data.List (nub, findIndex )

part1 :: String -> Int
part1 str = case (findIndex (\x -> length x == 4) . map nub . divvy 4 1) str of
    Just x -> x + 4
    Nothing -> 0

part2 :: String -> Int
part2 str = case (findIndex (\x -> length x == 14) . map nub . divvy 14 1) str of
    Just x -> x + 14
    Nothing -> 0