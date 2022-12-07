module Day06 
    ( part1, part2
    ) where

import Data.List.Split ( divvy )
import Data.List (nub, findIndex )

find :: Int -> String -> Int
find n str =  case (findIndex (\x -> length x == n) . map nub . divvy n 1) str of
    Just x -> x + n
    Nothing -> 0

part1 :: String -> Int
part1 = find 4

part2 :: String -> Int
part2 = find 14