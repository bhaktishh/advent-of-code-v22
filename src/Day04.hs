module Day04 
    ( part1, part2
    ) where

import Data.List.Split

oneLine :: String -> ((Int, Int), (Int, Int))
oneLine str = let a = splitOn "," str in
    let s1 = splitOn "-" (a !! 0)
        s2 = splitOn "-" (a !! 1) in
            ((read (s1 !! 0), read (s1 !! 1)), (read (s2 !! 0), read (s2 !! 1)))

check1 :: ((Int, Int), (Int, Int)) -> Bool
check1 ((s1a, s1b), (s2a, s2b)) = s1a <= s2a && s1b >= s2b || s1a >= s2a && s1b <= s2b

check2 :: ((Int, Int), (Int, Int)) -> Bool
check2 ((s1a, s1b), (s2a, s2b)) = not $ s1b < s2a || s2b < s1a


part1 :: String -> Int
part1 = length . filter check1 . map oneLine . lines

part2 :: String -> Int
part2 = length . filter check2 . map oneLine . lines