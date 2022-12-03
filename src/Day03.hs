module Day03 
    ( part1, part2
    ) where

import Data.Char ( isLower )
import Data.List.Split ( chunksOf )

priority :: Char -> Int
priority x = case isLower x of
    True  -> fromEnum x - 96
    False -> fromEnum x - 64 + 26

rep1 :: (String, String) -> Char
rep1 (f, s) = head $ filter ((flip elem) s) f

rep2 :: [String] -> Char
rep2 (c0:c1:c2:_) = head $ filter (\x -> elem x c1 && elem x c2) c0
rep2 _ = ' '

part1 :: String -> Int
part1 str = sum $ map priority $ map rep1 $ map (\x -> splitAt (div (length x) 2) x) $ lines str

part2 :: String -> Int
part2 str = sum $ map priority $ map rep2 $ chunksOf 3 . lines $ str