module Day08 
    ( part1, part2
    ) where

import Data.List ( transpose )
import Data.Char ( digitToInt )

genTuples :: [[Int]] -> [(Int, Int)]
genTuples board = [(r, c) | r <- [0..length board - 1], c <- [0.. length (board !! 0) - 1]]

isVisible_ :: [Int] -> Int -> Bool
isVisible_ lst ind = let lt = (\x -> lst !! x < lst !! ind) in
    and (map lt [0..ind-1]) || and (map lt [ind+1..length lst - 1])

isVisible :: [[Int]] -> (Int, Int) -> Bool
isVisible b (r,c) = (isVisible_ (b !! r) c) || (isVisible_ ((transpose b) !! c) r)

allIsVisible :: [[Int]] -> Int
allIsVisible board = length $ filter (== True) $ map (isVisible board) (genTuples board)

length' :: [Int] -> Int -> Int
length' lst l = if length lst == l then length lst else length lst + 1 

howManyVisible :: [Int] -> Int -> Int
howManyVisible lst ind = let lt = (\x -> lst !! x < lst  !! ind) in
    length' (takeWhile lt (reverse [0..ind-1])) ind * length' (takeWhile lt [ind+1..length lst - 1]) (length lst - ind - 1)

scenicScore :: [[Int]] -> (Int, Int) -> Int
scenicScore b (r,c) = (howManyVisible (b !! r) c) * (howManyVisible ((transpose b) !! c) r)

maxScenic :: [[Int]] -> Int
maxScenic board = maximum $ map (scenicScore board) (genTuples board)

part1 :: String -> Int
part1 = allIsVisible . map (map digitToInt) . lines

part2 :: String -> Int
part2 = maxScenic . map (map digitToInt) . lines