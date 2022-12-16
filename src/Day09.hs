module Day09 
    ( part1, part2
    ) where

import Data.List ( nub, mapAccumL, scanl )

data Dir = L | R | U | D

type Point = (Int, Int)

move :: Dir -> Point -> Point
move L (r,c) = (r-1, c)
move R (r,c) = (r+1, c)
move U (r,c) = (r, c+1)
move D (r,c) = (r, c-1)

moveTail :: Point -> Point -> Point
moveTail (hr, hc) (tr, tc) | abs (hr - tr) < 2 && abs (hc - tc) < 2 = (tr,tc)
moveTail (hr, hc) (tr, tc) = (tr + signum (hr - tr), tc + signum (hc - tc))

visit :: (Dir, Int) -> Point -> Point -> (Point, Point, [Point])
visit (d, i) h t = let heads = (take (i + 1) (iterate (move d) h)) in
    let rs = mapAccumL (\s a -> (moveTail a s, moveTail a s)) t heads in
    (last heads, fst rs, snd rs)

visits :: Point -> Point -> [(Dir, Int)] -> [Point]
visits h t (mv:mvs) = let (h', t', tlst) = visit mv h t in
    tlst ++ visits h' t' mvs
visits _ _ [] = []

visitN :: Point -> [Point] -> [Point]
visitN cur prev = scanl (\s a -> moveTail a s) cur prev

fakeParse :: [[String]] -> [(Dir, Int)]
fakeParse (c:cs) = (dirToDir (c !! 0), read (c !! 1)) : fakeParse cs
fakeParse [] = []

dirToDir :: String -> Dir
dirToDir "L" = L
dirToDir "R" = R
dirToDir "U" = U
dirToDir "D" = D
dirToDir _   = L

part1 :: String -> Int
part1 = length . nub . visits (0,0) (0,0) . fakeParse . map words . lines

part2 :: String -> Int
part2 =  length . nub . flip (!!) 8 . iterate (visitN (0,0)) . visits (0,0) (0,0) . fakeParse . map words . lines