module Day02 
    ( part1, part2
    ) where

points_p1 :: String -> Int
points_p1 str = case (str !! 0, str !! 2) of
    ('A', 'X') -> 1 + 3
    ('A', 'Y') -> 2 + 6
    ('A', 'Z') -> 3 + 0
    ('B', 'X') -> 1 + 0
    ('B', 'Y') -> 2 + 3
    ('B', 'Z') -> 3 + 6
    ('C', 'X') -> 1 + 6
    ('C', 'Y') -> 2 + 0
    ('C', 'Z') -> 3 + 3
    (_ , _)    -> 0

points_p2 :: String -> Int
points_p2 str = case (str !! 0, str !! 2) of
    ('A', 'X') -> 3 + 0
    ('A', 'Y') -> 1 + 3
    ('A', 'Z') -> 2 + 6
    ('B', 'X') -> 1 + 0
    ('B', 'Y') -> 2 + 3
    ('B', 'Z') -> 3 + 6
    ('C', 'X') -> 2 + 0
    ('C', 'Y') -> 3 + 3
    ('C', 'Z') -> 1 + 6
    (_ , _)    -> 0

part1 :: String -> Int
part1 = foldl (+) 0 . map points_p1 . lines

part2 :: String -> Int
part2 = foldl (+) 0 . map points_p2 . lines