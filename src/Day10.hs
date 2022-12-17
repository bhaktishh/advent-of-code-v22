module Day10 
    ( part1, part2
    ) where

import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List.Split ( chunksOf )

data Instruction = Addx Int | Noop deriving (Read, Show)

parseInstr :: Parser [Instruction]
parseInstr = sepBy (Addx <$> ((string "addx ") *> integer) 
             <|> (\_ -> Noop) <$> (string "noop")) newline

updateC :: (Int, Int) -> [Instruction] -> [(Int, Int)]
updateC (cy, x) (Noop:insts) = (cy, x) : updateC (cy + 1, x) insts
updateC (cy, x) ((Addx i): insts) = (cy, x) : (cy + 1, x) : updateC (cy + 2, x + i) insts
updateC _ [] = []

lit :: (Int, Int) -> Bool
lit (cy, x) = (abs ((cy `mod` 40) - 1 - x)) < 2

sketch :: (Int, Int) -> Char
sketch l = if (lit l) then '#' else '.'

part1 :: String -> Int
part1 = sum . map (uncurry (*)) . take 6 . filter (\x -> fst x `mod` 40 == 20) . updateC (1,1) . runP parseInstr []


part2 :: String -> String
part2 = unlines . chunksOf 40 . map sketch . updateC (1,1) . runP parseInstr []