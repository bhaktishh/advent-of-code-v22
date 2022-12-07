module Day05 
    ( part1, part2
    ) where

import Data.List.Split ( chunksOf )
import Data.List ( transpose )
import Data.Void ( Void ) 
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char ( isUpper, isDigit )

type Parser = Parsec Void String

integer :: Parser Int
integer = read <$> some numberChar

parseCrate :: Parser [Char]
parseCrate = many (try (string "[" *> satisfy isUpper <* string "] ") 
            <|> try (string "    " *> pure '-'))

parseMove :: Parser (Int, Int, Int)
parseMove = (,,) <$> (string "move " *> integer) <*> (string " from " *> integer)
            <*> (string " to " *> integer)

parseAll :: Parser ([[Char]], [(Int, Int, Int)])
parseAll = (,) <$> (map (filter (/= '-')) . transpose <$> sepBy parseCrate newline 
                <* manyTill (satisfy isDigit <|> char ' ') newline) 
                <*> (newline *> sepBy parseMove newline)

move :: Bool -> [[Char]] -> (Int, Int, Int) -> [[Char]]
move part stacks (n, f, t) = let f' = drop n (stacks !! (f-1))
                                 t' = (if part then reverse . take n else take n) (stacks !! (f-1)) 
                                    ++ (stacks !! (t-1)) in
                                let stacks' = (take (f-1) stacks) ++ [f'] ++ (drop f stacks) in
                                    (take (t-1) stacks') ++ [t'] ++ (drop t stacks')

part1 :: String -> String
part1 str = case (parse parseAll "" str) of
    Right (stacks, moves) -> map head $ foldl (move True) stacks moves
    Left _ -> ""

part2 :: String -> String
part2 str = case (parse parseAll "" str) of
    Right (stacks, moves) -> map head $ foldl (move False) stacks moves
    Left _ -> ""