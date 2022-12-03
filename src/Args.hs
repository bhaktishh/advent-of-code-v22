module Args
    (   main'
    ) where

import qualified Day01 as Day01
import qualified Day02 as Day02
import qualified Day03 as Day03
import qualified Day04 as Day04
import qualified Day05 as Day05
import qualified Day06 as Day06
import qualified Day07 as Day07
import qualified Day08 as Day08
import qualified Day09 as Day09
import qualified Day10 as Day10
import qualified Day11 as Day11
import qualified Day12 as Day12
import qualified Day13 as Day13
import qualified Day14 as Day14
import qualified Day15 as Day15
import qualified Day16 as Day16
import qualified Day17 as Day17
import qualified Day18 as Day18
import qualified Day19 as Day19
import qualified Day20 as Day20
import qualified Day21 as Day21
import qualified Day22 as Day22
import qualified Day23 as Day23
import qualified Day24 as Day24
import qualified Day25 as Day25

import qualified Data.Text as T
import GHC.Generics (Generic)
import Options.Generic (
    ParseRecord,
    unwrapRecord,
    Unwrapped,
    Wrapped,
    type (:::),
    type (<?>),
    type (<#>),
    )

data CmdArgs w = Args {
    day  :: w ::: Int <?> "day of solution, 1-25" <#> "d",
    part :: w ::: Int <?> "part of solution, 1-2" <#> "p"
} deriving (Generic)

type UWArgs = CmdArgs Unwrapped

instance ParseRecord (CmdArgs Wrapped)

deriving instance Show (CmdArgs Unwrapped)

getFile :: UWArgs -> IO String
getFile Args {day} = readFile $ "./inputs/day" <> fday <> ".txt" where
    fday = case (length $ show day) of
        1 -> "0" <> show day
        _ -> show day


main' :: IO ()
main' = do
    (x :: UWArgs) <- unwrapRecord $ T.pack "running advent of code solutions!"
    input <- getFile x
    case (day x, part x) of
        (1,1)  -> print $ Day01.part1 input
        (1,2)  -> print $ Day01.part2 input
        (2,1)  -> print $ Day02.part1 input
        (2,2)  -> print $ Day02.part2 input
        (3,1)  -> print $ Day03.part1 input
        (3,2)  -> print $ Day03.part2 input
        (4,1)  -> print $ Day04.part1 input
        (4,2)  -> print $ Day04.part2 input
        (5,1)  -> print $ Day05.part1 input
        (5,2)  -> print $ Day05.part2 input
        (6,1)  -> print $ Day06.part1 input
        (6,2)  -> print $ Day06.part2 input
        (7,1)  -> print $ Day07.part1 input
        (7,2)  -> print $ Day07.part2 input
        (8,1)  -> print $ Day08.part1 input
        (8,2)  -> print $ Day08.part2 input
        (9,1)  -> print $ Day09.part1 input
        (9,2)  -> print $ Day09.part2 input
        (10,1) -> print $ Day10.part1 input
        (10,2) -> print $ Day10.part2 input
        (11,1) -> print $ Day11.part1 input
        (11,2) -> print $ Day11.part2 input
        (12,1) -> print $ Day12.part1 input
        (12,2) -> print $ Day12.part2 input
        (13,1) -> print $ Day13.part1 input
        (13,2) -> print $ Day13.part2 input
        (14,1) -> print $ Day14.part1 input
        (14,2) -> print $ Day14.part2 input
        (15,1) -> print $ Day15.part1 input
        (15,2) -> print $ Day15.part2 input
        (16,1) -> print $ Day16.part1 input
        (16,2) -> print $ Day16.part2 input
        (17,1) -> print $ Day03.part1 input
        (17,2) -> print $ Day03.part2 input
        (18,1) -> print $ Day18.part1 input
        (18,2) -> print $ Day18.part2 input
        (19,1) -> print $ Day19.part1 input
        (19,2) -> print $ Day19.part2 input
        (20,1) -> print $ Day20.part1 input
        (20,2) -> print $ Day20.part2 input
        (21,1) -> print $ Day21.part1 input
        (21,2) -> print $ Day21.part2 input
        (22,1) -> print $ Day22.part1 input
        (22,2) -> print $ Day22.part2 input
        (23,1) -> print $ Day23.part1 input
        (23,2) -> print $ Day23.part2 input
        (24,1) -> print $ Day24.part1 input
        (24,2) -> print $ Day24.part2 input
        (25,1) -> print $ Day25.part1 input
        (25,2) -> print $ Day25.part2 input
        _      -> print $ "no matching day/part"
            

    
    
