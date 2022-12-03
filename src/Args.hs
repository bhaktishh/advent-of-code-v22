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
        (1,1) -> print $ Day01.part1 input
        (1,2) -> print $ Day01.part2 input
        (2,1) -> print $ Day02.part1 input
        (2,2) -> print $ Day02.part2 input
        (3,1) -> print $ Day03.part1 input
        (3,2) -> print $ Day03.part2 input
        _     -> print $ "no matching day/part"
            

    
    
