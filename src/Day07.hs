module Day07 
    ( part1, part2
    ) where

import Data.List.Split ( splitOn )
import Data.List ( nub , isPrefixOf , stripPrefix , findIndex )
import Parsing
import Text.Megaparsec
import Text.Megaparsec.Char

-- um in progress

-- data Dir = Dir String [Dir] | File String Int deriving (Ord, Eq, Read, Show)
-- data Cd = Cd Dir deriving (Ord, Eq, Read, Show)

-- dirName :: Dir -> String
-- dirName (Dir n ls) = n
-- dirName (File n sz) = n

-- cdDown :: (Dir, [Cd]) -> String -> (Dir, [Cd])
-- cdDown (d@(Dir n ls), cdls) str = case findIndex (\x -> dirName x == str) ls of
--     Nothing -> (d, (Cd d : cdls))
--     Just i -> (ls !! i, (Cd d : cdls))
-- cdDown x _ = x

-- cdUp :: (Dir, [Cd]) -> (Dir, [Cd])
-- cdUp (d@(Dir n ls), Cd updir:cds) = (updir, Cd d : cds)
-- cdUp x = x

-- createSubs :: ([Dir], [String]) -> ([Dir], [String])
-- createSubs (dirs, (c:cs)) | isPrefixOf "dir " c = createSubs ((Dir (drop 4 c) []) : dirs, cs)
-- createSubs (dirs, (c:cs)) | isPrefixOf "$ " c = (dirs, (c:cs))
-- createSubs (dirs, (c:cs)) = let file = splitOn " " c in
--     createSubs ((File (file !! 1) (read (file !! 0))) : dirs, cs)
-- createSubs x = x

-- goUp :: (Dir, [Cd]) -> Dir
-- goUp (dir, ((Cd d@(Dir n ls))):cs) = goUp (d, cs)
-- goUp (dir, _) = dir

-- modifyCmd :: Dir -> Cd -> Cd
-- modifyCmd d (Cd d') = if dirName d == dirName d' then (Cd d) else (Cd d')

-- runCmds :: [String] -> (Dir, [Cd]) -> (Dir, [Cd])
-- runCmds ("$ cd ..":rest) cur = runCmds rest $ cdUp cur
-- runCmds (str:rest) cur | isPrefixOf "$ cd " str = runCmds rest $ cdDown cur (drop 5 str)
-- runCmds ("$ ls":rest) (Dir n ls, cmds) = let (dirs, rest') = createSubs ([], rest) in
--     let dir' = Dir n (dirs ++ ls) in
--         runCmds rest' (dir', (map (modifyCmd dir') cmds))
-- runCmds _ x = x

-- totalSize :: Dir -> Int
-- totalSize (File _ i) = i
-- totalSize (Dir _ ls) = (sum . map totalSize) ls

-- allSizes :: Dir -> Int
-- allSizes (File _ _) = 0
-- allSizes d@(Dir n ls) = let size = totalSize d in 
--     (if (size < 100000) then size else 0) + sum (map allSizes ls)

-- part1 :: String -> Dir
-- part1 = goUp . flip runCmds ((Dir "/" []), []) . tail . lines

part1 :: String -> Int
part1 str = 0

part2 :: String -> Int
part2 str = 0
