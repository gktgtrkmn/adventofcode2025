{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (union, sortBy)
import System.Environment

type Range = (Integer, Integer)

parseRanges :: [T.Text] -> [Range]
parseRanges = map parseRange
 where
    parseRange :: T.Text -> Range
    parseRange t =
     let [a, b] = T.splitOn "-" t
     in (read (T.unpack a), read (T.unpack b))

inRange :: Integer -> Range -> Bool
inRange x (a, b) = x >= a && x <= b

part1 :: [Range] -> [Integer] -> Integer
part1 ranges ids = toInteger . length  $ filter inAnyRange ids
 where
    inAnyRange x = any (inRange x) ranges

go :: [Range] -> Range -> [Range]
go acc@( (a1, b1) : rest ) (a2, b2)
 | a2 <= b1 + 1 = (a1, max b1 b2) : rest
 | otherwise = (a2, b2) : acc

mergeOverlaps :: [Range] -> [Range]
mergeOverlaps [] = []
mergeOverlaps xs = foldl go [head sorted] (tail sorted)
 where 
    sorted = sortBy (\(a1, _) (a2, _) -> compare a1 a2) xs

part2 :: [Range]  -> Integer
part2 rs = sum $ map (\(a,b) -> b-a+1) (mergeOverlaps rs)  

main :: IO ()
main = do
 args <- getArgs
 let filePath = head args
 content <- TIO.readFile filePath
 let [rangeLines, idLines] = T.splitOn "\n\n" content
 let ranges = parseRanges (T.lines rangeLines)
 let ids = map (read @Integer . T.unpack) (T.lines idLines)
 print $ part1 ranges ids
 print $ part2 ranges


-- extremely inefficient part1 functions used before

rangeToLis :: Range -> [Integer]
rangeToLis r = [fst r .. snd r]

mergeRanges :: [Range] -> [Integer]
mergeRanges = foldl (\acc r -> acc `union` rangeToLis r) []