import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Char (digitToInt)

maxWithIndex :: Ord a => [a] -> (Int, a)
maxWithIndex = maximumBy (comparing snd) . zip [0..]

compareAlgos :: (Eq b) => (a -> b) -> (a -> b) -> [a] -> [(a, b, b)]
compareAlgos f g xs = 
    [(x, fx, gx)
    | x <- xs
    , let fx = f x
    , let gx = g x
    , fx /= gx
    ]
 
-- this does not work with results a*11, dont know why (smth with drop?)
part1'' :: [Int] -> Int
part1'' x
 | length x < 2 = 0
 | otherwise = read (show val1 ++ show val2)
 where 
    firstLookup = init x
    (val1index, val1) = maxWithIndex firstLookup
    secondLookup = drop (val1index + 1) x
    (_, val2) = maxWithIndex secondLookup

part1' :: [Int] -> Int
part1' xs = go xs (-1) 0
 where 
    go [] _ best = best
    go (d:ds) maxLeft best =
        let best' = if maxLeft >= 0 then max best (10*maxLeft + d) else best
            maxLeft' = max maxLeft d
        in go ds maxLeft' best'

part1 :: [Int] -> Int 
part1 xs
 | length xs < 2 = 0
 | otherwise = maximum [10*a + b | (i, a) <- zip [0..] xs, (j, b) <- zip [0..] xs, j > i]

part2 :: Int -> [Int] -> [Int]
part2 k xs = take k $ reverse $ go xs [] (length xs - k)
 where
    go [] stack _ = reverse stack
    go (d:ds) stack dropsLeft
     | dropsLeft > 0
     , not (null stack)
     , last stack < d
     = go (d:ds) (init stack) (dropsLeft - 1)
     | otherwise = go ds (stack ++ [d]) dropsLeft

splitLine :: String -> [Int]
splitLine = map digitToInt

listToInt :: [Int] -> Integer
listToInt xs = read $ concatMap show xs

main :: IO ()
main = do
    content <- readFile "input.txt"
    let fileLines = lines content
    let linesAsInts = map splitLine $ filter (not . null) fileLines
    let result2 = sum $ map (listToInt . part2 12) linesAsInts
    print $ show result2
