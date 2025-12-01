parse :: String -> Int
parse ('L':xs) = -(read xs)
parse ('R':xs) = read xs

parseAll :: [String] -> [Int]
parseAll = map parse

ch :: Int -> Bool
ch = (==) 0 . (`mod` 100)

-- taken from net (AoC subreddit)
timePasses :: Int -> Int -> Int
timePasses a b = length $ filter ch (if a < b then [(a+1) .. b] else [(a-1), (a-2) .. b])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let fileLines = lines content
    let inputs = parseAll fileLines
    let startValue = 50
    let abspos = scanl (+) startValue inputs
    let part2 = sum $ zipWith timePasses abspos (tail abspos)
    print part2