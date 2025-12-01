import Text.Read (readMaybe)
import Control.Applicative (liftA2)

parse :: String -> Maybe Int
parse ('L':xs) = fmap negate (readMaybe xs)
parse ('R':xs) = readMaybe xs
parse _ = Nothing

parseAll :: [String] -> [Maybe Int]
parseAll = map parse

process :: Maybe Int -> [Maybe Int] -> [Maybe Int]
process init list = tail (scanl (liftA2 addMod) init list)

addMod :: Int -> Int -> Int
addMod x y = (x + y) `mod` 100

countZeros :: [Maybe Int] -> Int
countZeros list = length (filter (== Just 0) list)

main :: IO ()
main = do
    content <- readFile "input.txt"
    let fileLines = lines content
    let inputs = parseAll fileLines
    let startValue = Just 50
    let result = countZeros (process startValue inputs)
    print result