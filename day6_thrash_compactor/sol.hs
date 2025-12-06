{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Operator = Add | Mul deriving Show

toOperator :: Char -> Operator
toOperator '+' = Add
toOperator '*' = Mul
toOperator _ = error "unknown operator"

applyOp :: Operator -> (Int -> Int -> Int)
applyOp Add = (+)
applyOp Mul = (*)

trimOps :: T.Text -> T.Text
trimOps = T.filter (not . (`elem` [' ', '\t']))

trimRow :: T.Text -> T.Text
trimRow = T.strip

foldOp :: Operator -> [Int] -> Int
foldOp op = foldl1 (applyOp op)

part1 :: [Operator] -> [[Int]] -> Int
part1 ops rows  = sum $ zipWith foldOp ops (transpose rows)

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    content <- T.readFile filePath
    let fileLines = T.lines content
    let operatorLine : numberLines = reverse fileLines
    let operators = map toOperator ((T.unpack . trimOps) operatorLine)
    let rows = [ map (read . T.unpack) (T.words (trimRow line)) | line <- numberLines ]  
    print $ part1 operators rows