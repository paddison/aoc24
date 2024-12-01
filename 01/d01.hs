-- d01.hs
import Data.List

main :: IO ()
main = do
  contents <- readFile "data"
  let input = parseFile contents
  let first = uncurry calcDiffs (sortInput input)
  let second = uncurry countOccurences input
  print first
  print second

countOccurences :: [Int] -> [Int] -> Int
countOccurences [] _ = 0
countOccurences (a : as) bs = a * length [b | b <- bs, b == a] + countOccurences as bs

absDiff :: Int -> Int -> Int
absDiff a b = if a > b then a - b else b - a

calcDiffs :: [Int] -> [Int] -> Int
calcDiffs [] [] = 0
calcDiffs (a : as) (b : bs) = absDiff a b + calcDiffs as bs

sortInput :: ([Int], [Int]) -> ([Int], [Int])
sortInput (a, b) = (sort a, sort b)

parseFile :: String -> ([Int], [Int])
parseFile contents = unzip [makeTuple ([read j | j <- words i]) | i <- lines contents]

makeTuple :: [a] -> (a, a)
makeTuple [a, b] = (a, b)
