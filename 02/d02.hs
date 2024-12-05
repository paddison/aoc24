-- d02.hs

main :: IO ()
main = do
  input <- readFile "dataAll"
  print $ countSafeReports $ parseInput input

parseInput :: String -> [[Int]]
parseInput input = [[read num | num <- words line] | line <- lines input]

isValidList :: [Int] -> Bool
isValidList (a : b : rest) = case compare a b of
  LT -> isAscending (a : b : rest)
  GT -> isDescending (a : b : rest)
  EQ -> False

isAscending :: [Int] -> Bool
isAscending [a] = True
isAscending (a : b : rest)
  | a < b && abs (a - b) <= 3 = isAscending (b : rest)
  | otherwise = False

isDescending :: [Int] -> Bool
isDescending [a] = True
isDescending (a : b : rest)
  | a > b && abs (a - b) <= 3 = isDescending (b : rest)
  | otherwise = False

countSafeReports :: [[Int]] -> Int
countSafeReports = foldr (\ x -> (+) (if isValidList x then 1 else 0)) 0

verify :: [[Int]] -> IO ()
verify [] = print "done"
verify (x : xs) = do
  print $ isValidList x
  print x
  verify xs
