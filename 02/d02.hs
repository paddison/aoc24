-- d02.hs

main :: IO ()
main = do
  input <- readFile "../data/02"
  print $ countSafeReports $ parseInput input
  print $ countSafeReportsP2 $ parseInput input

parseInput :: String -> [[Integer]]
parseInput input =[map read (words line) | line <- lines input]

isValidList :: [Integer] -> Bool
isValidList (a : b : rest) = case compare a b of
  LT -> isAscending (a : b : rest)
  GT -> isDescending (a : b : rest)
  EQ -> False

isValidListTolerance :: [Integer] -> Int -> Int -> Bool
isValidListTolerance  xs l i
  | i == l = False
  | otherwise = let ys = (take i xs ++ drop (i + 1) xs) 
                in isValidList ys || isValidListTolerance xs l (i + 1)

isAscending :: [Integer] -> Bool
isAscending [a] = True
isAscending (a : b : rest)
  | a < b && abs (a - b) <= 3 = isAscending (b : rest)
  | otherwise = False

isDescending :: [Integer] -> Bool
isDescending [a] = True
isDescending (a : b : rest)
  | a > b && abs (a - b) <= 3 = isDescending (b : rest)
  | otherwise = False

countSafeReports :: [[Integer]] -> Integer
countSafeReports = foldr (\ x -> (+) (if isValidList x then 1 else 0)) 0

countSafeReportsP2 :: [[Integer]] -> Integer
countSafeReportsP2 = foldr (\ x -> (+) (if isValidList x || isValidListTolerance x (length x) 0 then 1 else 0)) 0


