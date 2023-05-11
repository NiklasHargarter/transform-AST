module UnrelatedMethods where

lengthInt :: Int -> Int
lengthInt value = length (show value)

firsts :: [[Int]] -> [Int]
firsts ((y : ys) : xs) = y : firsts xs
firsts _ = []

firsts' :: [[Int]] -> [Int]
firsts' list = foldl (\y (x : xs) -> x : y) [] list

firstsMaxWidth :: [[Int]] -> Int
firstsMaxWidth ((y : _) : xs) = max (lengthInt y) (firstsMaxWidth xs)
firstsMaxWidth _ = 0

removeFirsts :: [[Int]] -> [[Int]]
removeFirsts ((y : ys) : xs) = ys : removeFirsts xs
removeFirsts _ = []

sort :: [[Int]] -> [[Int]] -> [[Int]]
sort list@((y : ys) : xs) akk = sort (removeFirsts list) (akk ++ [firsts list])
sort _ akk = akk

gridWidth :: [[Int]] -> [Int] -> [Int]
gridWidth list@((y : ys) : xs) akk = gridWidth (removeFirsts list) (akk ++ [firstsMaxWidth list])
gridWidth _ akk = akk

allPairs :: [a] -> [b] -> [(a, b)]
allPairs list1 list2 =
  case list1 of
    [] ->
      []
    x : xs ->
      map (\y -> (x, y)) list2 ++ allPairs xs list2

allPairs2 :: [a] -> [b] -> [(a, b)]
allPairs2 list1 list2 = concatMap (\x -> map (\y -> (x, y)) list2) list1