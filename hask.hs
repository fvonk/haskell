import Data.List

diagonalDifference :: [[Int]] -> Int
diagonalDifference input = abs $ (\(a, b) -> a - b) summa
  where
    tuples = map handleLine (zip [0..] input)
    summa = foldl (\(a,b) (i, i2) -> (a+i, b+i2)) (0,0) tuples

handleLine :: (Int, [Int]) -> (Int, Int)
handleLine (n, arr) = (((!!) arr n), ((!!) arr ((length arr) - n - 1)))

main = interact $ show . diagonalDifference . map (map (read::String -> Int)) . (map words . tail . lines)
