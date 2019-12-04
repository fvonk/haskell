import Data.List
import Data.List.Split

solve :: [Int] -> [Int]
solve n = (elemS maximum):[elemS minimum]
  where 
    elemS f = sum $ removeElem (f n) n

-- only once
removeElem :: Int -> [Int] -> [Int]
removeElem _ [] = []
removeElem a (x:xs)
  | a == x = xs
  | otherwise = x : removeElem a xs

main = interact $ unwords . map show . solve . map read . words

