import Data.List
import Data.List.Split

solve :: Int -> String
solve n = unlines $ (map sharpLine . tail . pairs) n

pairs :: Int -> [(Int, Int)]
pairs n = zip [n, n - 1 .. 0] [0 .. n]

sharpLine :: (Int,Int) -> String 
sharpLine (em,sh) = (take em (repeat ' ')) <> (take sh (repeat '#'))

main = interact $ solve . read
