import Data.List
import Data.List.Split

birthdayCakeCandles :: [Int] -> Int
birthdayCakeCandles candles = (length . filter (== mxm)) candles
  where
    mxm = maximum candles

main :: IO ()
main = interact $ show . birthdayCakeCandles . map read . tail . words
