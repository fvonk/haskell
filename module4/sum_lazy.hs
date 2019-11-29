import System.Environment
import Control.Monad
import Data.List.Split

main :: IO ()
main = do
  userInput <- getContents
  let ints = toInts userInput
  print (sum (map (^2) ints))


toInts :: String -> [Int]
toInts = map read . lines
