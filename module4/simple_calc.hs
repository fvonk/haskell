import System.Environment
import Control.Monad
import Data.List.Split

main :: IO ()
main = do
  userInput <- getContents
  let values = lines userInput
  print (map calc values)


calc :: String -> Int
calc (val1:'+':val2:rest) = read [val1] + read [val2]
calc (val1:'*':val2:rest) = read [val1] * read [val2]

toInts :: String -> [Int]
toInts = map read . lines
