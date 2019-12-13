module Main where

import qualified Data.Text.IO as TIO
-- import qualified Data.Text.IO as TIO
import Palindrome

-- isPalindrome :: String -> Bool
-- isPalindrome text = text == reverse text

main :: IO ()
main = do
  putStrLn "Enter the word, to know is it a palindrome."
  text <- TIO.getLine
  let response = if isPalindrome text
                 then "Yes, it is"
                 else "No, it'not"
  putStrLn response
