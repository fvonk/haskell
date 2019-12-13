module Main where

import Lib
import Palindrome
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  TIO.putStrLn "Enter the word, to know is it a palindrome."
  text <- TIO.getLine
  let response = if isPalindrome text
                 then "Yes, it is"
                 else "No, it's not"
  TIO.putStrLn response
