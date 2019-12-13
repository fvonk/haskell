module Palindrome
      ( isPalindrome
      , preprocess
      ) where

import qualified Data.Text as T

-- import Data.Char (toLower, isSpace, isPunctuation)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.strip text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = (T.filter (/= ',') . T.filter (/= '.') . T.filter (/= '!')) text


toLowerCase :: T.Text -> T.Text
toLowerCase text = T.toLower text

preprocess :: T.Text -> T.Text
preprocess = stripPunctuation . stripWhiteSpace . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preprocess text
