module Lib where

import Data.Text as T
import Data.Char (isPunctuation)

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where
    cleanText = preprocess text

toLowerCase :: T.Text -> T.Text
toLowerCase text = T.toLower text

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.strip

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

--

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . toLowerCase . stripPunctuation
