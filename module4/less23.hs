{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T


myLines :: T.Text -> [T.Text]
myLines t = T.splitOn "\n" t

myUnlines :: [T.Text] -> T.Text
myUnlines t = T.intercalate "\n" t


aWord :: T.Text
aWord = "asdf 1323 123\n234213\nasdf2ff"



main :: IO ()
main = do
  print aWord
