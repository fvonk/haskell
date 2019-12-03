{-# LANGUAGE OverloadedStrings #-}
import System.Random
import System.Environment
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  -- imageFileText <- B.readFile fileName
  -- let charCount = B.length imageFileText
  -- let bytesCount = (T.length . E.decodeUtf8) imageFileText
  imageFileText <- TIO.readFile fileName
  let charCount = getCounts imageFileText
  let bytesCount = (BC.length . E.encodeUtf8) imageFileText
  putStrLn ("Done!\nchars: " <> (show charCount) <> "\nbytes: " <> (show bytesCount))


getCounts :: T.Text -> Int
getCounts input = charCount
  where
    charCount = T.length input
