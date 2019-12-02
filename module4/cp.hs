{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Environment
import System.IO




main :: IO ()
main = do
  args <- getArgs
  let (fromFile, toFile) = (\(from:to:_) -> (from,to)) args
  input <- TI.readFile fromFile
  TI.writeFile toFile input
  TI.putStrLn "Done!"
