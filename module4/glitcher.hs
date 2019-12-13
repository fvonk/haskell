{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8 as BC
import System.Environment
import Control.Monad
import Glitch

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  putStrLn "Done!"
