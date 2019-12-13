{-# LANGUAGE OverloadedStrings #-}
module Glitch
    ( glitchActions
    ) where

import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReverseBytes, randomReplaceByte, randomSortSection, randomReplaceByte]

intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc chV bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC chV

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  chV <- randomRIO (0, 255)
  return (replaceByte location chV bytes)


randomChar :: IO Char
randomChar = do
  randInt <- randomRIO (0, 255)
  return (toEnum randInt)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes =
  mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes =
  mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (reverseSection start sectionSize bytes)
