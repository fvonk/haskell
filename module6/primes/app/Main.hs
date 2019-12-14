module Main where

import Primes

main :: IO ()
main = do
  putStrLn "Enter the number"
  val <- readInt
  handleInputInt val
  putStrLn "Next!"
  main


handleInputInt :: Int -> IO ()
handleInputInt n =
  case isPrime n of
    (Just True) -> putStrLn "it's prime"
    (Just False) -> putStrLn ("it's composite: " ++ pFactors)
    otherwise -> putStrLn "over range"
  where
    pFactors = (show . primeFactors) n


readInt :: IO Int
readInt = read <$> getLine
