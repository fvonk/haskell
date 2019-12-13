
module Main where

main :: IO ()
main = putStrLn "Hello World"

head :: Monoid a => [a] -> a
head (x:_) = x
head [] = mempty
