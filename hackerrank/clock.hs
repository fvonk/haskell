import Data.List
import Data.Maybe

data Format = AM | PM deriving (Show, Enum, Eq)
parseFormat :: String -> Maybe Format
parseFormat "AM" = Just AM
parseFormat "PM" = Just PM
parseFormat _ = Nothing

convert :: String -> Maybe Format -> Maybe String
convert _ Nothing = Nothing
convert time (Just format) = Just (newHoursStr ++ woHours)
  where 
     woHours = (take (length time - 4) . (drop 2)) time  
     hours = (read . take 2) time :: Int
     newHours
        | PM == format && hours /= 12 = (hours + 12) `mod` 24
        | AM == format && hours == 12 = 0
        | otherwise = hours
     newHoursStr 
        | newHours < 10 = "0" ++ show newHours
        | otherwise = show newHours

solve :: String -> Maybe String
solve input = result
  where 
    format = (parseFormat . drop (length input - 2)) input
    result = convert input format
    

main = interact $ fromJust . solve 
