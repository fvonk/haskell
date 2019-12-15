import Data.Char(isDigit)
import Data.Maybe

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) =
  (:) <$> maybeHead xs <*> myTakeSafer (n - 1) (Just (tail xs))


eitherHead :: [a] -> Either String a
eitherHead [] = Left "no first element in list"
eitherHead (x:_) = Right x


primes :: [Int]
primes = [2, 3, 5, 7]

maxN = 10

data PrimeError = TooLarge | InvalidValue
instance Show PrimeError where
  show TooLarge = "too big number"
  show InvalidValue = "number is not correct for handling"


isPrime :: Int -> Either PrimeError Bool
isPrime n
        | n < 2 = Left InvalidValue
        | n > maxN = Left TooLarge
        | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "is prime number"
displayResult (Right False) = "is composite number"
displayResult (Left primeError) = show primeError


main :: IO ()
main = do
  putStrLn "enter the number for checking"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn (displayResult result)

-- task 1

addStrInts :: String -> String -> Either SumsIntError Int
addStrInts a b
  | aInt == Nothing && bInt == Nothing = Left BothWrong
  | aInt == Nothing = Left LeftWrong
  | bInt == Nothing = Left RightWrong
  | otherwise = Right (fromJust ((+) <$> aInt <*> bInt))
  where
    aInt = strToInt a
    bInt = strToInt b

data SumsIntError = LeftWrong | RightWrong | BothWrong
instance Show SumsIntError where
  show LeftWrong = "left value is not number"
  show RightWrong = "right value is not number"
  show BothWrong = "both values are not numbers"

strToInt :: String -> Maybe Int
strToInt "" = Nothing
strToInt a
  | allDigits = Just (read a)
  | otherwise = Nothing
  where
    allDigits = all isDigit
      ((\(x:xs) ->
        if x == '-'
        then xs
        else (x:xs)
      ) a)


-- task2

maybeSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
maybeSucc a =
  if a == maxBound
  then Nothing
  else (Just . succ) a

tailSafer :: [a] -> [a]
tailSafer [] = []
tailSafer (x:xs) = xs


data ListError = Empty | Infinite
instance Show ListError where
  show Empty = "Empty list"
  show Infinite = "Infinite list"

lastSafer :: [a] -> Either ListError a
lastSafer [] = Left Empty
lastSafer xs = lastSafer' 10000 xs

lastSafer' :: Int -> [a] -> Either ListError a
lastSafer' 0 _ = Left Infinite
lastSafer' _ (x:[]) = Right x
lastSafer' n (x:xs) = lastSafer' (n - 1) xs
