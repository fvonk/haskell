-- myAv :: [Double] -> Double
myAv l = sum l / (fromIntegral (length l))


halve :: Integer -> Integer
halve a = div a 2

printDouble :: Int -> [Char]
printDouble a = show (a * 2)


makeAddress a b c = (a, b, c)
makeAddressL a b c = (\a ->
                      (\b ->
                       (\c -> (a, b, c)))) a b c

simple :: a -> a
simple x = x


safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

myFoldl :: (i -> a -> i) -> i -> [a] -> i
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
