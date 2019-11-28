import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

nameData :: Map.Map Int String
nameData = Map.fromList [(1, "Pasha"), (2, "Sasha")]

maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 1 nameData
  let statement = helloPerson name
  return statement


fibonacci :: IO ()
fibonacci = do
  putStrLn "enter n number of Fibonacci"
  n <- getLine
  let answer = fastFib (read n)
  putStrLn ("n number is " ++ (show answer))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib :: Int -> Integer
fastFib n = fastFib' 1 1 n

fastFib' :: Int -> Int -> Int -> Integer
fastFib' _ _ 1 = 1
fastFib' _ n2 2 = toInteger n2
fastFib' n1 n2 counter = fastFib' n2 (n1 + n2) (counter - 1)
