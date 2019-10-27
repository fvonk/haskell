messyMain :: IO ()
messyMain = do
  putStrLn "Who ?"
  recipient <- getLine
  putStrLn "title ?"
  title <- getLine
  putStrLn "Author ?"
  author <- getLine
  putStrLn ("Dear " ++ recipient ++ "!\n" ++ "Thanks for \"" ++ title ++ "\"!\n Best" ++ author)
