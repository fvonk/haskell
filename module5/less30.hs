import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1, "asdf2f")
                          ,(2, "vusdfhvs8fy")
                          ,(3, "420j20f2f")
                          ,(4, "asdh0g2")
                          ,(5, "sdfh20f2")
                          ,(6, "Asdf2f2")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("asdf2f", 13)
                         ,("vusdfhvs8fy", 123)
                         ,("420j20f2f", 120)
                         ,("asdh0g2", 13)
                         ,("sdfh20f2", 103)
                         ,("Asdf2f2", 33)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

-- test id = pure lookupCredits <*> lookupUserName id

wrapperVerbose :: IO ()
wrapperVerbose = putStrLn "Enter The line" >>
                 getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "Say your name" >>
             getLine >>= (\x -> putStrLn ("Hi mister " ++ x))

readInt :: IO Int
readInt = read <$> getLine
printDouble :: Int -> IO ()
printDouble n = print(n * 2)


plus2 :: Num a => a -> IO a
plus2 = \input -> return (input + 2)


-- task 1
allFMapM :: Monad m => (a -> b) -> m a -> m b
allFMapM func m = m >>= (\input -> return (func input))
-- task 2
allApp :: Monad m => m (a -> b) -> m a -> m b
-- allApp func val = func >>= (\f -> val >>= (\x -> return (f x)))
allApp func val = func >>= (\f -> val >>= (\x -> return (f x)))

-- task 3
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just val) func = func val
