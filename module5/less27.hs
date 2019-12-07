import qualified Data.Map as Map


data Box a = Box a deriving Show
instance Functor Box
  where
    fmap func (Box a) = Box (func a)

morePresnets :: Int -> Box a -> Box [a]
morePresnets n box = replicate n <$> box

myBox :: Box Int
myBox = Box 1

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a


leftArmIO :: IO RobotPart
leftArmIO = return leftArm


-- main :: IO ()
-- main = do
--   putStrLn "Enter Id"
--   partNo <- getLine
--   let part = Map.lookup (read partNo) partsDB
--   printCost (cost <$> part)
main :: IO ()
main = do
  id1 <- readInt
  id2 <- readInt
  let r1 = Map.lookup id1 robotPartsDB
  let r2 = Map.lookup id2 robotPartsDB
  printCost (minCost r1 r2)
--
--
minCost :: Maybe RobotPart -> Maybe RobotPart -> Maybe Double
minCost p1 p2 = min <$> (cost <$> p1) <*> (cost <$> p2)

readInt :: IO Int
readInt = read <$> getLine

robotPartsDB :: Map.Map Int RobotPart
robotPartsDB = Map.fromList $ zip [1 .. 3] [leftArm, rightArm, robotHead]


printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "not found"
printCost (Just val) = print val

whatCost :: Int -> Maybe Double
whatCost idd = cost <$> Map.lookup idd partsDB



-- insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partVal2 :: Maybe RobotPart
partVal2 = Map.lookup 2 partsDB

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1, 2, 3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals


data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { name = "one"
  , description = "left hand"
  , cost = 2342.03
  , count = 5
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name = "two"
  , description = "right hand"
  , cost = 122.03
  , count = 3
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name = "three"
  , description = "head"
  , cost = 4444.03
  , count = 2
      }
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2", partName, "</h2>"
                          , "<p><h3>desc</h3>", partDescr
                          , "</p><p><h3>cost</h3>"
                          , partCost
                          , "</p><p><h3>count</h3>"
                          , partCount, "</p>"]
  where
    partName = name part
    partDescr = description part
    partCost = show $ cost part
    partCount = show $ count part
