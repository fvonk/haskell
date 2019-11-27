import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes fullValues
  where
    completeTimes = [minimum times .. maximum times]
    timeValues = Map.fromList (zip times values)
    fullValues = map (\v -> Map.lookup v timeValues) completeTimes


fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where
    (times, values) = unzip tvPairs


showTVPair :: Show a => Int -> Maybe a -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where
      rows = zipWith showTVPair times values

file1 :: [(Int, Double)]
file1 = [(1, 200.1), (3, 220.1), (4, 240.1), (7, 280.1), (10, 300.1), (12, 200.1), (13, 240.1), (14, 200.1), (18, 200.1), (20, 400.1)]

ts1 :: TS Double
ts1 = fileToTS file1

file2 :: [(Int, Double)]
file2 = [(20, 288.1), (13, 222.1), (24, 244.1), (27, 289.1), (30, 301.1), (32, 202.1), (33, 290.1), (34, 230.1), (38, 204.1), (40, 340.1)]

ts2 :: TS Double
ts2 = fileToTS file2


insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (k, Just v) = Map.insert k v myMap


combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combineValues
  where
    bothTimes = mconcat [t1, t2]
    completeTimes = [minimum bothTimes .. maximum bothTimes]
    tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
    updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
    combineValues = map (\v -> Map.lookup v updatedMap) completeTimes

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []
  mappend = (<>)


tsAll :: TS Double
tsAll = mconcat [ts1, ts2]


mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = (realToFrac . sum) xs
    count = (realToFrac . length) xs


meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
  where
   justVals = filter isJust values
   cleanVals = map fromJust justVals
   avg = mean cleanVals


type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where
    newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
    newFunc (_, Nothing) (i, val) = (i, val)
    newFunc (i, val) (_, Nothing) = (i, val)
    newFunc (i1, Just val1) (i2, Just val2) = if func val1 val2 == val1
                                              then (i1, Just val1)
                                              else (i2, Just val2)


compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) =
  if all isNothing values
  then Nothing
  else Just best
  where
    pairs = zip times values
    best = foldl (makeTSCompare func) (0, Nothing) pairs


minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max



diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just a) (Just b) = Just (a - b)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
  where
    shiftValues = tail values
    diffValues = zipWith diffPair shiftValues values


meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe vals =
  if any isNothing vals
  then Nothing
  else (Just avg)
  where
    avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] _ = []
movingAvg vals n =
  if length nextVals == n
  then meanMaybe nextVals : movingAvg restVals n
  else []
  where
    nextVals = take n vals
    restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where
    ma = movingAvg values n
    nothings = replicate (n `div` 2) Nothing
    smoothedValues = mconcat [nothings, ma, nothings]
