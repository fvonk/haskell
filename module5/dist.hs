import qualified Data.Map as Map


main :: IO ()
main = do
  putStrLn "Enter first city"
  startInput <- getLine
  let start = Map.lookup startInput locationDB
  putStrLn "Enter second city"
  destInput <- getLine
  let dest = Map.lookup destInput locationDB
  let distance = haversine <$> start <*> dest
  printDistance distance



printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, there is no city in database"
printDistance (Just dist) = putStrLn (show dist ++ " km")

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Archem", (43.2323, 56.2341))
                          ,("Insmut", (23.3412, 45.7563))
                          ,("Carcosa", (21.3412, 25.7563))
                          ,("NewYork", (22.3412, 43.7563))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRad :: LatLong -> (Double, Double)
latLongToRad (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

_haversineIO :: IO LatLong -> IO LatLong -> IO Double
_haversineIO val1 val2 = do
  v1 <- val1
  v2 <- val2
  return (haversine v1 v2)


haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO val1 val2 = haversine <$> val1 <*> val2

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRad coords1
    (rlat2, rlong2) = latLongToRad coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2))^2 + cos rlat1 * cos rlat2 * (sin (dlong / 2))^2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 6378.1
