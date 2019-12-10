import qualified Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size / 2)^2

type Pizza = (Double, Double)

costPerCm :: Pizza -> Double
costPerCm (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
  then p1
  else p2
  where
    costP1 = costPerCm p1
    costP2 = costPerCm p2

describePizza :: Pizza -> String
describePizza (size, cost) = "Pizza size of " ++ show size ++
                            " cheaper by the cost of " ++ show costSqCm ++
                            "on one square cm"
                            where
                              costSqCm = costPerCm (size, cost)

-- main :: IO ()
-- main = do
--   putStrLn "1 Pizza size"
--   size1 <- getLine
--   putStrLn "1 Pizza cost"
--   cost1 <- getLine
--   putStrLn "2 Pizza size"
--   size2 <- getLine
--   putStrLn "2 Pizza cost"
--   cost2 <- getLine
--   let pizza1 = (read size1, read cost1)
--   let pizza2 = (read size2, read cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   putStrLn (describePizza betterPizza)

main :: IO ()
main = do
  putStrLn "1 Pizza size" >>
    getLine >>=
    (\size1 ->
      putStrLn "1 Pizza cost" >>
        getLine >>=
        (\cost1 ->
          putStrLn "2 Pizza size" >>
            getLine >>=
            (\size2 ->
              putStrLn "2 Pizza cost" >>
                getLine >>=
                (\cost2 ->
                  putStrLn "2 Pizza cost" >>
                   (\pizza1 ->
                     (\pizza2 ->
                       (\betterPizza ->
                        putStrLn (describePizza betterPizza)
                       ) (comparePizzas pizza1 pizza2)
                     ) (read size2, read cost2)
                   ) (read size1, read cost1)
                )
            )
        )
    )

costData :: Map.Map Int Double
costData = Map.fromList [(1, 150), (2, 220)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 30), (2, 50)]

-- maybeMain :: Maybe String
-- maybeMain = do
--   size1 <- Map.lookup 1 sizeData
--   cost1 <- Map.lookup 1 costData
--   size2 <- Map.lookup 2 sizeData
--   cost2 <- Map.lookup 2 costData
--   let pizza1 = (size1, cost1)
--   let pizza2 = (size2, cost2)
--   let betterPizza = comparePizzas pizza1 pizza2
--   return (describePizza betterPizza)

ioMain :: IO (Double)
ioMain = do
  putStrLn "1 Pizza size"
  getLine >>= (return . read)

monadMain :: (Monad m) => m Double -> m Double ->
                          m Double -> m Double -> m String
monadMain a b c d = do
  size1 <- a
  cost1 <- b
  size2 <- c
  cost2 <- d
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

listMain :: [String]
listMain = do
  size1 <- [34, 30]
  cost1 <- [42, 150]
  size2 <- [4]
  cost2 <- [3]
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
