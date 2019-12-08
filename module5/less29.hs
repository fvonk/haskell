primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
  where
    twoThroughN = [2 .. n]
    composite = pure (*) <*> twoThroughN <*> twoThroughN
    isNotComposite = not . (`elem` composite)


allFMap :: Applicative f => (a -> b) -> f a -> f b
allFMap func x = pure func <*> x

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 6

-- not determinated calculations

beers :: [Int]
beers = [6, 12]

remain :: [Int]
remain = (\count -> count - 4) <$> beers

neighbourBeers :: [Int]
neighbourBeers = [4]

friends :: [Int]
friends = [2, 3]

totalPeople :: [Int]
totalPeople = (+2) <$> friends

totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*> friendToDrink
                             <*> totalPeople

friendToDrink :: [Int]
friendToDrink = [3, 4]

beersToPurchase :: [Int]
beersToPurchase = pure (-) <*> totalBeersNeeded
                           <*> remain
