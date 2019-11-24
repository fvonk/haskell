data Events = Events [String] deriving (Show, Eq)
instance Semigroup Events where
  (<>) (Events e1) (Events []) = Events e1
  (<>) (Events []) (Events e2) = Events e2
  (<>) (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where
      combiner = (\x y -> mconcat [x, "-", y])
instance Monoid Events where
  mempty = Events []
  mappend = (<>)

data Probs = Probs [Double] deriving (Show, Eq)
instance Semigroup Probs where
  (<>) (Probs p1) (Probs []) = Probs p1
  (<>) (Probs []) (Probs p2) = Probs p2
  (<>) (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)
instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
  where
    totalProbs = sum probs
    normalizedProbs = map (\x -> x / totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable (Events events) (Probs probs)) = mconcat pairs
    where
      pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where
    nToAdd = length l2
    repeatedL1 = map (take nToAdd . repeat) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

instance Semigroup PTable where
  (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
  (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
  (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
    where
      newEvents = e1 <> e2
      newProbs = p1 <> p2

instance Monoid PTable where
  mempty = PTable (Events []) (Probs [])
  mappend = (<>)

e1 = Events ["a", "b"]
e2 = Events ["d", "c"]

coin :: PTable
coin = createPTable (Events ["a", "b"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["r", "g", "b"]) (Probs [0.33, 0.33, 0.34])


--


data Color = Red | Yellow | Blue | Green | Purple | Orange | Clear | Brown deriving (Show, Eq)
instance Semigroup Color where
  (<>) Clear color = color
  (<>) color Clear = color
  (<>) Red Blue = Purple
  (<>) Blue Red = Purple
  (<>) Yellow Blue = Green
  (<>) Blue Yellow = Green
  (<>) Yellow Red = Orange
  (<>) Red Yellow = Orange
  (<>) a b
        | a == b = a
        | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
        | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
        | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
        | otherwise = Brown

instance Monoid Color where
  mempty = Clear
  mappend = (<>)
