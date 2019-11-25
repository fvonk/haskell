import qualified Data.Map as Map

data Organ = Heart | Brain | Kindey | Spleen | Finger deriving (Ord, Eq, Enum, Show)
organs = [Heart, Heart, Brain, Kindey, Spleen, Spleen, Spleen, Spleen, Brain, Kindey]

ids :: [Int]
ids = [2, 7, 8, 9, 14, 17, 23, 33]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

catalog :: Map.Map Int Organ
catalog = Map.fromList organPairs

allOrgans :: [Organ]
allOrgans = [Heart .. Finger]

values :: [Organ]
values = map snd (Map.toList catalog)

countOrgans :: [Organ] -> [(Organ, Int)]
countOrgans o = zip allOrgans counting
  where
    counting = map (filterOrgan o) allOrgans

filterOrgan :: [Organ] -> Organ -> Int
filterOrgan organs org = (length . filter (== org)) organs

organCount :: Map.Map Organ Int
organCount = Map.fromList (countOrgans values)


data Box a = Box a deriving Show
wrap :: a -> Box a
wrap x = Box x
unwrap :: Box a -> a
unwrap (Box x) = x
boxMap :: (a -> b) ->  Box a -> Box b
boxMap f (Box x) = Box (f x)


data Triple a = Triple a a a deriving Show
tripleMap :: (a -> b) ->  Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

type Point3D = Triple Double
aPoint :: Point3D
aPoint = Triple 1.2 3.4 0.5

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)


data List a = Empty | Cons a (List a) deriving Show

listEx1 :: List Int
listEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

listEx2 :: List Char
listEx2 = Cons 'k' (Cons 'o' (Cons 't' Empty))

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Cons x rest) = Cons (f x) (mapList f rest)
