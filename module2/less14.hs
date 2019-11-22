import Data.List


data Number = One | Two | Three deriving (Enum, Show)
instance Eq Number where
   (==) a b = (fromEnum a) == (fromEnum b)

instance Ord Number where
  compare a b = compare (fromEnum a) (fromEnum b)


names :: [Name]
names = [Name ("Z", "B"),
         Name ("A", "A")]

data Name = Name (String, String) deriving (Show, Eq)
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l1, f2)





class (Eq a, Enum a) => Die a where
  roll :: Int -> a
  getFirst :: a

data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Show, Eq, Enum)
instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
  getFirst = Side1

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Enum, Eq, Ord)
instance Die SixSidedDie where
  roll n = toEnum (n `mod` 6)
  getFirst = S1

-- instance Enum SixSidedDie where
--   toEnum 0 = S1
--   toEnum 1 = S2
--   toEnum 2 = S3
--   toEnum 3 = S4
--   toEnum 4 = S5
--   toEnum 5 = S6
--   toEnum _ = error "no value"
--
--   fromEnum S1 = 0
--   fromEnum S2 = 1
--   fromEnum S3 = 2
--   fromEnum S4 = 3
--   fromEnum S5 = 4
--   fromEnum S6 = 5

-- instance Show SixSidedDie where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

-- instance Eq SixSidedDie where
--   (==) S1 S1 = True
--   (==) S2 S1 = True
--   (==) S3 S1 = True
--   (==) S4 S1 = True
--   (==) S5 S1 = True
--   (==) _ _ = False
--
-- instance Ord SixSidedDie where
--   compare S4 S4 = EQ
--   compare S4 _ = GT
--   compare _ S4 = LT
