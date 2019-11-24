data Shape = CircleItem Circle | SquareItem Square | RectangleItem Rectangle deriving Show
data Circle = Circle {
    radius :: Double
} deriving Show

data Square = Square {
    side :: Double
} deriving Show

data Rectangle = Rectangle {
    width :: Double
  , height :: Double
} deriving Show

square :: Shape -> Double
square (CircleItem circle) = (radius circle)^2 * pi
square (SquareItem square) = (side square)^2
square (RectangleItem rectangle) = (width rectangle) * (height rectangle)

perimeter :: Shape -> Double
perimeter (CircleItem circle) = 2 * pi * (radius circle)
perimeter (SquareItem square) = 4 * (side square)
perimeter (RectangleItem rectangle) = 2 * (width rectangle) + 2 * (height rectangle)


--

data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show
data Author = Author Name deriving Show
data Name = Name String String
          | NameWithMiddle String String String
          | TwoInitialsWithLast Char Char String
          deriving Show

data Artist = Person Name | Band String deriving Show

data Book = Book {
    author :: Creator
  , isbn :: String
  , bookTitle :: String
  , bookYear :: Int
  , bookPrice :: Double
} deriving Show

data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
} deriving Show

data CollectibleToy = CollectibleToy {
    name :: String
  , toyDescription :: String
  , toyPrice :: Double
} deriving Show

data Pamphlet = Pamphlet {
    title :: String
  , pamphletDescription :: String
  , contactInfo :: String
  , pamphletPrice :: Double
} deriving Show

data StoreItem = BookItem Book
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamphletItem Pamphlet
               deriving Show

kurt :: Creator
kurt = AuthorCreator (Author (Name "Will" "Kurt"))
haskell :: StoreItem
haskell = BookItem Book {
    author = AuthorCreator (Author (Name "Will" "Kurt"))
  , isbn = "23"
  , bookTitle = "haskell programming"
  , bookYear = 2019
  , bookPrice = 7.99
}

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem pamphlet) = pamphletPrice pamphlet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "uknown"
