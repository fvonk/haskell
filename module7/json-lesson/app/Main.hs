module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)
instance FromJSON IntList
instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  -- let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

printResponse :: Maybe NOAAResponse -> IO ()
printResponse Nothing = Prelude.putStrLn "Error loading data"
printResponse (Just response) = print response

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = Prelude.putStrLn "Error loading data"
printResults (Just results) = forM_ results (print . name)

printResult :: Maybe NOAAResult -> IO ()
printResult Nothing = Prelude.putStrLn "Error loading data"
printResult (Just results) = (print . name) results


-- NOAA

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)
instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

data Resultset = Resultset { offset :: Int
                           , count :: Int
                           , limit :: Int
                           } deriving (Show, Generic)
instance FromJSON Resultset
instance ToJSON Resultset

data Metadata = Metadata { resultset :: Resultset
                         } deriving (Show, Generic)
instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double
                             , resultId :: T.Text
                             } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
    object [ "uid" .= uid
           , "mindate" .= mindate
           , "maxdate" .= maxdate
           , "name" .= name
           , "datacoverage" .= datacoverage
           , "id" .= resultId
           ]

-- CUSTOM

sampleErrorMessage :: ErrorMessage
sampleErrorMessage = ErrorMessage "ahah" (-1)

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage message errorCode) =
    object [ "message" .= message
           , "error" .= errorCode
           ]

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops\",\"error\": 123}"

data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving (Show)

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"


data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  , age :: Double
  } deriving Show

myName :: Name
myName = Name ("Pavel" :: T.Text) ("Koz" :: T.Text) 123.3

instance ToJSON Name where
  toJSON (Name firstName lastName age) =
    object [ "firstName" .= firstName
           , "last" .= lastName
           , "ag2" .= age
           ]

instance FromJSON Name where
  parseJSON (Object v) =
    Name <$> v .: "firstName"
         <*> v .: "last"
         <*> v .: "ag"

-- BOOK

data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "Pavel Kozlov"
              , title = "Prog on Haskell"
              , year = 2019 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"year\":2019,\"author\":\"Pavel Kozlov\",\"title\":\"Prog on Haskell\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"year\":2119,\"writer\":\"Pavel_Kozlov\",\"title\":\"Prog on Haskell2\"}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON
