module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

gDBName = "tools.db"

main :: IO ()
main = do
  putStrLn "Enter the command:"
  command <- getLine
  performCommand command


-- COMMANDS

performCommand :: String -> IO ()
performCommand "users"    = printUsers >> main
performCommand "tools"    = printTools >> main
performCommand "adduser"  = promptAndAddUser >> main
performCommand "addtool"  = promptAndAddTool >> main
performCommand "checkout" = promptAndCheckout >> main
performCommand "checkin"  = promtAndCheckin >> main
performCommand "in"       = printAvailable >> main
performCommand "out"      = printCheckedout >> main
performCommand "quit"     = putStrLn "chao!"
performCommand _          = putStrLn "command not found!" >> main

-- INTERACTIONS

promptAndAddTool :: IO ()
promptAndAddTool = do
  putStrLn "Enter name of the tool to add"
  name <- getLine
  putStrLn "Enter description of the tool to add"
  description <- getLine
  addTool name description

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter username to add"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  putStrLn "Enter user ID"
  userId <- pure read <*> getLine
  putStrLn "Enter tool ID"
  toolId <- pure read <*> getLine
  checkout userId toolId


promtAndCheckin :: IO ()
promtAndCheckin = do
  putStrLn "Enter tool ID to return"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

-- DELETE

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

checkin :: Int -> IO ()
checkin toolId =
  withConn gDBName $
    \conn ->
      execute conn
              "DELETE FROM checkedout WHERE tool_id = (?)"
              (Only toolId)

-- UPDATE

updateToolTable :: Int -> IO ()
updateToolTable toolId =
  withConn gDBName $
    \conn -> do
      tool <- selectTool conn toolId
      currentDay <- utctDay <$> getCurrentTime
      let updatedTool = updateTool <$> tool
                                   <*> pure currentDay
      updateOrWarn conn updatedTool

updateOrWarn :: Connection -> Maybe Tool -> IO ()
updateOrWarn _ Nothing = putStrLn "id was not found"
updateOrWarn conn (Just tool) = do
    let q = mconcat [ "UPDATE TOOLS SET "
                    , "lastReturned = ?,"
                    , " timesBorrowed = ? "
                    , "WHERE ID = ?"]
    execute conn q (lastReturned tool,
                    timesBorrowed tool,
                    toolId tool)
    putStrLn "tool data was changed"

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
  { lastReturned = date
  , timesBorrowed = 1 + timesBorrowed tool
  }

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn
                "SELECT * FROM tools WHERE id = (?)"
                (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

-- READ

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools"

printAvailable :: IO ()
printAvailable =
  printToolQuery $
    mconcat [ "SELECT * FROM tools "
            , "WHERE id not in "
            , "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout =
  printToolQuery $
    mconcat [ "SELECT * FROM tools "
            , "WHERE id in "
            , "(select tool_id from checkedout);"]


printToolQuery :: Query -> IO ()
printToolQuery q =
  withConn gDBName $
    \conn -> do
      resp <- query_ conn q :: IO [Tool]
      mapM_ print resp


printUsers :: IO ()
printUsers =
  withConn gDBName $
    \conn -> do
      resp <- query_ conn "SELECT * FROM users" :: IO [User]
      mapM_ print resp

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
--

checkout :: Int -> Int -> IO ()
checkout userId toolId =
  withConn gDBName
    $ \conn ->
      execute conn
              "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)"
              (userId, toolId)


-- ADD

addTool :: String -> String -> IO ()
addTool name description =
  withConn gDBName $
    \conn -> do
      day <- show . utctDay <$> getCurrentTime
      execute conn
              "INSERT INTO tools (name, description, timesBorrowed) VALUES (?,?,?,?)"
              (name, description, day, 0 :: Int)
      putStrLn "Tool was added"

addUser :: String -> IO ()
addUser userName = do
  withConn gDBName $
    \conn -> do
       execute conn
              "INSERT INTO users (username) VALUES (?)"
              (Only userName)
       putStrLn "User was added"

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn


-- MODELS

data Tool = Tool { toolId :: Int
                 , name :: String
                 , description :: String
                 , lastReturned :: Day
                 , timesBorrowed :: Int
                 }
instance Show Tool where
  show tool = mconcat [ show $ toolId tool
                      , ".) "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last time returned: "
                      , show $ lastReturned tool
                      , "\n times borrowed this tool: "
                      , show $ timesBorrowed tool
                      , "\n"]


data User = User { userId :: Int
                 , userName :: String
                 }
instance Show User where
 show user = mconcat [ show $ userId user, ".) ", userName user]
