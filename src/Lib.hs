{-# LANGUAGE OverloadedStrings #-}


module Lib
    ( main
    ) where

import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple

import Data.Time
import User 
import Tool



type UserProm = (String,String,String)

addUser :: UserProm -> IO ()
addUser user = withConn "tools.db" $
                   \conn -> do
                     execute conn "INSERT INTO users (username,surname,email) VALUES (?,?,?)"
                       user
                     
addManyUsers :: [UserProm] -> IO ()
addManyUsers users = withConn "tools.db" $
                   \conn -> do
                     executeMany conn "INSERT INTO users (username,surname,email) VALUES (?,?,?)" users
                     
deleteUser :: User -> IO ()
deleteUser user = withConn "tools.db" $ 
                \conn -> do
                  execute conn 
                    "DELETE FROM users WHERE username = (?);"
                    $ Only $ userName user

printUsers :: IO ()
printUsers = withConn "tools.db" $
             \conn ->  do
               resp <- query_ conn "SELECT * FROM users;" :: IO [User]
               mapM_ print resp



checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                           execute conn "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                             (userId,toolId)

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
   conn <- open dbName
   action conn
   close conn





                  
                  
printToolQuery :: Query -> IO ()
printToolQuery q =  withConn "tools.db" $
                         \conn ->  do
                           resp <- query_ conn q :: IO [Tool]
                           mapM_ print resp
                           
printToolQuery_ :: ToRow a => Query -> a -> IO ()
printToolQuery_ q a =  withConn "tools.db" $
                         \conn ->  do
                           resp <- query conn q a :: IO [User]
                           mapM_ print resp                         

printByNQuery :: Int -> IO ()
printByNQuery i = printToolQuery_  (mconcat  ["SELECT * FROM users WHERE ID = ?;"])  (Only i) 

printTools :: IO ()
printTools =  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
                  mconcat [ "select * from tools "
                          , "where id in "
                          , "(select tool_id from checkedout);"]

                          
selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
   resp <- query conn
           "SELECT * FROM tools WHERE id = (?)"
           (Only toolId) :: IO [Tool]
   return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
   { lastReturned = date
   , timesBorrowed = 1 + timesBorrowed tool
   }
   
updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) =  withConn "tools.db" $
                            \conn -> do
                              let q = mconcat ["UPDATE TOOLS SET  "
                                              ,"lastReturned = ?,"
                                              ," timesBorrowed = ? "
                                              ,"WHERE ID = ?;"]

                              execute conn q (lastReturned tool
                                             , timesBorrowed tool
                                             , toolId tool)
                              print "tool updated"
                              
                              
updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                     \conn -> do
                       tool <- selectTool conn toolId
                       currentDay <- utctDay <$> getCurrentTime
                       let updatedTool = updateTool <$> tool
                                                    <*> pure currentDay
                       updateOrWarn updatedTool
                       
                       

                
checkin :: Int -> IO ()
checkin toolId =  withConn "tools.db" $
                     \conn -> do
                       execute conn
                         "DELETE FROM checkedout WHERE tool_id = (?);"
                         (Only toolId)
                         
                         
                         
checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
   checkin toolId
   updateToolTable toolId
   
   
   
   
   
promptAndAddUser :: IO ()
promptAndAddUser = do
   print "Enter new user name"
   userName <- getLine
   print "Enter surname"
   surName <- getLine
   print "Enter Email"
   email <- getLine
   addUser (userName,surName,email)

promptAndCheckout :: IO ()
promptAndCheckout = do
   print "Enter the id of the user"
   userId <- read <$> getLine
   print "Enter the id of the tool"
   toolId <- read <$> getLine
   checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
   print "enter the id of tool"
   toolId <- read <$> getLine
   checkinAndUpdate toolId
   
performCommand :: String -> IO ()
performCommand command
   | command == "users" = printUsers >> main
   | command == "tools" = printTools >> main
   | command == "adduser" = promptAndAddUser >> main
   | command == "checkout" = promptAndCheckout >> main
   | command == "checkin" = promptAndCheckin >> main
   | command == "in" = printAvailable >> main
   | command == "out" = printCheckedout >> main
   | command == "quit" = print "bye!"
   | otherwise = print "Sorry command not found" >> main


main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command