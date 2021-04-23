module User where

import Database.SQLite.Simple.FromRow 


data User = User
 { userId :: Int
 , userName :: String
 , surname :: String
 , email :: String
 }

instance Show User where
  show user = mconcat [ show $ userId user
                      , ".)  "
                      , userName user
                      , "\n surname: "
                      , surname user
                      , "\n email: "
                      , email user
                      , "\n"
                      ]

instance FromRow User where
   fromRow = User <$> field
                  <*> field
                  <*> field
                  <*> field