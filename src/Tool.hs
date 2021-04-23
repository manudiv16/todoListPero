module Tool where

import Data.Time
import Database.SQLite.Simple.FromRow
import Data.Text as T
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesBorrowed :: Int
  }

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool,
        ".) ",
        name tool,
        "\n description: ",
        description tool,
        "\n last returned: ",
        show $ lastReturned tool,
        "\n times borrowed: ",
        show $ timesBorrowed tool,
        "\n"
      ]

instance FromRow Tool where
   fromRow = Tool <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field


instance ToRow Tool where
   toRow tool = [ SQLInteger $ fromIntegral $ toolId tool
                , SQLText $ T.pack $ name tool
                , SQLText $ T.pack $ description tool
                , SQLText $ T.pack $ show $ lastReturned tool
                , SQLInteger $ fromIntegral $ timesBorrowed tool ]