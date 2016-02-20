module Config where

import qualified Data.Configurator as CFG

data EasyncConfig = EasyncConfig
  { port :: Int
  , redis :: RedisConfig
  } deriving (Show,Eq)

data RedisConfig = RedisConfig
  { host :: String
  , dbPort :: Int
  } deriving (Show, Eq)


readConfig :: FilePath -> IO EasyncConfig
readConfig config_filename = do
  config <- CFG.load [ CFG.Required config_filename]

  prt <- CFG.lookupDefault 3000 config "port"
  dbhost <- CFG.require config "database.host"
  dbport <- CFG.require config "database.port"

  return EasyncConfig
    {
      port = prt
    , redis = RedisConfig
        {
          host = dbhost
        , dbPort = dbport
        }
    }
