module Config where

import Data.Yaml

data EasyncConfig = EasyncConfig
  { app :: AppConfig
  , redis :: RedisConfig
  } deriving (Show,Eq)

data AppConfig = AppConfig { appPort :: Int } deriving (Show, Eq)

data RedisConfig = RedisConfig
  { host :: String
  , dbPort :: Int
  } deriving (Show, Eq)

instance FromJSON EasyncConfig where
  parseJSON (Object m) = EasyncConfig <$>
    m .: "app" <*>
    m .: "redis"
  parseJSON x = fail ("Not an object: " ++ show x)

instance FromJSON AppConfig where
  parseJSON (Object m) = AppConfig <$>
    m .: "port"
  parseJSON x = fail ("Not an object: " ++ show x)

instance FromJSON RedisConfig where
  parseJSON (Object m) = RedisConfig <$>
    m .: "host" <*>
    m .: "port"
  parseJSON x = fail ("Not an object: " ++ show x)

readConfig :: IO EasyncConfig
readConfig =
  either (error . show) id <$> decodeFileEither "./easync.yaml"
