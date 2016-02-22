{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import System.Environment
import Data.Maybe
import qualified Database.Redis as R
import System.Console.CmdLib

import Config
import qualified Handlers as H

data Main = Main { config :: String, port :: Maybe Int }
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        config %> [ Help "Location of the Config file", ArgHelp "config.cfg"
                    , Default ("easync.cfg" :: String) ],
        Main.port    %> [Help "Override the Application port specified in the config", Default (Nothing :: Maybe Int)]
        ]

instance RecordCommand Main where
    mode_summary _ = "Easync server"
    run' _ _ = putStrLn "No specific commands are implemented yet"

main :: IO ()
main = getArgs >>= executeR Main {config = "", Main.port=Nothing} >>= \opts -> do
  cfg <- readConfig $ config opts
  let webPort = fromMaybe (Config.port cfg) (Main.port opts)
      redisPort = (dbPort . redis) cfg
      redisHost = (host . redis) cfg
      connInfo = R.defaultConnectInfo {R.connectHost = redisHost
                                      ,R.connectPort = R.PortNumber (fromIntegral redisPort)}
  scotty webPort $ do
  middleware logStdoutDev
  get "/" $ (html . mconcat) ["<h1>Welcome to easync</h1>"]
  get "/sync/:file" (H.getFileHandler connInfo)
  get "/hash/:file" H.getHashHandler
  post "/sync/:file" (H.createFileHandler connInfo)
  post "/user/create" (H.createUserHandler connInfo)
