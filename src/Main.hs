{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

import Web.Scotty

import Network.Wai.Middleware.RequestLogger
import System.Environment
import Data.Maybe
import qualified Database.Redis as R
import System.Console.CmdLib

import Config
import qualified Handlers as H

data Main = Main { config :: String, port :: Maybe Int, uploadDir :: String}
    deriving (Typeable, Data, Eq)

instance Attributes Main where
    attributes _ = group "Options" [
        config %> [ Help "Location of the Config file", ArgHelp "config.cfg"
                    , Default ("easync.cfg" :: String) ],
        uploadDir %> [Help "Location for uploaded files", ArgHelp "uploads", Default ("uploads" :: String)],
        Main.port    %> [Help "Override the Application port specified in the config", Default (Nothing :: Maybe Int)]
        ]

instance RecordCommand Main where
    mode_summary _ = "Easync server"
    run' _ _ = putStrLn "No specific commands are implemented yet"

main :: IO ()
main = getArgs >>= executeR Main {config = "", Main.port=Nothing,uploadDir = ""} >>= \opts -> do
  cfg <- readConfig $ config opts
  let webPort = fromMaybe (Config.port cfg) (Main.port opts)
      redisPort = (dbPort . redis) cfg
      redisHost = (host . redis) cfg
      dir = uploadDir opts
      connInfo = R.defaultConnectInfo {R.connectHost = redisHost
                                      ,R.connectPort = R.PortNumber (fromIntegral redisPort)}
  scotty webPort $ do
  middleware logStdoutDev
  get "/" (H.homeHandler dir)
  get "/sync/:file" (H.getFileHandler connInfo dir)
  get "/hash/:file" (H.getHashHandler dir)
  post "/sync/:file" (H.createFileHandler connInfo dir)
  post "/user/create" (H.createUserHandler connInfo)
