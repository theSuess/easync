{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import qualified Database.Redis as R
import System.Environment

import Config
import qualified Handlers as H

data Result = Valid | NoUser | NoPasswd | Invalid | Success | Duplicate deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  let cfgFile = if null args then "" else head args
  cfg <- readConfig cfgFile
  let webPort = (appPort . app) cfg
      redisPort = (dbPort . redis) cfg
      redisHost = (host . redis) cfg
      connInfo = R.defaultConnectInfo {R.connectHost = redisHost
                                      ,R.connectPort = R.PortNumber (fromIntegral redisPort)}
  scotty webPort $ do
  get "/" $ (html . mconcat) ["<h1>Welcome to easync</h1>"]
  get "/sync/:file" (H.getFileHandler connInfo)
  post "/sync/:file" (H.createFileHandler connInfo)
  post "/user/create" (H.createUserHandler connInfo)
