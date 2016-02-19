module Handlers where

import Web.Scotty
import Web.Scotty.Internal.Types
import Control.Monad.IO.Class
import Data.Maybe

import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import Network.Wai.Parse
import qualified Crypto.Hash.SHA256 as SHA256 
import Data.ByteString.Base16
import Crypto.BCrypt
import qualified Database.Redis as R
import Control.Exception

data Result = Valid | NoUser | NoPasswd | Invalid | Success | Duplicate deriving (Show, Eq)

getFileHandler :: R.ConnectInfo -> Web.Scotty.Internal.Types.ActionT TL.Text IO ()
getFileHandler connInfo = do
  (fn,user,passwd) <- getInfo
  valid <- login connInfo user passwd
  case valid of
    Valid -> do
      fc <- liftIO (try $ B.readFile (generatePath (TL.unpack (fromJust user)) fn) :: IO (Either IOException B.ByteString))
      case fc of
        Left _ -> text "No such file"
        Right f -> raw f
    NoUser -> text "Please specify a User"
    NoPasswd -> text "Pleace specify your Password"
    Invalid -> text "Authentication Failiure"
    _ -> text "Internal Server Error"

createFileHandler :: R.ConnectInfo -> Web.Scotty.Internal.Types.ActionT TL.Text IO ()
createFileHandler connInfo = do
  fs <- files
  (fn,user,passwd) <- getInfo
  valid <- login connInfo user passwd
  case valid of
    Valid -> uploadFile fs fn (TL.unpack (fromJust user))
    NoUser -> text "Please specify a User"
    NoPasswd -> text "Pleace specify your Password"
    Invalid -> text "Authentication Failiure"
    _ -> text "Internal Server Error"


createUserHandler :: R.ConnectInfo -> Web.Scotty.Internal.Types.ActionT TL.Text IO ()
createUserHandler connInfo =  do
  user <- header "User"
  passwd <- header "Password"
  rstat <- createUser connInfo user passwd
  case rstat of
    NoUser -> text "Please specify a User"
    NoPasswd -> text "Pleace specify your Password"
    Duplicate -> text "This user allready exists"
    Success -> text "Success"
    _ -> text "Internal Server Error"


getInfo :: ActionM (String,Maybe TL.Text,Maybe TL.Text)
getInfo = do
  fn <- param "file"
  user <- header "User"
  passwd <- header "Password"
  return (fn,user,passwd)

login :: R.ConnectInfo -> Maybe TL.Text -> Maybe TL.Text -> ActionM Result
login _ Nothing _ = return NoUser
login _ _ Nothing = return NoPasswd
login c (Just user) (Just passwd) = do
  rConn <- liftIO (R.connect c)
  pwd <- liftIO $ getPasswd rConn ((BS.pack . TL.unpack ) user)
  case pwd of
    Right (Just pwd') -> if validatePass passwd pwd' then
                   return Valid
                   else return Invalid
    _ -> return Invalid

getPasswd :: R.Connection -> BS.ByteString -> IO (Either R.Reply (Maybe BS.ByteString))
getPasswd conn u = R.runRedis conn $ R.get (mconcat ["easync:",u])

setPasswd :: R.Connection -> BS.ByteString -> BS.ByteString -> IO (Either R.Reply R.Status)
setPasswd conn u h = R.runRedis conn $ R.set (mconcat ["easync:",u]) h

createUser :: R.ConnectInfo -> Maybe TL.Text -> Maybe TL.Text -> ActionM Result
createUser _ Nothing _ = return NoUser
createUser _ _ Nothing = return NoPasswd
createUser c (Just user) (Just password) = do
  rConn <- liftIO (R.connect c)
  Right mh <- liftIO $ getPasswd rConn ((BS.pack . TL.unpack) user)
  case mh of
    Nothing -> do
      h <- liftIO $ slowHash password
      _ <-liftIO $ setPasswd rConn ((BS.pack . TL.unpack) user) h
      return Success
    Just _ -> return Duplicate

slowHash :: TL.Text -> IO BS.ByteString
slowHash s = do
  h <- hashPasswordUsingPolicy slowerBcryptHashingPolicy ((BS.pack . TL.unpack) s)
  return (fromJust h)

validatePass :: TL.Text -> BS.ByteString -> Bool
validatePass passwd hash = validatePassword hash ((BS.pack . TL.unpack) passwd)

uploadFile :: MonadIO m => [(t,FileInfo B.ByteString)] -> String -> String -> m ()
uploadFile fs fn u = do
  let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
  -- write the files to disk, so they will be served by the static middleware
  liftIO $ sequence_ [ B.writeFile (generatePath u fn) fc | (_,_,fc) <- fs' ]

generatePath :: String -> String -> String
generatePath u fn = "uploads" </> hashSha256 (hashSha256 fn ++ hashSha256 u)

hashSha256 :: String -> String
hashSha256 s = BS.unpack $ encode $ SHA256.hash $ BS.pack s