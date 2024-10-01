{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable (..))
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serveDirectoryWith)
import Servant.API
import Servant.Server (Application, serve)
import System.Directory
import System.OsPath
import WaiAppStatic.Types (unsafeToPiece)

-- * Types

data Record = Record { id :: T.Text
                     , username :: T.Text
                     , review :: T.Text
                     , rating :: Int
                     }deriving (Generic)

newtype CoffeeCompany = CC T.Text deriving (Eq, Generic)

newtype CoffeeType = CT T.Text deriving (Eq, Generic)

instance Hashable CoffeeType where
  hash (CT txt) = hash txt
  hashWithSalt n (CT txt) = hashWithSalt n txt

instance Hashable CoffeeCompany where
  hash (CC txt) = hash txt
  hashWithSalt n (CC txt) = hashWithSalt n txt

newtype Database = DB (Map.HashMap CoffeeCompany (Map.HashMap CoffeeType [Record])) deriving (Generic)

instance ToJSON Record

instance ToJSON CoffeeCompany

instance ToJSONKey CoffeeCompany

instance ToJSON CoffeeType

instance ToJSONKey CoffeeType

instance ToJSON Database

instance FromJSON Record

instance FromJSON CoffeeCompany

instance FromJSONKey CoffeeCompany

instance FromJSON CoffeeType

instance FromJSONKey CoffeeType

instance FromJSON Database

-- * Api

type CoffeeApi =
  "coffee_db" :> Get '[JSON] Database
    :<|> Capture "coffee_company" T.Text
      :> Capture "coffee_name" T.Text
      :> ( Get '[JSON] [Record]
             :<|> ReqBody '[JSON] Record :> Post '[JSON] ()
             :<|> Delete '[PlainText] T.Text
         )
    :<|> Raw

type Api = "api" :> "v1" :> CoffeeApi

data DatabaseMngr = DBMngr
  { dbMVar :: MVar Database,
    jsonPath :: OsPath
  }

class DBUpdater a where
  insert :: a -> T.Text -> T.Text -> Record -> IO ()

  -- delete :: a -> T.Text -> IO ()
  updateDB :: a -> Database -> IO ()

instance DBUpdater DatabaseMngr where
  insert (DBMngr dbMVar jsonPath) company coffee record = do
    (DB db) <- takeMVar dbMVar
    let newDB = Map.update (Just . Map.update (\records -> Just $ record : records) (CT coffee)) (CC company) db
    commit jsonPath newDB
    putMVar dbMVar $ DB newDB

  -- delete (DBMngr dbMVar jsonPath) id = do
  --   (DB db) <- takeMVar dbMVar
  --   commit jsonPath newDB
  --   undefined

  updateDB (DBMngr dbMVar jsonPath) newDB = do
    _ <- takeMVar dbMVar
    commit jsonPath newDB
    putMVar dbMVar newDB

server :: DatabaseMngr -> Server Api
server mngr =
  getDBHandler mngr
    :<|> ( \coffeeCo coffee ->
             getHandler mngr coffeeCo coffee
               :<|> postHandler mngr coffeeCo coffee
               :<|> deleteHandler mngr coffeeCo coffee
         )
    :<|> serveFiles

getDBHandler :: DatabaseMngr -> Handler Database
getDBHandler (DBMngr dbMVar _jsonPath) = do
  DB db <- liftIO $ readMVar dbMVar
  return $ DB db

getHandler :: DatabaseMngr -> T.Text -> T.Text -> Handler [Record]
getHandler (DBMngr dbMVar _jsonPath) company coffee = do
  DB db <- liftIO $ readMVar dbMVar
  let res = Map.lookup (CC company) db >>= Map.lookup (CT coffee)
  case res of
    Nothing -> return []
    Just rcs -> return rcs

postHandler :: DatabaseMngr -> T.Text -> T.Text -> Record -> Handler ()
postHandler mngr company coffee newRecord = do
  liftIO $ insert mngr company coffee newRecord

deleteHandler :: DatabaseMngr -> T.Text -> T.Text -> Handler T.Text
deleteHandler = undefined

serveFiles :: Server Raw
serveFiles = do
  let initSettings = defaultWebAppSettings "web/dist"
      staticSettings =
        initSettings
          { ssRedirectToIndex = True,
            ssIndices = [unsafeToPiece "index.html"]
          }
  serveDirectoryWith staticSettings

app :: DatabaseMngr -> Application
app mngr = serve (Proxy @Api) $ server mngr

-- * Database

loadDB :: OsPath -> IO Database
loadDB jsonPath = do
  path <- decodeUtf jsonPath
  pathExists <- doesFileExist path
  if pathExists
    then do
      bytes <- BL.readFile path
      case decode @Database bytes of
        Nothing -> do
          putStrLn "Failed to load database..."
          return $ DB Map.empty
        Just db -> return db
    else do
      let db = DB Map.empty
      commit jsonPath $ Map.empty @Int @Int
      return db

commit :: (ToJSON a) => OsPath -> a -> IO ()
commit ospath val = do
  path <- decodeUtf ospath
  BL.writeFile path $ encode val

-- * Main

main :: IO ()
main = do
  dbPath <- encodeUtf "./database.json"
  dbMVar <- loadDB dbPath >>= newMVar
  putStrLn "Running server on 8080..."
  run 8080 (app $ DBMngr dbMVar dbPath)
