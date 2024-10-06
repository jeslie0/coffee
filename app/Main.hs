{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Servant (Handler, Server, serveDirectoryWith)
import Servant.API
import Servant.Server (Application, serve)
import System.Directory
import System.Environment (getArgs)
import System.OsPath
import WaiAppStatic.Types (unsafeToPiece)

-- * Types

data Record = Record
  { aroma :: Int,
    acidity :: Int,
    sweetness :: Int,
    body :: Int,
    finish :: Int,
    rating :: Int,
    flavour :: T.Text,
    uuid :: T.Text,
    coffee :: T.Text
  }
  deriving (Generic)

newtype Database = DB (Map.HashMap T.Text [Record]) deriving (Generic)

instance ToJSON Record

instance ToJSON Database

instance FromJSON Record

instance FromJSON Database

-- * Api

type CoffeeApi =
  "coffee-list.json" :> Get '[JSON] [T.Text]
    :<|> "coffeeRecord" :> ReqBody '[JSON] Record :> Post '[JSON] ()

type Api =
  "api" :> "v1" :> CoffeeApi
    :<|> Raw

server :: FilePath -> DatabaseMngr -> Server Api
server path mngr =
  ( getCoffeeListJson
      :<|> postHandler mngr
  )
    :<|> serveFiles path

getCoffeeListJson :: Handler [T.Text]
getCoffeeListJson = do
  bytes <- liftIO $ BL.readFile "./coffee-list.json"
  case decode @[T.Text] bytes of
    Nothing -> return []
    Just val -> return val

postHandler :: DatabaseMngr -> Record -> Handler ()
postHandler mngr newRecord = do
  liftIO $ insert mngr newRecord

serveFiles :: FilePath -> Server Raw
serveFiles path = do
  let initSettings = defaultWebAppSettings path
      staticSettings =
        initSettings
          { ssRedirectToIndex = True,
            ssIndices = [unsafeToPiece "index.html"]
          }
  serveDirectoryWith staticSettings

app :: FilePath -> DatabaseMngr -> Application
app path mngr = serve (Proxy @Api) $ server path mngr

-- * Database manager
data DatabaseMngr = DBMngr
  { dbMVar :: MVar Database,
    jsonPath :: OsPath
  }

class DBUpdater a where
  insert :: a -> Record -> IO ()

  updateDB :: a -> Database -> IO ()

instance DBUpdater DatabaseMngr where
  insert (DBMngr dbMVar jsonPath) record = do
    (DB db) <- takeMVar dbMVar
    let newDB =
          Map.alter
            ( \case
                Just records -> Just $ record : filter (\rcd -> uuid rcd /= uuid record) records
                Nothing -> Just [record]
            )
            (uuid record)
            db
    commit jsonPath newDB
    putMVar dbMVar $ DB newDB

  updateDB (DBMngr dbMVar jsonPath) newDB = do
    _ <- takeMVar dbMVar
    commit jsonPath newDB
    putMVar dbMVar newDB


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
  args <- getArgs
  absPath <-
    ( if null args
        then canonicalizePath "./web/dist"
        else canonicalizePath $ head args
      )
  dbPath <- encodeUtf "./database.json"
  dbMVar <- loadDB dbPath >>= newMVar
  putStrLn "Running server on 9000..."
  run 9000 (app absPath $ DBMngr dbMVar dbPath)
