module Api (makeApi, sendFormData, Api, getExistingCoffees) where

import Prelude

import CoffeeTastingData (CoffeeTastingData)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Ref (Ref, new)
import Fetch (fetch)
import Web.HTML (window)
import Web.HTML.Location (host, protocol)
import Web.HTML.Window (location, localStorage)
import Web.Storage.Storage (Storage, getItem, setItem)
import Yoga.JSON as Yoga

data Api = Api

makeApi :: Effect (Ref Api)
makeApi = new Api

getLocalStorage :: Effect Storage
getLocalStorage = window >>= localStorage

sendFormData :: Ref Api -> CoffeeTastingData -> Effect Unit -> Effect Unit
sendFormData _ coffeeData onSuccess = do
  uuidStr <- getUUID
  let updatedData = if coffeeData.uuid == "" then coffeeData { uuid = uuidStr } else coffeeData
  saveDataToStorage updatedData
  url <- getApiUrl
  launchAff_ do
    { ok } <- fetch (url <> "coffeeRecord")
      { method: POST
      , body: Yoga.writeJSON updatedData
      , headers: { "Content-Type": "application/json" }
      }
    if not ok
    then liftEffect $ Console.error "Failed to send data to server"
    else liftEffect $ onSuccess

saveDataToStorage :: CoffeeTastingData -> Effect Unit
saveDataToStorage coffeeData = do
  storage <- getLocalStorage
  coffeeDataArrayM <- runMaybeT do
    mdata <- MaybeT $ (getItem "coffeeData" storage <#> \mstring -> Just $ fromMaybe (Yoga.writeJSON ([] :: Array CoffeeTastingData)) $ mstring)
    coffeeDataArray :: Array CoffeeTastingData <- MaybeT <<< pure $ Yoga.readJSON_ mdata
    let updatedCoffeeData = Array.filter (\cd -> cd.uuid /= coffeeData.uuid) coffeeDataArray
    pure $ Array.cons coffeeData updatedCoffeeData
  case coffeeDataArrayM of
    Nothing -> pure unit
    Just arr -> setItem "coffeeData" (Yoga.writeJSON arr) storage

getExistingCoffees :: Ref Api -> (Array String -> Effect Unit) -> Effect Unit
getExistingCoffees _ updater = do
  url <- getApiUrl
  launchAff_ do
    resp@{ ok } <- fetch (url <> "coffee-list.json")
      { method: GET
      }
    if not ok then pure unit
    else do
      _ <- attempt do
        responseBody <- resp.json
        case Yoga.read_ responseBody :: Maybe (Array String) of
          Nothing -> pure unit
          Just arr -> liftEffect $ updater arr
      pure unit

getUrl :: Effect String
getUrl = do
  loc <- window >>= location
  prot <- protocol loc
  hst <- host loc
  pure $ prot <> "//" <> hst <> "/"

getApiUrl :: Effect String
getApiUrl =
  getUrl <#> \url -> url <> "api/v1/"

getUUID :: Effect String
getUUID = do
  storage <- getLocalStorage
  mStr <- getItem "userUUID" storage
  case mStr of
    Just str -> pure str
    Nothing -> do
      uuidStr <- UUID.genUUID <#> UUID.toString
      setItem "userUUID" uuidStr storage
      pure uuidStr
