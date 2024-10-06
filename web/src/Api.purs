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

sendFormData :: Ref Api -> CoffeeTastingData -> Effect Unit
sendFormData _ coffeeData = do
  uuidStr <- UUID.genUUID <#> UUID.toString
  let updatedData = if coffeeData.uuid == "" then coffeeData { uuid = uuidStr } else coffeeData
  saveDataToStorage updatedData
  url <- getUrl
  launchAff_ do
    { status } <- fetch (url <> "coffeeRecord")
      { method: POST
      , body: Yoga.writeJSON updatedData
      , headers: { "Content-Type": "application/json" }
      }
    if status /= 200 then liftEffect $ Console.error "Failed to send data to server" else pure unit

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
  url <- getUrl
  Console.log url
  launchAff_ do
    resp@{ ok } <- fetch (url <> "coffee-list.json")
      { method: GET
      }
    if not ok then pure unit
    else do
      liftEffect $Console.logShow ok
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
  pure $ prot <> "//" <> hst <> "/api/v1"
