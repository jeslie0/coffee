module Page (Page(..), pageList) where

import Prelude

data Page
  = CoffeeTasting
  | CurrentCoffee
  | PreviousResults
  | CoffeeInfo

derive instance eqPage :: Eq Page

instance Show Page where
  show CoffeeTasting = "Coffee Tasting"
  show CurrentCoffee = "Current Coffee"

  show PreviousResults = "Previous Results"
  show CoffeeInfo = "Information"

pageList :: Array Page
pageList =
  [ CoffeeTasting
  -- , CurrentCoffee
  -- , PreviousResults
  -- , CoffeeInfo
  ]
