module Page (Page(..), pageList) where

import Prelude

data Page
  = CurrentCoffee
  | PreviousResults
  | CoffeeInfo
  | CoffeeTasting

derive instance eqPage :: Eq Page

instance Show Page where
  show CurrentCoffee = "Current Coffee"
  show PreviousResults = "Previous Results"
  show CoffeeInfo = "Information"
  show CoffeeTasting = "Coffee Tasting"

pageList :: Array Page
pageList = [ CoffeeTasting, CurrentCoffee, PreviousResults, CoffeeInfo ]
