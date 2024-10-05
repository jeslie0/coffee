module Page (Page(..), pageList) where

import Prelude

data Page = CurrentCoffee | PreviousResults | CoffeeInfo

derive instance eqPage :: Eq Page

instance Show Page where
  show CurrentCoffee = "Current Coffee"
  show PreviousResults = "Previous Results"
  show CoffeeInfo = "Information"

pageList :: Array Page
pageList = [ CurrentCoffee, PreviousResults, CoffeeInfo ]
