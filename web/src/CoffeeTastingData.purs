module CoffeeTastingData where

type CoffeeTastingData =
  { aroma :: Int
  , acidity :: Int
  , sweetness :: Int
  , body :: Int
  , finish :: Int
  , rating :: Int
  , flavour :: String
  , uuid :: String
  , coffee :: String
  }

initialCoffeeTastingData :: CoffeeTastingData
initialCoffeeTastingData =
  { aroma: 0
  , acidity: 0
  , sweetness: 0
  , body: 0
  , finish: 0
  , rating: 0
  , flavour: ""
  , uuid: ""
  , coffee: ""
  }
