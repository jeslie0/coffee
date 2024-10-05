module Page.CoffeeTasting (coffeeTastingPage) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber, round)
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks as DH
import Deku.Pursx (pursx)
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Fetch (fetch)
import Page (Page(..))
import Patternfly (makeSlider, makeRadio, makeTextInputForm, tabItem)
import Web.Event.Event (preventDefault)
import Yoga.JSON (writeJSON)

data Categories
  = Aroma
  | Acidity
  | Sweetness
  | Body
  | Finish
  | Flavour
  | Rating

type FullTableData =
  { quantityData :: CoffeeTastingData
  , qualityData :: CoffeeTastingData
  }

initialFullTableData :: FullTableData
initialFullTableData =
  { quantityData: initialCoffeeTastingData
  , qualityData: initialCoffeeTastingData
  }

type CoffeeTastingData =
  { aroma :: Int
  , acidity :: Int
  , sweetness :: Int
  , body :: Int
  , finish :: Int
  , flavour :: Int
  , rating :: Int
  }

initialCoffeeTastingData :: CoffeeTastingData
initialCoffeeTastingData =
  { aroma: 0
  , acidity: 0
  , sweetness: 0
  , body: 0
  , finish: 0
  , flavour: 0
  , rating: 0
  }

coffeeTastingPage :: Nut
coffeeTastingPage =
  DD.div [ DA.klass_ "pf-v5-c-card" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ] [ DC.text_ "Coffee tasting form" ] ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ body ]
    , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ footer ]
    ]

categories :: Array Categories
categories = [ Aroma, Acidity, Sweetness, Body, Finish, Flavour, Rating ]

instance Show Categories where
  show Aroma = "Aroma"
  show Acidity = "Acidity"
  show Sweetness = "Sweetness"
  show Body = "Body"
  show Finish = "Finish"
  show Flavour = "Flavour"
  show Rating = "Rating"

body :: Nut
body = Deku.do
  DD.table [ DA.klass_ "pf-v5-c-table pf-m-grid-sm", DA.role_ "grid" ]
    [ caption, thead, tbody ]
  where
  caption =
    DD.caption [ DA.klass_ "pf-v5-c-table__caption" ] [ DC.text_ tableCaption ]

  thead =
    DD.thead [ DA.klass_ "pf-v5-c-table__thead" ]
      [ DD.tr
          [ DA.klass_ "pf-v5-c-table__tr", DA.role_ "row" ] $
          categories <#> \category ->
            DD.th [ DA.klass_ "pf-v5-c-table__th", DA.role_ "columnheader", DA.scope_ "col" ]
              [ DC.text_ $ show category ]
      ]

  tbody = Deku.do
    setTableData /\ tableData <- DH.useState initialFullTableData
    tableDataEff <- DH.useRef initialFullTableData tableData
    DD.tbody [ DA.klass_ "pf-v5-c-table__tbody", DA.role_ "rowgroup" ]
      [ DD.tr [ DA.klass_ "pf-v5-c-table__tr", DA.role_ "row" ]
          [ DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.aroma <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { aroma = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.acidity <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { acidity = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.sweetness <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { sweetness = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.body <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { body = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.finish <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { finish = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.flavour <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { flavour = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.quantityData.rating <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { quantityData = tData.quantityData { rating = round n } }
              ]
          ]
      , DD.tr [ DA.klass_ "pf-v5-c-table__tr", DA.role_ "row" ]
          [ DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.aroma <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { aroma = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.acidity <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { acidity = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.sweetness <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { sweetness = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.body <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { body = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.finish <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { finish = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.flavour <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { flavour = round n } }
              ]
          , DD.td [ DA.klass_ "pf-v5-c-table__td", DA.role_ "cell" ]
              [ makeSlider rangeArray (toNumber <<< _.qualityData.rating <$> tableData) $
                  \n -> tableDataEff >>= \tData -> setTableData $ tData { qualityData = tData.qualityData { rating = round n } }
              ]
          ]
      ]

  rangeArray = Array.range 0 5 <#> \n -> show n /\ toNumber n / 5.0 * 100.0

footer =
  DD.div [] []

tableCaption :: String
tableCaption = "Table caption"
