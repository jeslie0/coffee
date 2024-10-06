module Page.CoffeeTasting (coffeeTastingPage) where

import Prelude

import Api (Api, sendFormData)
import CoffeeTastingData (initialCoffeeTastingData)
import Data.Array as Array
import Data.Int (toNumber, round)
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import FRP.Poll (Poll)
import Patternfly (gallery, galleryItem, makeDropdown, makeSliderForm, makeTextInputForm)
import SVG.CheckCircle (checkCircle)
import Web.Event.Event (preventDefault)

data Categories
  = Aroma
  | Acidity
  | Sweetness
  | Body
  | Finish
  | Flavour
  | Rating

coffeeTastingPage :: Ref Api -> Poll (Array String) -> Nut
coffeeTastingPage api coffeeList =
  gallery
    [ galleryItem $
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-full-height pf-m-display-lg" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ] [ DC.text_ "Coffee tasting guide" ] ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ guideBody ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ guideNotes ]
            ]
        ]
    , galleryItem $
        [ DD.div [ DA.klass_ "pf-v5-c-card pf-m-full-height pf-m-display-lg" ]
            [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
                [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ] [ DC.text_ "Coffee tasting form" ] ]
            , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ body api coffeeList ]
            ]
        ]
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

body :: Ref Api -> Poll (Array String) -> Nut
body api coffeeList = Deku.do
  setCoffeeData /\ coffeeData <- DH.useState initialCoffeeTastingData
  coffeeDataEff <- DH.useRef initialCoffeeTastingData coffeeData
  setShowSuccess /\ showSuccess <- DH.useState false

  setDropdownOpen /\ dropdownOpen <- DH.useState false

  let
    formMaker label get set =
      makeSliderForm label rangeArray (toNumber <<< get <$> coffeeData) (\n -> coffeeDataEff >>= \tData -> setCoffeeData $ set tData n)

    send = coffeeDataEff >>= \fdata -> sendFormData api fdata (setShowSuccess true)
    reset = setCoffeeData initialCoffeeTastingData

    coffeeListPoll = coffeeList <#> \arr -> arr <#> \str -> str /\ (coffeeDataEff >>= \cdata -> setCoffeeData $ cdata { coffee = str })

  DD.form [ DA.klass_ "pf-v5-c-form pf-m-horizontal", DA.novalidate_ "", DL.submit_ preventDefault ]
    [ makeDropdown "Coffee" dropdownOpen setDropdownOpen (coffeeData <#> _.coffee) coffeeListPoll
    , formMaker "Aroma" (_.aroma) (\tData n -> tData { aroma = round n })
    , formMaker "Acidity" (_.acidity) (\tData n -> tData { acidity = round n })
    , formMaker "Sweetness" (_.sweetness) (\tData n -> tData { sweetness = round n })
    , formMaker "Body" (_.body) (\tData n -> tData { body = round n })
    , formMaker "Finish" (_.finish) (\tData n -> tData { finish = round n })
    , formMaker "Rating" (_.rating) (\tData n -> tData { rating = round n })
    , makeTextInputForm "Flavour" "" (_.flavour <$> coffeeData) (\str -> coffeeDataEff >>= \cf -> setCoffeeData $ cf { flavour = str })
    , buttons coffeeData send reset showSuccess
    ]

  where
  rangeArray = let max = 4 in Array.range 0 max <#> \n -> show n /\ toNumber n / toNumber max * 100.0

  buttons coffeeData send reset showSuccess =
    DD.div [ DA.klass_ "pf-m-action" ]
      [ DD.span [ ]
          [ DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              , DA.xtype_ "submit"
              , DL.click_ $ \_ -> send
              , DA.disabled $ coffeeData <#> \cdata -> if cdata.coffee == "" then "true" else "false"
              ]
              [ DC.text_ "Submit form" ]
          , showSuccess <#~> if _ then checkCircle else DD.div [] []
          , DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-link"
              , DA.xtype_ "submit"
              , DL.click_ $ \_ -> reset
              ]
              [ DC.text_ "Reset form" ]
          ]
      ]


guideBody :: Nut
guideBody =
  DD.div [ DA.klass_ "pf-v5-c-content" ]
    [ DD.p []
        [ DC.text_ "One of the best ways to improve your coffee drinking experience is to drink coffee that you truly enjoy. To find coffee that you love, it helps to identify which aspects of its flavour appeal to you most. You can then use this information when selecting which coffees to try. The form on this page is designed to help you focus on different elements of the taste of coffee, so you can more easily determine which properties you like. There are no wrong answers when it comes to coffee preference." ]
    , DD.p []
        [ DC.text_ "Each of the properties in the form is accompanied by a slider. The slider value represents how much of the property you detect, rather than an opinion on it. The \"Rating\" slider is for how much you enjoy the coffee. There is also an text box at the bottom to add notes on the coffee, such as any flavours that your detect." ]
    , DD.p []
        [ DC.text_ "The most important part is to have fun and enjoy the magic bean juice!" ]
    ]

guideNotes :: Nut
guideNotes =
  DD.div [ DA.klass_ "pf-v5-c-content" ]
    [ DD.ul [ DA.klass_ "pf-v5-c-list", DA.role_ "list" ]
        [ DD.li__ "Aroma: The smell of the coffee. How intense is the aroma before you start to drink?"
        , DD.li__ "Acidity: Think of this as the brightness, crispness or juiciness of the coffee. It is generally more prominent in lighter roast."
        , DD.li__ "Sweetness: Some coffees are sweeter than others. How sweet is the coffee you are drinking?"
        , DD.li__ "Body: Sometimes called \"mouthfeel\", the body of a coffee refers to the texture of the coffee when in the mouth. Does it feel fuller and heavier or lighter?"
        , DD.li__ "Finish: How does the coffee feel when you have swallowed? Does the coffee flavour linger or does it disappear?"
        , DD.li__ "Flavour: What words come to mind when describing the coffee? Do you detect any fruitiness, bitterness or nuttiness? There are no wrong answers - you might have a different opinion from everyone else."
        ]
    ]
