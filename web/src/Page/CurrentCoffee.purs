module Page.CurrentCoffee (currentCoffeePage) where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks as DH
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Fetch (fetch)
import Patternfly (makeRadio, makeTextInputForm)
import Web.Event.Event (preventDefault)
import Yoga.JSON (writeJSON)

currentCoffeePage :: Nut
currentCoffeePage =
  DD.div [ DA.klass_ "pf-v5-c-card" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ] [ DC.text_ "East Timor - Rotutu" ] ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ body ]
    , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ footer ]
    ]

  where
  body =
    Deku.do
      setAromaTxt /\ aromaTxt <- DH.useState ""
      setUndertonesTxt /\ undertonesTxt <- DH.useState ""
      setThoughtsTxt /\ thoughtsTxt <- DH.useState ""
      setRating /\ rating <- DH.useState 2

      formData <- DH.useRef initialFormData $ { aroma: _, undertones: _, thoughts: _, rating: _ }
        <$> aromaTxt
        <*> undertonesTxt
        <*> thoughtsTxt
        <*> rating

      let
        sendData = sendFormData formData
        resetData = do
          setAromaTxt initialFormData.aroma
          setUndertonesTxt initialFormData.undertones
          setThoughtsTxt initialFormData.thoughts
          setRating initialFormData.rating

      DD.form [ DA.klass_ "pf-v5-c-form", DA.novalidate_ "", DL.submit_ preventDefault ]
        [ makeTextInputForm "Aroma" "" aromaTxt setAromaTxt
        , makeTextInputForm "Undertones" "" undertonesTxt setUndertonesTxt
        , makeTextInputForm "Thoughts" ""thoughtsTxt setThoughtsTxt
        , ratingRadios rating setRating
        , buttons sendData resetData
        ]

  buttons send reset =
    DD.div [ DA.klass_ "pf-v5-c-form__group pf-m-action" ]
      [ DD.div [ DA.klass_ "pf-v5-c-form__actions" ]
          [ DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-primary"
              , DA.xtype_ "submit"
              , DL.click_ $ \_ -> send
              ]
              [ DC.text_ "Submit form" ]
          , DD.button
              [ DA.klass_ "pf-v5-c-button pf-m-link"
              , DA.xtype_ "submit"
              , DL.click_ $ \_ -> reset
              ]
              [ DC.text_ "Reset form" ]
          ]
      ]

  ratingRadios rating setRating =
    DD.div [ DA.klass_ "pf-v5-c-from__group" ]
      [ makeRadio "Would never drink again" (rating <#> \n -> n == 0) (setRating 0)
      , makeRadio "Would drink but didn't enjoy it" (rating <#> \n -> n == 1) (setRating 1)
      , makeRadio "Neither like nor dislike" (rating <#> \n -> n == 2) (setRating 2)
      , makeRadio "Would drink again but didn't love it" (rating <#> \n -> n == 3) (setRating 3)
      , makeRadio "Absolutely loved it" (rating <#> \n -> n == 4) (setRating 4)
      ]

  footer =
    DD.div [] []

type FormData =
  { aroma :: String
  , undertones :: String
  , thoughts :: String
  , rating :: Int
  }

initialFormData :: FormData
initialFormData = { aroma: "", undertones: "", thoughts: "", rating: 2 }

sendFormData :: Effect FormData -> Effect Unit
sendFormData effFormData = do
  formData <- effFormData
  launchAff_ do
    res <- attempt do
      { status, text } <- fetch "http://localhost:5173"
        { method: POST
        , body: writeJSON formData
        , headers: { "Content-Type": "application/json" }
        }
      pure unit
    case res of
      Left err -> liftEffect $ Console.logShow err
      Right _ -> pure unit
    pure unit
