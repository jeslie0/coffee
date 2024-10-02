module Content (pageContent) where

import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Prelude (($), (<>))
import Web.Event.Event (preventDefault)

pageContent :: Nut
pageContent =
  DD.div [ DA.klass_ "pf-v5-c-card" ]
    [ DD.div [ DA.klass_ "pf-v5-c-card__title" ]
        [ DD.h2 [ DA.klass_ "pf-v5-c-card__title-text" ] [ DC.text_ "East Timor - Rotutu" ] ]
    , DD.div [ DA.klass_ "pf-v5-c-card__body" ] [ body ]
    , DD.div [ DA.klass_ "pf-v5-c-card__footer" ] [ footer ]
    ]

  where
  body =
    DD.form [ DA.klass_ "pf-v5-c-form", DA.novalidate_ "", DL.submit_ preventDefault ]
      [ makeTextInputForm "Aroma"
      , makeTextInputForm "Undertones"
      , makeTextInputForm "Thoughts"
      , makeNumberInputForm "Ranking"
      , buttons
      ]

  buttons =
    DD.div [ DA.klass_ "pf-v5-c-form__group pf-m-action" ]
      [ DD.div [ DA.klass_ "pf-v5-c-form__actions" ]
          [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-primary", DA.xtype_ "submit" ] [ DC.text_ "Submit form" ]
          , DD.button [ DA.klass_ "pf-v5-c-button pf-m-link", DA.xtype_ "submit" ] [ DC.text_ "Reset form" ]
          ]
      ]

  footer =
    DD.div [] []

makeTextInputForm :: String -> Nut
makeTextInputForm name =
  DD.div [ DA.klass_ "pf-v5-c-form__group" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ name <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ name ] ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ DD.span [ DA.klass_ "pf-v5-c-form-control pf-m-required" ]
            [ DD.input [ DA.required_ "", DA.id_ $ name <> "-form" ] [] ]
        ]
    ]

makeNumberInputForm :: String -> Nut
makeNumberInputForm name =
  DD.div [ DA.klass_ "pf-v5-c-form__group" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ name <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ name ] ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ numberInput
        ]
    ]

  where
  numberInput =
    DD.div [ DA.klass_ "pf-v5-c-number-input" ]
      [ DD.div [ DA.klass_ "pf-v5-c-input-group" ]
          [ DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-control", DA.xtype_ "button" ]
                  [ DD.span [ DA.klass_ "pf-v5-c-number-input__icon" ]
                      [ DD.i [ DA.klass_ "fas fa-minus", DA.ariaHidden_ "true" ] [] ]
                  ]
              ]
          , DD.div [ DA.klass_ "pf-v5-c-input-group__item pf-m-fill" ]
              [ DD.span [ DA.klass_ "pf-v5-c-form-control" ]
                  [ DD.input [ DA.xtype_ "number", DA.value_ "90" ] [] ]
              ]

          , DD.div [ DA.klass_ "pf-v5-c-input-group__item" ]
              [ DD.button [ DA.klass_ "pf-v5-c-button pf-m-control", DA.xtype_ "button" ]
                  [ DD.span [ DA.klass_ "pf-v5-c-number-input__icon" ]
                      [ DD.i [ DA.klass_ "fas fa-plus", DA.ariaHidden_ "true" ] [] ]
                  ]
              ]
          ]
      ]
