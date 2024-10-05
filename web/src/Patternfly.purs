module Patternfly where

import Prelude

import Data.Maybe (Maybe(..))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Listeners as DL
import Deku.DOM.Attributes as DA
import Effect (Effect)
import FRP.Poll (Poll)
import Web.Event.Event (currentTarget)
import Web.HTML.HTMLInputElement as Input

gallery :: Array Nut -> Nut
gallery =
  DD.div
    [ DA.klass_ "pf-v5-l-gallery pf-m-gutter"
    , DA.style_ "--pf-v5-l-gallery--GridTemplateColumns--min: 33%;"
    ]

galleryItem :: Array Nut -> Nut
galleryItem =
  DD.div [ DA.klass_ "pf-v5-l-gallery__item" ]

dlistGroup :: String -> Poll String -> Nut
dlistGroup name termPoll =
  DD.div [ DA.klass_ "pf-v5-c-description-list__group" ]
    [ DD.dt [ DA.klass_ "pf-v5-c-description-list__term" ]
        [ DD.span [ DA.klass_ "pf-v5-c-description-list__text" ] [ DC.text_ name ] ]
    , DD.dd [ DA.klass_ "pf-v5-c-description-list__description" ]
        [ DD.div [ DA.klass_ "pf-v5-c-description-list__text" ]
            [ DC.text termPoll ]
        ]
    ]

makeTextInputForm :: String -> Poll String -> (String -> Effect Unit) -> Nut
makeTextInputForm name content setContent =
  DD.div [ DA.klass_ "pf-v5-c-form__group" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ name <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ name ]
            , DC.text_ "   "
            , DD.span [ DA.klass_ "pf-v5-c-form__label-help", DA.role_ "button" ]
                [ DD.i [ DA.klass_ "pf-v5-pficon pf-v5-pficon-help" ] []
                ]
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ DD.span [ DA.klass_ "pf-v5-c-form-control pf-m-required" ]
            [ DD.input
                [ DA.required_ ""
                , DA.id_ $ name <> "-form"
                , DA.value content
                , DL.change_ updateContent
                ]
                []
            ]
        ]
    ]
  where
  updateContent event =
    case currentTarget event >>= Input.fromEventTarget >>= (pure <<< Input.value) of
      Nothing -> pure unit
      Just effString -> effString >>= setContent

makeNumberInputForm :: String -> Poll String -> (String -> Effect Unit) -> Nut
makeNumberInputForm name content setContent =
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

makeRadio :: String -> Poll Boolean -> Effect Unit -> Nut
makeRadio label statePoll onChecked =
  DD.div [ DA.klass_ "pf-v5-c-radio" ]
    [ DD.input
        [ DA.klass_ "pf-v5-c-radio__input"
        , DA.xtype_ "radio"
        , DA.checked $ statePoll <#> if _ then "true" else ""
        , DL.change_ $ \_ -> onChecked
        ]
        []
    , DD.label
        [ DA.klass_ "pf-v5-c-radio__label"
        , DL.click_ $ \_ -> onChecked
        ]
        [ DC.text_ label ]
    ]

popover :: Nut
popover =
  DD.div
    [ DA.klass_ "pf-v5-c-popover pf-m-right"
    , DA.role_ "dialog"
    ]
    [ DD.div [ DA.klass_ "pf-v5-c-popover__arrow" ] []
    , DD.div [ DA.klass_ "pf-v5-c-popover__content" ]
        [ --Popover close
          --Popover header,
          DD.div [ DA.klass_ "pf-v5-c-popover__body" ]
            [ DC.text_ "Body!" ]
        ]
    ]
