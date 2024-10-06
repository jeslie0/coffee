module Patternfly where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FRP.Poll (Poll)
import Web.DOM.Element (getBoundingClientRect)
import Web.DOM.Node (fromEventTarget, parentElement)
import Web.Event.Event (currentTarget)
import Web.Event.Event as Event
import Web.HTML.HTMLInputElement as Input
import Web.UIEvent.MouseEvent (MouseEvent, screenX, toEvent)

gallery :: Array Nut -> Nut
gallery =
  DD.div
    [ DA.klass_ "pf-v5-l-gallery pf-m-gutter"
    , DA.style_ "--pf-v5-l-gallery--GridTemplateColumns--min-on-lg: 49%; --pf-v5-l-gallery--GridTemplateColumns--min: 50%;"
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

makeTextInputForm :: String -> String -> Poll String -> (String -> Effect Unit) -> Nut
makeTextInputForm name helperText content setContent =
  DD.div [ DA.klass_ "pf-v5-c-form__group", DA.style_ "max-width: 500px" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ name <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ name ]
            -- , DC.text_ "   "
            -- , DD.span [ DA.klass_ "pf-v5-c-form__label-help", DA.role_ "button" ]
            --     [ DD.i [ DA.klass_ "pf-v5-pficon pf-v5-pficon-help" ] []
            --     ]
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ DD.span
            [ DA.klass_ "pf-v5-c-form-control pf-m-required"
            ]
            [ DD.input
                [ DA.required_ ""
                , DA.id_ $ name <> "-form"
                , DA.value content
                , DL.change_ updateContent
                ]
                []
            ]
        , DD.div [ DA.klass_ "pf-v5-c-form__helper-text", DA.ariaLive_ "polite" ]
            [ DD.div [ DA.klass_ "pf-v5-c-helper-text" ]
                [ DD.div [ DA.klass_ "pf-v5-c-helper-text__item" ]
                    [ DD.span [ DA.klass_ "pf-v5-c-helper-text__item-text" ] [ DC.text_ helperText ] ]
                ]
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

tabItem :: String -> Poll Boolean -> Effect Unit -> Nut
tabItem title isActivePoll onClick =
  DD.li
    [ DA.klass $ pure "pf-v5-c-tabs__item" <> (isActivePoll <#> if _ then " pf-m-current" else "")
    , DA.role_ "presentation"
    ]
    [ DD.button
        [ DA.xtype_ "button"
        , DA.klass_ "pf-v5-c-tabs__link"
        , DA.role_ "tab"
        ]
        [ DD.span
            [ DA.klass_ "pf-v5-c-tabs__item-text"
            , DL.click_ $ \_ -> onClick
            ]
            [ DC.text_ title ]
        ]
    ]

makeSlider :: Array (Tuple String Number) -> Poll Number -> (Number -> Effect Unit) -> Nut
makeSlider stepData currentPercentPoll changePercent = Deku.do
  Tuple setIsMouseDown isMouseDown <- DH.useHot false
  isMouseDownEff <- DH.useRef false isMouseDown

  DD.div
    [ DA.klass_ "pf-v5-c-slider"
    , DA.style $ pure "--pf-v5-c-slider--value: " <> (show <$> currentPercentPoll) <> pure "%"
    , DL.mousemove_ $ onMouseMove isMouseDownEff
    , DL.mousedown_ $ \_ -> setIsMouseDown true
    , DL.mouseup_ $ \_ -> setIsMouseDown false
    , DL.mouseleave_ $ \_ -> setIsMouseDown false
    ]
    [ DD.div [ DA.klass_ "pf-v5-c-slider__main" ]
        [ DD.div [ DA.klass_ "pf-v5-c-slider__rail" ]
            [ DD.div [ DA.klass_ "pf-v5-c-slider__rail-track" ] [] ]
        , DD.div [ DA.klass_ "pf-v5-c-slider__steps" ] $
            stepData <#> uncurry sliderStep
        , DD.div
            [ DA.klass_ "pf-v5-c-slider__thumb"
            , DA.role_ "slider"
            ]
            []
        ]
    ]
  where
  sliderStep label percent =
    DD.div
      [ DA.klass_ "pf-v5-c-slider__step pf-m-active"
      , DA.style_ $ "--pf-v5-c-slider__step--Left: " <> show percent <> "%"
      ]
      [ DD.div [ DA.klass_ "pf-v5-c-slider__step-tick" ] []
      , DD.div [ DA.klass_ "pf-v5-c-slider__step-label" ] [ DC.text_ label ]
      ]

  onMouseMove :: Effect Boolean -> MouseEvent -> Effect Unit
  onMouseMove isMouseDownEff mouseEvent = do
    isMouseDown <- isMouseDownEff
    if not isMouseDown then pure unit
    else do
      -- Get parent div's bounding X range, then figure out how far
      -- along we are in percent. Round to nearest percent
      let
        event = toEvent mouseEvent
        mouseScreenX = screenX mouseEvent
      val <- getParentXRange event
      case val of
        Nothing -> pure unit
        Just range -> do
          let
            newPercent = rangePercent range (toNumber mouseScreenX)
            roundedPercent = min 100 (max 0 $ roundNeareastMultiple (100 / (Array.length stepData - 1)) newPercent)
          changePercent (toNumber roundedPercent)

makeSliderForm :: String -> Array (Tuple String Number) -> Poll Number -> (Number -> Effect Unit) -> Nut
makeSliderForm label stepData currentPercentPoll changePercent =
  DD.div [ DA.klass_ "pf-v5-c-form__group", DA.style_ "max-width: 500px" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ label <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ label ] ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ makeSlider stepData currentPercentPoll changePercent
        ]
    ]

getParentXRange :: Event.Event -> Effect (Maybe { left :: Number, right :: Number })
getParentXRange event =
  runMaybeT do
    targetEvent <- MaybeT <<< pure $ currentTarget event
    targetNode <- MaybeT <<< pure $ fromEventTarget targetEvent
    parElement <- MaybeT $ parentElement targetNode
    boundingRect <- lift $ getBoundingClientRect parElement
    pure { left: boundingRect.left, right: boundingRect.right }

rangePercent :: { left :: Number, right :: Number } -> Number -> Number
rangePercent { left, right } x =
  100.0 * (x - left) / (right - left)

roundNeareastMultiple :: Int -> Number -> Int
roundNeareastMultiple multiple val =
  multiple * round (val / toNumber multiple)

makeDropdown :: String -> Poll Boolean -> (Boolean -> Effect Unit) -> Poll String -> Poll (Array (Tuple String (Effect Unit))) -> Nut
makeDropdown label isOpenPoll setOpen labelPoll menuItems =
  DD.div [ DA.klass_ "pf-v5-c-form__group", DA.style_ "max-width: 500px" ]
    [ DD.div [ DA.klass_ "pf-v5-c-form__label" ]
        [ DD.label [ DA.klass_ "pf-v5-c-form__label", DA.for_ $ label <> "-form" ]
            [ DD.span [ DA.klass_ "pf-v5-c-form__label-text" ] [ DC.text_ label ] ]
        ]
    , DD.div [ DA.klass_ "pf-v5-form__group-control" ]
        [ DD.div
            [ DA.klass $ pure "pf-v5-c-dropdown" <> (isOpenPoll <#> if _ then " pf-m-expanded" else "")
            , DL.click $ isOpenPoll <#> if _ then (\_ -> setOpen false) else (\_ -> setOpen true)
            ]
            [ DD.button [ DA.klass_ "pf-v5-c-dropdown__toggle", DA.xtype_ "button" ]
                [ DD.span [ DA.klass_ "pf-v5-c-dropdown__toggle-text" ] [ DC.text labelPoll ]
                , DD.span [ DA.klass_ "pf-v5-c-dropdown__toggle-icon" ]
                    [ DD.i [ DA.klass_ "fas fa-caret-down" ] [] ]
                ]
            , menuItems <#~> \itemArr ->
                isOpenPoll <#~>
                  if _ then
                    DD.ul
                      [ DA.klass_ "pf-v5-c-dropdown__menu"
                      , DA.role_ "menu"
                      ] $
                      itemArr <#> uncurry makeMenuItem
                  else
                    DD.ul
                      [ DA.klass_ "pf-v5-c-dropdown__menu"
                      , DA.role_ "menu"
                      , DA.hidden_ ""
                      ] $
                      itemArr <#> uncurry makeMenuItem
            ]
        ]
    ]
  where
  makeMenuItem label handler =
    DD.li [ DA.role_ "none", DL.click_ $ \_ -> handler ]
      [ DD.a [ DA.klass_ "pf-v5-c-dropdown__menu-item", DA.role_ "menuitem" ] [ DC.text_ label ]
      ]
