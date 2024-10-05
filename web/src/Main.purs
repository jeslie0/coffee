module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Poll (Poll)
import Page (Page(..), pageList)
import Page.CoffeeTasting (coffeeTastingPage)
import Page.CurrentCoffee (currentCoffeePage)
import Page.Information (informationPage)
import Page.PreviousResults (previousResultsPage)

main :: Effect Unit
main = do
  app <- dekuApp
  _ <- runInBody app
  pure unit

header :: Poll Page -> (Page -> Effect Unit) -> Nut
header ev setPage =
  DD.header [ DA.klass_ "pf-v5-c-masthead" ]
    [ DD.div [ DA.klass_ "pf-v5-c-masthead__main" ]
        [ DD.h2 [ DA.klass_ "pf-v5-c-title pf-m-xl" ]
            [ DC.text_ "Coffee"
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-masthead__content" ]
        [ DD.nav [ DA.klass_ "pf-v5-c-nav pf-m-horizontal" ]
            [ DD.ul [ DA.klass_ "pf-v5-c-nav__list", DA.role_ "list" ]
                navList
            ]
        ]
    ]
  where
  navList = pageList <#> \page ->
    DD.li
      [ DA.klass $ ev <#> \activePage ->
          "pf-v5-c-nav__link" <> if page == activePage then " pf-m-current" else ""
      , DL.click_ $ \_ -> setPage page
      ]
      [ DC.text_ $ show page ]

pageBody :: Poll Page -> Nut
pageBody pagePoll =
  DD.main [ DA.klass_ "pf-v5-c-page__main" ]
    [ DD.section [ DA.klass_ "pf-v5-c-page__main-section" ]
        [ pagePoll <#~>
            case _ of
              CoffeeTasting -> coffeeTastingPage

              CurrentCoffee -> currentCoffeePage

              PreviousResults -> previousResultsPage

              CoffeeInfo -> informationPage
        ]
    ]

dekuApp :: Effect Nut
dekuApp =
  pure Deku.do
    setActivePage /\ activePage <- DH.useState CoffeeTasting
    DD.div [ DA.klass_ "pf-v5-c-page" ]
      [ header activePage setActivePage
      , pageBody activePage
      ]
