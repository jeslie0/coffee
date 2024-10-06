module Main where

import Prelude

import Api (Api, getExistingCoffees, makeApi)
import Constants (version)
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect as DE
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Ref (Ref)
import FRP.Poll (Poll)
import Page (Page(..), pageList)
import Page.CoffeeTasting (coffeeTastingPage)
import Page.CurrentCoffee (currentCoffeePage)
import Page.Information (informationPage)
import Page.PreviousResults (previousResultsPage)

main :: Effect Unit
main = do
  api <- makeApi
  app <- dekuApp api
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
        [ DD.div [ DA.klass_ "pf-v5-l-flex", DA.style_ "width: 100%;" ]
            [ DD.div [ DA.klass_ "pf-v5-l-flex__item" ]
                [ DD.nav [ DA.klass_ "pf-v5-c-nav pf-m-horizontal" ]
                    [ DD.ul [ DA.klass_ "pf-v5-c-nav__list", DA.role_ "list" ]
                        navList
                    ]
                ]
            , DD.div [ DA.klass_ "pf-v5-l-flex__item pf-m-align-right" ] [ DC.text_ version ]
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

pageBody :: Ref Api -> Poll (Array String) -> Poll Page -> Nut
pageBody api coffeeList pagePoll =
  DD.main [ DA.klass_ "pf-v5-c-page__main" ]
    [ DD.section [ DA.klass_ "pf-v5-c-page__main-section" ]
        [ pagePoll <#~>
            case _ of
              CoffeeTasting -> coffeeTastingPage api coffeeList

              CurrentCoffee -> currentCoffeePage

              PreviousResults -> previousResultsPage

              CoffeeInfo -> informationPage
        ]
    ]

dekuApp :: Ref Api -> Effect Nut
dekuApp api = do
  setCoffeeList /\ coffeeList <- DE.useState []
  getExistingCoffees api setCoffeeList
  pure Deku.do
    setActivePage /\ activePage <- DH.useState CoffeeTasting
    DD.div [ DA.klass_ "pf-v5-c-page" ]
      [ header activePage setActivePage
      , pageBody api coffeeList activePage
      ]
