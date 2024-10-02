module Main where

import Content (pageContent)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Deku.Control as DC
import Deku.Core (Nut)
import Deku.DOM as DD
import Deku.DOM.Attributes (klass, klass_, role_) as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Effect as DE
import Deku.Hooks ((<#~>))
import Deku.Hooks as DH
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Console as Console
import FRP.Poll (Poll)
import Prelude (Unit, bind, discard, pure, show, unit, ($), (<#>), (<>), (==))

main :: Effect Unit
main = do
  app <- dekuApp
  _ <- runInBody app
  pure unit

header :: Nut
header =
  DD.header [ DA.klass_ "pf-v5-c-masthead" ]
    [ DD.div [ DA.klass_ "pf-v5-c-masthead__main" ]
        [ DD.h2 [ DA.klass_ "pf-v5-c-title pf-m-xl" ]
            [ DC.text_ "Coffee"
            ]
        ]
    , DD.div [ DA.klass_ "pf-v5-c-masthead__content" ]
        [
        ]
    ]

pageBody :: Nut
pageBody =
  DD.main [ DA.klass_ "pf-v5-c-page__main" ]
    [ DD.section [ DA.klass_ "pf-v5-c-page__main-section" ]
        [ pageContent ]
    ]

dekuApp :: Effect Nut
dekuApp = do

  pure Deku.do
    DD.div [ DA.klass_ "pf-v5-c-page" ]
      [ header
      , pageBody
      ]
