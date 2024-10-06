module SVG.CheckCircle (checkCircle) where

import Prelude
import Deku.Core (Nut, attributeAtYourOwnRisk)
import Deku.DOM.Attributes as DDA
import Deku.DOM.SVG as DD
import Deku.DOM.SVG.Attributes as DA

checkCircle :: Nut
checkCircle =
  DD.svg
    [ DA.klass_ "pf-v5-svg"
    , DA.viewBox_ "0 0 512 512"
    , DA.fill_ "currentColor"
    , DDA.role_ "img"
    , DA.width_ "100%"
    , DA.height_ "100%"
    , pure $ attributeAtYourOwnRisk "color" "var(--pf-v5-global--success-color--100)"
    ]
    [ DD.path [ DA.d_ "M504 256c0 136.967-111.033 248-248 248S8 392.967 8 256 119.033 8 256 8s248 111.033 248 248zM227.314 387.314l184-184c6.248-6.248 6.248-16.379 0-22.627l-22.627-22.627c-6.248-6.249-16.379-6.249-22.628 0L216 308.118l-70.059-70.059c-6.248-6.248-16.379-6.248-22.628 0l-22.627 22.627c-6.248 6.248-6.248 16.379 0 22.627l104 104c6.249 6.249 16.379 6.249 22.628.001z" ] []
    ]
