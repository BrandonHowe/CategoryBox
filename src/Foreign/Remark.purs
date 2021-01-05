module CategoryBox.Foreign.Remark where

import Prelude

import Concur.React.DOM (El, el')
import React (ReactClass, unsafeCreateElement)
import React.DOM.Props (unsafeFromPropsArray)

foreign import reactMarkdownImpl :: forall a. ReactClass a

reactMarkdown :: El
reactMarkdown = el' (unsafeCreateElement reactMarkdownImpl <<< unsafeFromPropsArray)