module Main where

import Prelude

import CategoryBox.Router (renderRouter, routingCodec)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Routing.Duplex (parse)
import Routing.PushState (makeInterface, matchesWith)

main :: Effect Unit
main = do
  nav <- makeInterface
  canceller <- nav # matchesWith (parse routingCodec) (\_ x -> runWidgetInDom "app" $ renderRouter x)
  pure unit