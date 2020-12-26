module Main where

import CategoryBox.Render (render)
import Data.Unit (Unit)
import Effect (Effect)

main :: Effect Unit
main = render
  