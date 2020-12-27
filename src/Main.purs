module Main where

import Prelude

import CategoryBox.Render (render)
import CategoryBox.Tutorials.GetTutorial (getTutorialFromURL)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)

main :: Effect Unit
main = do
  launchAff_ $ liftEffect <<< logShow =<< getTutorialFromURL "https://api.github.com/gists/b4541d2c738fd8726287cec39e0255a4"
  render
  