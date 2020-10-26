module Main where

import Data.Exists
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)
import Render (render)

import Category.Main

main :: Effect Unit
main = do
  logShow $ myMorphism2 `composeMorphisms` myMorphism1
  logShow $ myMorphism1 `composeMorphisms` myMorphism3
  logShow $ isMorphismInCategory myCategory (Just myObject1) (Just myObject3)
  logShow $ isIdentity <$> createMorphism myObject2 myObject1 `composeMorphisms` myMorphism1
  render
  where
    myObject1 = Object "Blah"
    myObject2 = Object "5"
    myObject3 = Object "bloo"
    myObject4 = Object "20"
    myMorphism1 = createMorphism myObject1 myObject2
    myMorphism2 = createMorphism myObject2 myObject3
    myMorphism3 = createMorphism myObject4 myObject1
    myCategory =
      { objects: [myObject1, myObject2, myObject3, myObject4]
      , morphisms : [myMorphism1, myMorphism2, myMorphism3]
      }
