module Main where

import CategoryBox.Data.Main
import CategoryBox.Data.Types
import Data.Exists
import Data.Maybe
import Data.Tuple
import Prelude

import CategoryBox.Data.CategoryEquivalence (categoriesEquivalent)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)
import Render (render)

main :: Effect Unit
main = do
  logShow $ myMorphism2 `composeMorphisms` myMorphism1
  logShow $ myMorphism1 `composeMorphisms` myMorphism3
  logShow $ isMorphismInCategory myCategory (Just myObject1) (Just myObject3)
  logShow $ isIdentity <$> createMorphism myObject2 myObject1 "i" `composeMorphisms` myMorphism1
  logShow $ categoriesEquivalent cat1 cat2
  logShow $ cat1
  logShow $ createFunctor cat1 cat3 "F" false
  logShow $ createFunctor cat1 cat2 "G" false
  logShow $ createFunctorAutomatic cat1 "F" false myWorld
  render
  where
    myObject1 = Object "Blah"
    myObject2 = Object "5"
    myObject3 = Object "bloo"
    myObject4 = Object "20"
    myMorphism1 = createMorphism myObject1 myObject2 "f"
    myMorphism2 = createMorphism myObject2 myObject3 "g"
    myMorphism5 = createMorphism myObject3 myObject2 "p"
    myMorphism3 = createMorphism myObject4 myObject1 "h"
    myMorphism4 = createMorphism myObject3 myObject4 "f"
    myCategory =
      { objects: [myObject1, myObject2, myObject3, myObject4]
      , morphisms : [myMorphism1, myMorphism2, myMorphism3]
      , name: "A"
      }
    cat1 = 
      { objects: [ myObject1, myObject2 ,myObject3 ]
      , morphisms: [ myMorphism1, myMorphism2 ]
      , name: "B"
      }
    cat2 = 
      { objects: [ myObject2, myObject3, myObject4 ]
      , morphisms: [ myMorphism2, myMorphism4 ]
      , name: "C"
      }
    cat3 = 
      { objects: [ myObject1, myObject2 ,myObject3 ]
      , morphisms: [ myMorphism1, myMorphism5 ]
      , name: "D"
      }
    myWorld = { categories: [ cat1, cat3 ], functors: [] }