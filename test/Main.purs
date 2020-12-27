module Test.Main where

import CategoryBox.Data.Main
import CategoryBox.Data.Types
import Data.Maybe
import Prelude

import CategoryBox.Data.CategoryEquivalence (categoriesEquivalent)
import CategoryBox.Helpers.CantorPairing (invertCantorPairing)
import CategoryBox.Tutorials.GetTutorial (Tutorial, getTutorialFromURL)
import Data.Bifunctor (bimap)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (logShow)

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
  logShow $ bimap (mul 100) (mul 100) $ invertCantorPairing 4
  launchAff_ tutorials
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
    myWorld = { categories: [ cat1, cat3 ], functors: [], name: "myWorld" }

tutorials :: Aff (Either String Tutorial)
tutorials = do
  getTutorialFromURL "https://gist.github.com/xWafl/b4541d2c738fd8726287cec39e0255a4"