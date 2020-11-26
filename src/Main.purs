module Main where

import Category.Main
import Data.Exists
import Data.Maybe
import Data.Tuple
import Prelude

import Category.CategoryEquivalence (categoriesEqual, categoriesEquivalent)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)
import Render (render)

main :: Effect Unit
main = do
  logShow $ myMorphism2 `composeMorphisms` myMorphism1
  logShow $ myMorphism1 `composeMorphisms` myMorphism3
  logShow $ isMorphismInCategory myCategory (Just myObject1) (Just myObject3)
  logShow $ isIdentity <$> createMorphism myObject2 myObject1 "I" `composeMorphisms` myMorphism1
  logShow $ categoriesEquivalent cat1 cat2
  where
    myObject1 = Object "Blah"
    myObject2 = Object "5"
    myObject3 = Object "bloo"
    myObject4 = Object "20"
    myMorphism1 = createMorphism myObject1 myObject2 "F"
    myMorphism2 = createMorphism myObject2 myObject3 "G"
    myMorphism5 = createMorphism myObject3 myObject2 "P"
    myMorphism3 = createMorphism myObject4 myObject1 "H"
    myMorphism4 = createMorphism myObject3 myObject4 "F"
    myCategory =
      { objects: [myObject1, myObject2, myObject3, myObject4]
      , morphisms : [myMorphism1, myMorphism2, myMorphism3]
      }
    cat1 = 
      { objects: [ myObject1, myObject2 ,myObject3 ]
      , morphisms: [ myMorphism1, myMorphism2 ]
      }
    cat2 = 
      { objects: [ myObject2, myObject3, myObject4 ]
      , morphisms: [ myMorphism2, myMorphism4 ]
      }
    cat3 = 
      { objects: [ myObject1, myObject2 ,myObject3 ]
      , morphisms: [ myMorphism1, myMorphism5 ]
      }