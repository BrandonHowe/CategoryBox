module Main where

import Data.Exists
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Console (logShow)
import Render (render)

newtype Object = Object String

derive instance eqObject :: Eq Object

instance showObject :: Show Object where
  show (Object a) = a 

newtype Morphism = Morphism (Tuple Object Object)

createMorphism :: Object -> Object -> Morphism
createMorphism a b = Morphism (Tuple a b)

instance showMorphism :: Show Morphism where
  show (Morphism (Tuple a b)) = show a <> " -> " <> show b

composeMorphisms :: Morphism -> Morphism -> Maybe Morphism
composeMorphisms (Morphism (Tuple g1 g2)) (Morphism (Tuple f1 f2)) = if f2 == g1 then Just (Morphism (Tuple f1 g2)) else Nothing

type Category =
  { objects :: Array Object
  , morphisms :: Array Morphism
  }

isMorphismInCategory :: Category -> Maybe Object -> Maybe Object -> Boolean
isMorphismInCategory _ Nothing Nothing = true
isMorphismInCategory category (Just f) Nothing = foldl (\m (Morphism n) -> m || fst n == f) false category.morphisms
isMorphismInCategory category Nothing (Just g) = foldl (\m (Morphism n) -> m || snd n == g) false category.morphisms
isMorphismInCategory category (Just f) (Just g) = foldl (\m (Morphism n) -> m || fst n == f && snd n == g) false category.morphisms

isEndomorphism :: Morphism -> Boolean
isEndomorphism (Morphism f) = fst f == snd f

isIdentity :: Morphism -> Boolean
isIdentity = isEndomorphism

isSection :: Morphism -> Morphism -> Boolean
isSection f g = fromMaybe false (isIdentity <$> g `composeMorphisms` f)

isRetraction :: Morphism -> Morphism -> Boolean
isRetraction f g = fromMaybe false (isIdentity <$> f `composeMorphisms` g)

isIsomorphism :: Morphism -> Category -> Boolean
isIsomorphism (Morphism f) category = isMorphismInCategory category (Just $ snd f) (Just $ fst f)

isAutomorphism :: Morphism -> Category -> Boolean
isAutomorphism f category = isIsomorphism f category && isEndomorphism f

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
