module Category.Main where

import Data.Exists
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Foldable (foldl)
import Data.Newtype (class Newtype)

newtype Object = Object String

derive instance eqObject :: Eq Object

derive instance newtypeObject :: Newtype Object _

instance showObject :: Show Object where
  show (Object a) = a 

newtype Morphism = Morphism 
  { from :: Object
  , to :: Object
  , name :: String
  }

derive instance eqMorphism :: Eq Morphism

derive instance newtypeMorphism :: Newtype Morphism _

createMorphism :: Object -> Object -> String -> Morphism
createMorphism a b name = Morphism ({ from: a, to: b, name: name })

instance showMorphism :: Show Morphism where
  show (Morphism f) = show f.from <> " -> " <> show f.to

composeMorphisms :: Morphism -> Morphism -> Maybe Morphism
composeMorphisms (Morphism f) (Morphism g) = if f.to == g.from then Just (Morphism { from: f.from, to: g.to, name: f.name <> " o " <> g.name }) else Nothing

type Category =
  { objects :: Array Object
  , morphisms :: Array Morphism
  }

emptyCategory :: Category
emptyCategory =
  { objects: []
  , morphisms: []
  }

type World =
  { categories :: Array Category
  , functors :: Array CFunctor
  }

newtype CFunctor = CFunctor
  { from :: Category
  , to :: Category
  , name :: String
  , contravariant :: Boolean
  }

isMorphismInCategory :: Category -> Maybe Object -> Maybe Object -> Boolean
isMorphismInCategory _ Nothing Nothing = true
isMorphismInCategory category (Just f) Nothing = foldl (\m (Morphism n) -> m || n.from == f) false category.morphisms
isMorphismInCategory category Nothing (Just g) = foldl (\m (Morphism n) -> m || n.to == g) false category.morphisms
isMorphismInCategory category (Just f) (Just g) = foldl (\m (Morphism n) -> m || n.from == f && n.to == g) false category.morphisms

isEndomorphism :: Morphism -> Boolean
isEndomorphism (Morphism m) = m.from == m.to

isIdentity :: Morphism -> Boolean
isIdentity = isEndomorphism

isSection :: Morphism -> Morphism -> Boolean
isSection f g = fromMaybe false (isIdentity <$> g `composeMorphisms` f)

isRetraction :: Morphism -> Morphism -> Boolean
isRetraction f g = fromMaybe false (isIdentity <$> f `composeMorphisms` g)

isIsomorphism :: Morphism -> Category -> Boolean
isIsomorphism (Morphism m) category = isMorphismInCategory category (Just $ m.to) (Just $ m.from)

isAutomorphism :: Morphism -> Category -> Boolean
isAutomorphism f category = isIsomorphism f category && isEndomorphism f
