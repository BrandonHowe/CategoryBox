module Category.Main where

import Category.Types
import Data.Maybe
import Prelude

import Category.CategoryEquivalence (categoriesEquivalent)
import Data.Array (snoc)
import Data.Foldable (foldl)
import Data.Newtype (over, under, unwrap)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)

createMorphism :: Object -> Object -> String -> Morphism
createMorphism a b name = Morphism ({ from: a, to: b, name: name })

composeMorphisms :: Morphism -> Morphism -> Maybe Morphism
composeMorphisms (Morphism f) (Morphism g) = if f.to == g.from then Just (Morphism { from: f.from, to: g.to, name: f.name <> " o " <> g.name }) else Nothing

emptyCategory :: Category
emptyCategory =
  { objects: []
  , morphisms: []
  , name: ""
  }

createFunctor :: Category -> Category -> String -> Boolean -> Maybe CFunctor
createFunctor c d name contra = if categoriesEquivalent c d then Just $ CFunctor { from: c, to: d, name: name, contravariant: contra } else Nothing

createFunctorAutomatic :: Category -> String -> Boolean -> World -> World
createFunctorAutomatic c name contra world = world { categories = snoc world.categories newCategory, functors = snoc world.functors newFunctor }
  where

    newCategory :: Category
    newCategory =
      { objects: c.objects <#> convertObject
      , morphisms: c.morphisms <#> over Morphism (\x -> (under Morphism (flip flipContra contra) x) { from = convertObject x.from, to = convertObject x.to, name = name <> "(" <> x.name <> ")" })
      , name: name <> "(" <> c.name <> ")"
      }

    -- | This is safe because the new category is guaranteed to have the same structure
    newFunctor :: CFunctor
    newFunctor = unsafePartial $ fromJust $ createFunctor c newCategory name contra

    flipContra :: Morphism -> Boolean -> Morphism
    flipContra f con = Morphism $ (unwrap f) { from = if con then to else from, to = if con then from else to }
      where
        to = unwrap >>> _.to $ f
        from = unwrap >>> _.from $ f

    convertObject :: Object -> Object
    convertObject = over Object (\x -> name <> "(" <> x <> ")")

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
