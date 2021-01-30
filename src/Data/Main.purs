module CategoryBox.Data.Main where

import CategoryBox.Data.Types
import Data.Maybe
import Prelude

import CategoryBox.Data.CategoryEquivalence (categoriesEquivalent)
import Data.Array (snoc, union)
import Data.Foldable (foldl)
import Data.Newtype (over, under, unwrap)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)

-- | Given two objects and a name, create a morphism
createMorphism :: Object -> Object -> String -> Morphism
createMorphism a b name = { from: a, to: b, name: name }

-- | Compose two morphisms with <<<
-- | ```purescript
-- | composeMorphisms { from: b, to: c } { from: a, to: b } == { from: a, to: c }
-- | ````
composeMorphisms :: Morphism -> Morphism -> Maybe Morphism
composeMorphisms g f = if f.to == g.from then Just { from: f.from, to: g.to, name: f.name <> " o " <> g.name } else Nothing

-- | Empty category with a blank name
emptyCategory :: Category
emptyCategory =
  { objects: []
  , morphisms: []
  , name: ""
  }

-- | Given two categories, a name, and a contravariance, create a functor if the two categories are compatible, otherwise return `Nothing`
createFunctor :: Category -> Category -> String -> Boolean -> Maybe CFunctor
createFunctor c d name contra = if categoriesEquivalent c d then Just $ CFunctor { from: c, to: d, name: name, contravariant: contra } else Nothing

createFunctorAutomatic :: Category -> String -> Boolean -> World -> World
createFunctorAutomatic c name contra world = world { categories = snoc world.categories newCategory, functors = snoc world.functors newFunctor }
  where

    newCategory :: Category
    newCategory =
      { objects: c.objects <#> convertObject
      , morphisms: c.morphisms <#> \x -> flip flipContra contra x { from = convertObject x.from, to = convertObject x.to, name = name <> "(" <> x.name <> ")" }
      , name: name <> "(" <> c.name <> ")"
      }

    -- | This is safe because the new category is guaranteed to have the same structure
    newFunctor :: CFunctor
    newFunctor = unsafePartial $ fromJust $ createFunctor c newCategory name contra

    -- | Flip morphism if contravariant
    flipContra :: Morphism -> Boolean -> Morphism
    flipContra f con = f { from = if con then to else from, to = if con then from else to }
      where
        to = _.to $ f
        from = _.from $ f

    convertObject :: Object -> Object
    convertObject = over Object (\x -> name <> "(" <> x <> ")")

-- | Get all identity functors from a `World`.
getIdentityFunctors :: World -> Array CFunctor
getIdentityFunctors cat = (\x -> CFunctor { from: x, to: x, name: "id " <> x.name, contravariant: false }) <$> cat.categories

-- | Get the functor category from a `World`, the category with all functors as objects and all natural transformations as morphisms
getFunctorCategory :: World -> Category
getFunctorCategory world = { 
  objects: (unwrap <$> (union world.functors $ getIdentityFunctors world)) <#> \x -> Object $ x.name
  , morphisms: [], name: "Functor Category"
  }

-- | Given a category and two objects, decide if the morphism between the two objects is in the category.
isMorphismInCategory :: Category -> Maybe Object -> Maybe Object -> Boolean
isMorphismInCategory _ Nothing Nothing = true
isMorphismInCategory category (Just f) Nothing = foldl (\m n -> m || n.from == f) false category.morphisms
isMorphismInCategory category Nothing (Just g) = foldl (\m n -> m || n.to == g) false category.morphisms
isMorphismInCategory category (Just f) (Just g) = foldl (\m n -> m || n.from == f && n.to == g) false category.morphisms

-- | See if a morphism is an endomorphism.
isEndomorphism :: Morphism -> Boolean
isEndomorphism m = m.from == m.to

-- | See if morphism is identity morphism.
isIdentity :: Morphism -> Boolean
isIdentity = isEndomorphism

isSection :: Morphism -> Morphism -> Boolean
isSection f g = fromMaybe false (isIdentity <$> g `composeMorphisms` f)

isRetraction :: Morphism -> Morphism -> Boolean
isRetraction f g = fromMaybe false (isIdentity <$> f `composeMorphisms` g)

isIsomorphism :: Morphism -> Category -> Boolean
isIsomorphism m category = isMorphismInCategory category (Just $ m.to) (Just $ m.from)

isAutomorphism :: Morphism -> Category -> Boolean
isAutomorphism f category = isIsomorphism f category && isEndomorphism f
