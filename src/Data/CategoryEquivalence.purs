module Category.CategoryEquivalence where

import Prelude

import Category.Types (Category, Morphism(..), Object(..))
import Data.Array (delete, (:), snoc, zip)
import Data.Foldable (find, foldl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple, fst, snd)

-- | Tuple of (old name, new name)
type ObjectReplacements = Array (Tuple String String)

replaceCategoryObjects :: Category -> ObjectReplacements -> Category
replaceCategoryObjects category replacements = category
    { objects = updatedObjects category replacements
    , morphisms = updatedMorphisms category replacements 
    }
  where
    findMatchingReplacement :: ObjectReplacements -> String -> Maybe Object
    findMatchingReplacement r match = Object <$> (snd <$> find (fst >>> (==) match) r)

    updatedObjects :: Category -> ObjectReplacements -> Array Object
    updatedObjects cat repl = (\x -> fromMaybe x (findMatchingReplacement repl $ unwrap x)) <$> cat.objects

    updatedMorphisms :: Category -> ObjectReplacements -> Array Morphism
    updatedMorphisms cat repl = (\(Morphism x) -> Morphism $ x 
      { from = fromMaybe x.from (findMatchingReplacement repl $ unwrap x.from)
      , to = fromMaybe x.to (findMatchingReplacement repl $ unwrap x.to) 
      } ) <$> cat.morphisms

categoriesEquivalent :: Category -> Category -> Boolean
categoriesEquivalent c d = foldl (\acc cur -> acc || categoriesEqual cur d) false (allObjectOrderings =<< (replaceCategoryObjects c <$> allObjectReplacements c d))
  where
    allObjectOrderings = (\x -> x { objects = _ } <$> permutations x.objects)

    permutations :: forall a. Eq a => Array a -> Array (Array a)
    permutations [] = [[]]
    permutations as = do
      a <- as
      ls <- permutations $ delete a as
      pure $ a : ls

    objectsArr :: Category -> Array String
    objectsArr cat = unwrap <$> cat.objects

    allObjectReplacements :: Category -> Category -> Array ObjectReplacements
    allObjectReplacements cat cat2 = foldl (\acc cur -> snoc acc $ zip objects cur) [] (permutations $ objectsArr cat2)
      where
        objects :: Array String
        objects = objectsArr cat
        
    morphismsEqual :: Tuple Morphism Morphism -> Boolean
    morphismsEqual t = m1.from == m2.from && m1.to == m2.to
      where
        m1 = (fst >>> unwrap) t
        m2 = (snd >>> unwrap) t

    categoriesEqual :: Category -> Category -> Boolean
    categoriesEqual cat1 cat2 = cat1.objects == cat2.objects && not (foldl (\acc cur -> acc || not morphismsEqual cur) false (zip cat1.morphisms cat2.morphisms))
