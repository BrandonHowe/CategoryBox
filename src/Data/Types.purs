module Category.Types where

import Prelude

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

instance showMorphism :: Show Morphism where
  show (Morphism f) = f.name <> ": " <> show f.from <> " -> " <> show f.to

type Category =
  { objects :: Array Object
  , morphisms :: Array Morphism
  , name :: String
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

derive instance newtypeFunctor :: Newtype CFunctor _

instance eqFunctor :: Eq CFunctor where
  eq (CFunctor f) (CFunctor g) = f.from == g.from && f.to == g.to && f.contravariant == g.contravariant

instance showFunctor :: Show CFunctor where
  show (CFunctor f) = f.name <> ": " <> f.from.name <> " -> " <> f.to.name