module CategoryBox.Foreign.ForeignAction where

import Data.Default (class Default)
import Data.Function.Uncurried (Fn2, Fn3, Fn1, mkFn1, mkFn2, mkFn3)

-- | Stuff the ts side of things can tell us to do
data ForeignAction
  = CreateObject Int Int String
  | GetObjectName Int Int
  | CreateMorphism Int Int String
  | GetMorphismName Int Int
  | ComposeMorphisms Int Int String
  | GetCompositionName Int Int
  | StartMorphism Int
  | StartDragging Int
  | StopDragging
  | NoAction

newtype ForeignActionConfig
  = ForeignActionConfig
  { createObject :: Fn3 Int Int String ForeignAction
  , getObjectName :: Fn2 Int Int ForeignAction
  , createMorphism :: Fn3 Int Int String ForeignAction
  , getMorphismName :: Fn2 Int Int ForeignAction
  , composeMorphisms :: Fn3 Int Int String ForeignAction
  , getCompositionName :: Fn2 Int Int ForeignAction
  , startMorphism :: Fn1 Int ForeignAction
  , startDragging :: Fn1 Int ForeignAction
  , stopDragging :: ForeignAction
  , nothing :: ForeignAction
  }


instance defaultForeignActionConfig :: Default ForeignActionConfig where
  def =
    ForeignActionConfig
      { createObject: mkFn3 CreateObject
      , getObjectName: mkFn2 GetObjectName
      , createMorphism: mkFn3 CreateMorphism
      , getMorphismName: mkFn2 GetMorphismName
      , composeMorphisms: mkFn3 ComposeMorphisms
      , getCompositionName: mkFn2 GetCompositionName
      , startMorphism: mkFn1 StartMorphism
      , startDragging: mkFn1 StartDragging
      , stopDragging: StopDragging
      , nothing: NoAction
      }