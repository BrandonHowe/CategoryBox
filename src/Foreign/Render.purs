module CategoryBox.Foreign.Render where

import Prelude

import CategoryBox.Foreign.ForeignAction (ForeignAction, ForeignActionConfig)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError)
import Data.Default (def)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn4, runFn1, runFn2, runFn4)
import Effect (Effect)
import React.SyntheticEvent (SyntheticMouseEvent, SyntheticWheelEvent)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Context2d :: Type

instance showCtx :: Show Context2d where
  show = showContext2d

foreign import data GeometryCache :: Type

instance showCache :: Show GeometryCache where
  show = showGeometryCache

instance decodeJsonCache :: DecodeJson GeometryCache where
  decodeJson = Right <<< unsafeCoerce

foreign import emptyGeometryCache :: Effect GeometryCache

foreign import renderCanvas :: Context2d -> GeometryCache -> Effect Unit

-- | Type of event handlers for the Scene component.
type NativeGeomMouseEventHandler
  = Fn4 ForeignActionConfig Context2d SyntheticMouseEvent GeometryCache (Effect ForeignAction)

type NativeGeomWheelEventHandler
  = Fn4 ForeignActionConfig Context2d SyntheticWheelEvent GeometryCache (Effect ForeignAction)

type GeomMouseEventHandler
  = Context2d -> SyntheticMouseEvent -> GeometryCache -> Effect ForeignAction

type GeomWheelEventHandler
  = Context2d -> SyntheticWheelEvent -> GeometryCache -> Effect ForeignAction

foreign import handleScrollImpl :: NativeGeomWheelEventHandler
foreign import handleMouseUpImpl :: NativeGeomMouseEventHandler
foreign import handleMouseDownImpl :: NativeGeomMouseEventHandler
foreign import handleMouseMoveImpl :: NativeGeomMouseEventHandler
foreign import createObjectImpl :: Fn4 GeometryCache Int Int String GeometryCache
foreign import createMorphismImpl :: Fn4 GeometryCache Int Int String GeometryCache
foreign import startMorphismImpl :: Fn2 GeometryCache Int GeometryCache
foreign import startDraggingImpl :: Fn2 GeometryCache Int GeometryCache
foreign import startComposingImpl :: Fn2 GeometryCache Int GeometryCache
foreign import stopDraggingImpl :: Fn1 GeometryCache GeometryCache
foreign import showGeometryCache :: Fn1 GeometryCache String
foreign import showContext2d :: Fn1 Context2d String
foreign import decodeJsonGeometryCache :: Fn1 Json (Either JsonDecodeError GeometryCache)

-- | Purescript functions which run the foreign functions.
createForeignObject :: GeometryCache -> Int -> Int -> String -> GeometryCache
createForeignObject = runFn4 createObjectImpl

createForeignMorphism :: GeometryCache -> Int -> Int -> String -> GeometryCache
createForeignMorphism = runFn4 createMorphismImpl

startMorphism :: GeometryCache -> Int -> GeometryCache
startMorphism = runFn2 startMorphismImpl

startDragging :: GeometryCache -> Int -> GeometryCache
startDragging = runFn2 startDraggingImpl

startComposing :: GeometryCache -> Int -> GeometryCache
startComposing = runFn2 startComposingImpl

stopDragging :: GeometryCache -> GeometryCache
stopDragging = runFn1 stopDraggingImpl

handleMouseDown :: GeomMouseEventHandler
handleMouseDown = runFn4 handleMouseDownImpl def

handleMouseMove :: GeomMouseEventHandler
handleMouseMove = runFn4 handleMouseMoveImpl def

handleMouseUp :: GeomMouseEventHandler
handleMouseUp = runFn4 handleMouseUpImpl def

handleScroll :: GeomWheelEventHandler
handleScroll = runFn4 handleScrollImpl def

foreign import resizeCanvas :: El -> Effect Unit

foreign import getContext :: forall a. Widget HTML a -> Effect Context2d