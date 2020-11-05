module Render where

import Prelude
import Prim

import Category.Main (Category, Object(..), createMorphism, emptyCategory)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El, text)
import Concur.React.DOM as D
import Concur.React.Props (onMouseDown, onMouseMove, onMouseUp)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Apply (applyFirst)
import Data.Array (length, singleton, snoc, (!!))
import Data.Default (class Default, def)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn4, runFn3, runFn2, runFn1, mkFn3, mkFn2, mkFn1)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import React.Ref (NativeNode, Ref)
import React.Ref as Ref
import React.SyntheticEvent (SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)

-- | Stuff the ts side of things can tell us to do
data ForeignAction
  = CreateObject Int Int String
  | CreateMorphism Int Int
  | StartMorphism Int
  | StartDragging Int
  | StopDragging
  | NoAction

newtype ForeignActionConfig
  = ForeignActionConfig
  { createObject :: Fn3 Int Int String ForeignAction
  , createMorphism :: Fn2 Int Int ForeignAction
  , startMorphism :: Fn1 Int ForeignAction
  , startDragging :: Fn1 Int ForeignAction
  , stopDragging :: ForeignAction
  , nothing :: ForeignAction
  }


instance defaultForeignActionConfig :: Default ForeignActionConfig where
  def =
    ForeignActionConfig
      { createObject: mkFn3 CreateObject
      , createMorphism: mkFn2 CreateMorphism
      , startMorphism: mkFn1 StartMorphism
      , startDragging: mkFn1 StartDragging
      , stopDragging: StopDragging
      , nothing: NoAction
      }

handleForeignAction :: Category -> GeometryState -> ForeignAction -> Tuple Category GeometryState
handleForeignAction category geom action = case action of
  CreateObject posX posY name -> Tuple (category { objects = snoc category.objects (Object name) }) $ geom { geometryCache = createForeignObject geom.geometryCache posX posY name }
  CreateMorphism idx1 idx2 -> 
    let obj1 = category.objects !! idx1
        obj2 = category.objects !! idx2
        newMorphism = createMorphism <$> obj1 <*> obj2
    in Tuple (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton newMorphism) }) $ geom { geometryCache = createForeignMorphism geom.geometryCache idx1 idx2 }
  StartMorphism idx -> Tuple category ( geom { geometryCache = startMorphism geom.geometryCache idx })
  StartDragging idx -> Tuple category ( geom { geometryCache = startDragging geom.geometryCache idx })
  StopDragging -> Tuple category ( geom { geometryCache = stopDragging geom.geometryCache })
  NoAction -> Tuple category geom

foreign import data Context2d :: Type

foreign import data GeometryCache :: Type

foreign import emptyGeometryCache :: Effect GeometryCache

foreign import renderCanvas :: Context2d -> GeometryCache -> Effect Unit

-- | Type of event handlers for the Scene component.
type NativeGeomEventHandler
  = Fn4 ForeignActionConfig Context2d SyntheticMouseEvent GeometryCache (Effect ForeignAction)

type GeomEventHandler
  = Context2d -> SyntheticMouseEvent -> GeometryCache -> Effect ForeignAction

foreign import handleMouseUpImpl :: NativeGeomEventHandler

foreign import handleMouseDownImpl :: NativeGeomEventHandler

foreign import handleMouseMoveImpl :: NativeGeomEventHandler

foreign import createObjectImpl :: Fn4 GeometryCache Int Int String GeometryCache

foreign import createMorphismImpl :: Fn3 GeometryCache Int Int GeometryCache

foreign import startMorphismImpl :: Fn2 GeometryCache Int GeometryCache

foreign import startDraggingImpl :: Fn2 GeometryCache Int GeometryCache

foreign import stopDraggingImpl :: Fn1 GeometryCache GeometryCache

createForeignObject :: GeometryCache -> Int -> Int -> String -> GeometryCache
createForeignObject = runFn4 createObjectImpl

createForeignMorphism :: GeometryCache -> Int -> Int -> GeometryCache
createForeignMorphism = runFn3 createMorphismImpl

startMorphism :: GeometryCache -> Int -> GeometryCache
startMorphism = runFn2 startMorphismImpl

startDragging :: GeometryCache -> Int -> GeometryCache
startDragging = runFn2 startDraggingImpl

stopDragging :: GeometryCache -> GeometryCache
stopDragging = runFn1 stopDraggingImpl

handleMouseDown :: GeomEventHandler
handleMouseDown = runFn4 handleMouseDownImpl def

handleMouseMove :: GeomEventHandler
handleMouseMove = runFn4 handleMouseMoveImpl def

handleMouseUp :: GeomEventHandler
handleMouseUp = runFn4 handleMouseUpImpl def

render :: Effect Unit
render = runWidgetInDom "app" $ component emptyCategory { context: Nothing, geometryCache: unsafePerformEffect emptyGeometryCache }

foreign import resizeCanvas :: El -> Effect Unit

foreign import getContext :: forall a. Widget HTML a -> Effect Context2d

-- | Run a computation (inside a halogen component) which requires access to a canvas rendering context.
withContext :: forall a. Ref NativeNode -> (Context2d -> Effect a) -> Effect (Maybe a)
withContext ref comp = do
  matchingRef <- liftEffect $ Ref.getCurrentRef ref
  case matchingRef of
    Nothing -> pure Nothing
    Just element -> do
      context <- getContext (unsafeCoerce element)
      sequence $ Just $ comp context

type GeometryState =
  { context :: Maybe Context2d
  , geometryCache :: GeometryCache
  }

data Query a
  = LoadScene GeometryState (Ref NativeNode) a
  | Rerender (Ref NativeNode) a

data Action = 
  Render (Ref NativeNode) 
  | HandleEvent GeomEventHandler SyntheticMouseEvent (Ref NativeNode)

type Input = Unit

type Output = Unit

component :: Category -> GeometryState -> Widget HTML _
component category st = do
  canvasRef <- liftEffect Ref.createNodeRef
  event <- D.div
    [ (\event -> HandleEvent handleMouseDown event canvasRef) <$> onMouseDown
    , (\event -> HandleEvent handleMouseMove event canvasRef) <$> onMouseMove
    , (\event -> HandleEvent handleMouseUp event canvasRef) <$> onMouseUp
    ]
    [ D.canvas
      [ P.width $ "600px"
      , P.height $ "600px"
      , P._id $ "leCanvas"
      , P.ref (Ref.fromRef canvasRef)
      ] []
    , text $ show category.objects
    ]

  newState <- liftEffect $ handleAction st event

  component (fromMaybe category $ fst <$> newState) (fromMaybe st $ snd <$> newState)

  where

  handleQuery :: forall a. Query a -> Effect (Maybe (Tuple Category GeometryState))
  handleQuery query = case query of
    LoadScene newState ref a ->
      let updatedState = st { context = newState.context, geometryCache = newState.geometryCache }
      in handleAction updatedState (Render ref)
    Rerender ref a -> handleAction st (Render ref)

  handleAction :: GeometryState -> Action -> Effect (Maybe (Tuple Category GeometryState))
  handleAction state action = case action of
    Render ref -> do
      _ <- withContext ref (\ctx -> renderCanvas ctx state.geometryCache)
      pure Nothing

    HandleEvent handler event ref -> do
      foreignAction <- withContext ref $ \ctx -> do
        handler ctx event state.geometryCache
      case foreignAction of
        Just (CreateObject a b c) -> log $ "New object: " <> show a <> " " <> show b <> " " <> c
        _ -> pure $ unit
      applyFirst (pure (handleForeignAction category state <$> foreignAction)) (handleQuery $ Rerender ref unit)