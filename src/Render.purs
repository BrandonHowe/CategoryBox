module Render where

import Prelude
import Prim

import Category.Main (Category, Object(..), createMorphism)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El)
import Concur.React.DOM as D
import Concur.React.Props (onMouseDown, onMouseMove)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Data.Array (singleton, snoc, (!!))
import Data.Default (class Default, def)
import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn4, mkFn3, mkFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import React.Ref (NativeNode, Ref)
import React.Ref as Ref
import React.SyntheticEvent (SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)

-- | Stuff the ts side of things can tell us to do
data ForeignAction
  = CreateObject Int Int String
  | CreateMorphism Int Int
  | NoAction

newtype ForeignActionConfig
  = ForeignActionConfig
  { createObject :: Fn3 Int Int String ForeignAction
  , createMorphism :: Fn2 Int Int ForeignAction
  , nothing :: ForeignAction
  }


instance defaultForeignActionConfig :: Default ForeignActionConfig where
  def =
    ForeignActionConfig
      { createObject: mkFn3 CreateObject
      , createMorphism: mkFn2 CreateMorphism
      , nothing: NoAction
      }

handleForeignAction :: Category -> GeometryState -> ForeignAction -> Tuple Category GeometryState
handleForeignAction category geom action = case action of
  CreateObject posX posY name -> Tuple (category { objects = snoc category.objects (Object name) }) geom
  CreateMorphism idx1 idx2 -> 
    let obj1 = category.objects !! idx1
        obj2 = category.objects !! idx2
        newMorphism = createMorphism <$> obj1 <*> obj2
    in Tuple (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton newMorphism) }) geom
  NoAction -> Tuple category geom

foreign import data Context2d :: Type

foreign import data GeometryCache :: Type

foreign import emptyGeometryCache :: Effect GeometryCache

foreign import renderCanvas :: Context2d -> GeometryCache -> Effect Unit

-- | Type of event handlers for the Scene component.
type NativeGeomEventHandler
  = Fn4 ForeignActionConfig Context2d SyntheticMouseEvent GeometryCache (Effect Unit)

type GeomEventHandler
  = Context2d -> SyntheticMouseEvent -> GeometryCache -> Effect Unit

foreign import handleMouseUpImpl :: NativeGeomEventHandler

foreign import handleMouseDownImpl :: NativeGeomEventHandler

foreign import handleMouseMoveImpl :: NativeGeomEventHandler

handleMouseDown :: GeomEventHandler
handleMouseDown = runFn4 handleMouseDownImpl def

handleMouseMove :: GeomEventHandler
handleMouseMove = runFn4 handleMouseMoveImpl def

handleMouseUp :: GeomEventHandler
handleMouseUp = runFn4 handleMouseUpImpl def

render :: Effect Unit
render = runWidgetInDom "app" $ component $ { context: Nothing, geometryCache: unsafePerformEffect emptyGeometryCache }

foreign import resizeCanvas :: El -> Effect Unit

foreign import getContext :: forall a. Widget HTML a -> Effect Context2d

-- | Run a computation (inside a halogen component) which requires access to a canvas rendering context.
withContext :: Ref NativeNode -> (Context2d -> Effect Unit) -> Effect Unit
withContext ref comp = do
  matchingRef <- liftEffect $ Ref.getCurrentRef ref
  case matchingRef of
    Nothing -> pure unit
    Just element -> do
      context <- getContext (unsafeCoerce element)
      comp context

type GeometryState =
  { context :: Maybe Context2d
  , geometryCache :: GeometryCache
  }

data Query a
  = LoadScene GeometryState (Ref NativeNode) a
  | Rerender (Ref NativeNode) a

data Action = Render (Ref NativeNode) | HandleEvent GeomEventHandler SyntheticMouseEvent (Ref NativeNode)

type Input = Unit

type Output = Unit

component :: GeometryState -> Widget HTML Unit
component st = do
  (canvasRef :: Ref.Ref _) <- liftEffect Ref.createNodeRef
  event <- D.div
    [ (\event -> HandleEvent handleMouseDown event canvasRef) <$> onMouseDown
    , (\event -> HandleEvent handleMouseMove event canvasRef) <$> onMouseMove
    ]
    [ D.canvas
      [ P.width $ "600px"
      , P.height $ "600px"
      , P._id $ "leCanvas"
      , P.ref (Ref.fromRef canvasRef)
      ] []
    ]

  liftEffect $ handleAction st event

  where

  handleQuery :: forall a. Query a -> GeometryState
  handleQuery query = case query of
    LoadScene newState ref a ->
      let updatedState = st { context = newState.context, geometryCache = newState.geometryCache }
      in const updatedState (handleAction updatedState (Render ref))
    Rerender ref a -> const st $ handleAction st $ Render ref

  handleAction :: GeometryState -> Action -> Effect Unit
  handleAction state action = case action of
    Render ref ->
      withContext ref (\ctx -> renderCanvas ctx state.geometryCache)

    HandleEvent handler event ref ->
      withContext ref $ \ctx -> do
        logShow "handling event..."
        _ <- handler ctx event state.geometryCache
        pure $ const unit (handleQuery $ Rerender ref unit)