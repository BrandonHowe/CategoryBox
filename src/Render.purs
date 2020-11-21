module Render where

import Prelude
import Prim

import Category.Main (Category, Object(..), Morphism(..), composeMorphisms, createMorphism, emptyCategory)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (El)
import Concur.React.DOM as D
import Concur.React.Props (onMouseDown, onMouseMove, onMouseUp)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Array (elemIndex, singleton, snoc, (!!))
import Data.Default (class Default, def)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn4, runFn2, runFn1, mkFn3, mkFn2, mkFn1)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import React.Ref (NativeNode, Ref)
import React.Ref as Ref
import React.SyntheticEvent (SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

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

data UpdateAction
  = UpdateCreateObject Int Int String
  | UpdateCreateMorphism Int Int String
  | UpdateComposeMorphisms Int Int String
  | NoUpdate

updateStateCache :: Category -> GeometryState -> UpdateAction -> Maybe (Tuple Category GeometryState)
updateStateCache category geom action = case action of
  UpdateCreateObject posX posY name -> Just $ Tuple (category { objects = snoc category.objects (Object name) }) $ geom { geometryCache = createForeignObject geom.geometryCache posX posY name }
  UpdateCreateMorphism idx1 idx2 name -> Just $ Tuple (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton newMorphism) }) $ geom { geometryCache = createForeignMorphism geom.geometryCache idx1 idx2 name }
    where
      obj1 = category.objects !! idx1
      obj2 = category.objects !! idx2
      newMorphism = createMorphism <$> obj1 <*> obj2
  UpdateComposeMorphisms idx1 idx2 name -> Just $ Tuple (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton composedMorphism) }) ( geom { geometryCache = fromMaybe geom.geometryCache $ createForeignMorphism geom.geometryCache <$> composedIdx1 <*> composedIdx2 <*> Just name })
    where
      mor1 = category.morphisms !! idx1
      mor2 = category.morphisms !! idx2
      composedMorphism = join $ composeMorphisms <$> mor1 <*> mor2
      composedIdx1 = join $ (\(Morphism (Tuple obj1 _)) -> elemIndex obj1 category.objects) <$> composedMorphism
      composedIdx2 = join $ (\(Morphism (Tuple _ obj2)) -> elemIndex obj2 category.objects) <$> composedMorphism
  NoUpdate -> Just $ Tuple category geom

handleForeignAction :: Category -> GeometryState -> ForeignAction -> HandleActionOutput
handleForeignAction category geom action = case action of
  CreateObject posX posY name -> NewState $ updateStateCache category geom (UpdateCreateObject posX posY name)
  CreateMorphism idx1 idx2 name -> NewState $ updateStateCache category geom (UpdateCreateMorphism idx1 idx2 name)
  ComposeMorphisms idx1 idx2 name -> NewState $ updateStateCache category geom (UpdateCreateMorphism idx1 idx2 name)
  StartMorphism idx -> NewState $ Just $ Tuple category ( geom { geometryCache = startMorphism geom.geometryCache idx })
  StartDragging idx -> NewState $ Just $ Tuple category ( geom { geometryCache = startDragging geom.geometryCache idx })
  StopDragging -> NewState $ Just $ Tuple category ( geom { geometryCache = stopDragging geom.geometryCache })
  GetObjectName posX posY -> RaiseComponent $ modalInputComponent "What is the name of this object?" "Object name" >>= (\name -> pure $ updateStateCache category geom $ UpdateCreateObject posX posY name)
  GetMorphismName idx1 idx2 -> RaiseComponent $ modalInputComponent "What is the name of this morphism?" "Morphism name" >>= (\name -> pure $ updateStateCache category geom $ UpdateCreateMorphism idx1 idx2 name)
  GetCompositionName idx1 idx2 -> RaiseComponent $ modalInputComponent "What is the name of this composed morphism?" "Morphism name" >>= (\name -> pure $ updateStateCache category geom $ UpdateComposeMorphisms idx1 idx2 name)
  NoAction -> NewState $ Just $ Tuple category geom

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
foreign import createMorphismImpl :: Fn4 GeometryCache Int Int String GeometryCache
foreign import startMorphismImpl :: Fn2 GeometryCache Int GeometryCache
foreign import startDraggingImpl :: Fn2 GeometryCache Int GeometryCache
foreign import startComposingImpl :: Fn2 GeometryCache Int GeometryCache
foreign import stopDraggingImpl :: Fn1 GeometryCache GeometryCache

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

handleMouseDown :: GeomEventHandler
handleMouseDown = runFn4 handleMouseDownImpl def

handleMouseMove :: GeomEventHandler
handleMouseMove = runFn4 handleMouseMoveImpl def

handleMouseUp :: GeomEventHandler
handleMouseUp = runFn4 handleMouseUpImpl def

render :: Effect Unit
render = runWidgetInDom "app" $ canvasComponent emptyCategory { context: Nothing, geometryCache: unsafePerformEffect emptyGeometryCache }

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

data HandleActionOutput
  = NewState (Maybe (Tuple Category GeometryState))
  | RaiseComponent (Widget HTML (Maybe (Tuple Category GeometryState)))

modalInputComponent :: String -> String -> Widget HTML String
modalInputComponent question placeholder = do 
  e <- D.div [ P.className "modalInputComponentBackground" ] 
    [ D.div [ P.className "modalInputComponentBody" ] 
      [ D.h2 [ P.className "modalInputComponentQuestion" ] [ D.text question ]
      , D.input $ [P.onKeyEnter, P.placeholder placeholder, P.className "modalInputComponentInput" ] 
      ] 
    ]
  new <- pure $ P.unsafeTargetValue e
  liftEffect (P.resetTargetValue "" e)
  pure new

canvasComponent :: forall a. Category -> GeometryState -> Widget HTML a
canvasComponent category st = do
  canvasRef <- liftEffect Ref.createNodeRef
  event <- D.div 
    [ (\event -> HandleEvent handleMouseDown event canvasRef) <$> onMouseDown
    , (\event -> HandleEvent handleMouseMove event canvasRef) <$> onMouseMove
    , (\event -> HandleEvent handleMouseUp event canvasRef) <$> onMouseUp
    , P._id $ "canvasDiv"
    ]
    [ D.canvas
      [ P.width $ unsafePerformEffect $ (window >>= innerWidth) <#> (toNumber >>> toString)
      , P.height $ unsafePerformEffect $ (window >>= innerHeight) <#> ((flip sub 6) >>> toNumber >>> toString)
      , P._id $ "leCanvas"
      , P.ref (Ref.fromRef canvasRef)
      ] []
    ]

  newState <- liftEffect $ handleAction st event

  fromMaybe (canvasComponent category st) $ (\passedState -> case passedState of
    (NewState state) -> canvasComponent (fromMaybe category $ fst <$> state) (fromMaybe st $ snd <$> state)
    (RaiseComponent component) -> canvasComponent category st <|> component >>= \state -> canvasComponent (fromMaybe category $ fst <$> state) (fromMaybe st $ snd <$> state)
  ) <$> newState

  where

  handleQuery :: forall b. Query b -> Effect (Maybe HandleActionOutput)
  handleQuery query = case query of
    LoadScene newState ref a ->
      let updatedState = st { context = newState.context, geometryCache = newState.geometryCache }
      in handleAction updatedState (Render ref)
    Rerender ref a -> handleAction st (Render ref)

  handleAction :: GeometryState -> Action -> Effect (Maybe HandleActionOutput)
  handleAction state action = case action of
    Render ref -> do
      _ <- withContext ref \ctx -> renderCanvas ctx state.geometryCache
      pure $ Just $ NewState Nothing

    HandleEvent handler event ref -> (\val -> handleForeignAction category state <$> val) <$> (withContext ref (\ctx -> handler ctx event state.geometryCache)) <* (handleAction state (Render ref))