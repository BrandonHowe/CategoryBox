module Render where

import Prelude
import Prim

import Category.Main (composeMorphisms, createMorphism, emptyCategory)
import Category.Types (Category, Morphism(..), Object(..), World)
import Concur.Core (Widget)
import Concur.Core.Props (filterProp)
import Concur.React (HTML)
import Concur.React.DOM (El)
import Concur.React.DOM as D
import Concur.React.Props (ReactProps, onMouseDown, onMouseMove, onMouseUp, unsafeTargetValue)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Array (elemIndex, length, singleton, snoc, updateAt, (!!))
import Data.Default (class Default, def)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn4, runFn2, runFn1, mkFn3, mkFn2, mkFn1)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number.Format (toString)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML.Events (onKeyDown)
import React.Ref (NativeNode, Ref)
import React.Ref as Ref
import React.SyntheticEvent (SyntheticEvent_, SyntheticKeyboardEvent, SyntheticMouseEvent)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.HTML.Window (document, innerHeight, innerWidth)

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

updateStateCache :: World -> GeometryState -> UpdateAction -> Maybe (Tuple World GeometryState)
updateStateCache world geom action = case action of
  UpdateCreateObject posX posY name -> Just
    $ Tuple (updateWorldCategory (category { objects = snoc category.objects (Object name) }) world )
    $ updateGeometryState (createForeignObject geometryCache posX posY name) geom
  UpdateCreateMorphism idx1 idx2 name -> Just 
    $ Tuple (updateWorldCategory (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton newMorphism) } ) world) 
    $ updateGeometryState (createForeignMorphism geometryCache idx1 idx2 name) geom
    where
      obj1 = category.objects !! idx1
      obj2 = category.objects !! idx2
      newMorphism = createMorphism <$> obj1 <*> obj2 <*> Just name
  UpdateComposeMorphisms idx1 idx2 name -> Just 
    $ Tuple (updateWorldCategory (category { morphisms = category.morphisms <> (fromMaybe [] $ sequence $ singleton composedMorphism) }) world) 
    $ updateGeometryState (fromMaybe geometryCache $ createForeignMorphism geometryCache <$> composedIdx1 <*> composedIdx2 <*> Just name) geom
    where
      mor1 = category.morphisms !! idx1
      mor2 = category.morphisms !! idx2
      composedMorphism = join $ composeMorphisms <$> mor1 <*> mor2
      composedIdx1 = join $ (\(Morphism f) -> elemIndex f.from category.objects) <$> composedMorphism
      composedIdx2 = join $ (\(Morphism f) -> elemIndex f.to category.objects) <$> composedMorphism
  NoUpdate -> Just $ Tuple world geom
  where
    category = fromMaybe emptyCategory $ ((!!) world.categories =<< geom.currentCategory)

    geometryCache = fromMaybe (unsafePerformEffect emptyGeometryCache) $ ((!!) geom.geometryCaches =<< geom.currentCategory)

    updateGeometryState :: GeometryCache -> GeometryState -> GeometryState
    updateGeometryState cache state = state { geometryCaches = fromMaybe state.geometryCaches (join $ updateAt <$> state.currentCategory <*> Just cache <*> Just state.geometryCaches) }

    updateWorldCategory :: Category -> World -> World
    updateWorldCategory category world = world { categories = fromMaybe world.categories $ join $ updateAt <$> geom.currentCategory <*> Just category <*> Just world.categories }

handleForeignAction :: World -> GeometryState -> ForeignAction -> HandleActionOutput
handleForeignAction world geom action = case action of
  CreateObject posX posY name -> NewState $ updateStateCache world geom (UpdateCreateObject posX posY name)
  CreateMorphism idx1 idx2 name -> NewState $ updateStateCache world geom (UpdateCreateMorphism idx1 idx2 name)
  ComposeMorphisms idx1 idx2 name -> NewState $ updateStateCache world geom (UpdateCreateMorphism idx1 idx2 name)
  StartMorphism idx -> NewState $ Just $ Tuple world $ updateGeometryState (startMorphism geometryCache idx) geom
  StartDragging idx -> NewState $ Just $ Tuple world $ updateGeometryState (startDragging geometryCache idx) geom
  StopDragging -> NewState $ Just $ Tuple world $ updateGeometryState (stopDragging geometryCache) geom
  GetObjectName posX posY -> RaiseComponent $ modalInputComponent "What is the name of this object?" "Object name" <#> handleReceivedName world geom
    where
      handleReceivedName :: World -> GeometryState -> Maybe String -> Maybe (Tuple World GeometryState)
      handleReceivedName c g rawName = case rawName of
        (Just name) -> updateStateCache c g $ UpdateCreateObject posX posY name
        (Nothing) -> Just $ Tuple c g
  GetMorphismName idx1 idx2 -> RaiseComponent $ modalInputComponent "What is the name of this morphism?" "Morphism name" <#> handleReceivedName world geom
    where
      handleReceivedName :: World -> GeometryState -> Maybe String -> Maybe (Tuple World GeometryState)
      handleReceivedName c g rawName = case rawName of
        (Just name) -> updateStateCache c g $ UpdateCreateMorphism idx1 idx2 name
        (Nothing) -> Just $ Tuple c g
  GetCompositionName idx1 idx2 -> RaiseComponent $ modalInputComponent "What is the name of this composed morphism?" "Morphism name" <#> handleReceivedName world geom
    where
      handleReceivedName :: World -> GeometryState -> Maybe String -> Maybe (Tuple World GeometryState)
      handleReceivedName c g rawName = case rawName of
        (Just name) -> updateStateCache c g $ UpdateComposeMorphisms idx1 idx2 name
        (Nothing) -> Just $ Tuple c g
  NoAction -> NewState $ Just $ Tuple world geom
  where
    category = fromMaybe emptyCategory $ (join $ (!!) world.categories <$> geom.currentCategory)
    geometryCache = fromMaybe (unsafePerformEffect emptyGeometryCache) $ ((!!) geom.geometryCaches =<< geom.currentCategory)

    updateGeometryState :: GeometryCache -> GeometryState -> GeometryState
    updateGeometryState cache state = state { geometryCaches = fromMaybe state.geometryCaches (join $ updateAt <$> state.currentCategory <*> Just cache <*> Just state.geometryCaches) }

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
render = runWidgetInDom "app" $ canvasComponent { categories: [emptyCategory], functors: [] } { context: Nothing, geometryCaches: [unsafePerformEffect emptyGeometryCache], currentCategory: Just 0 }

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
  , geometryCaches :: Array GeometryCache
  , currentCategory :: Maybe Int
  }

data Query a
  = LoadScene GeometryState (Ref NativeNode) a
  | Rerender (Ref NativeNode)

data Action = 
  Render
  | HandleEvent GeomEventHandler SyntheticMouseEvent
  | AddCategoryPress
  | IncreaseCategory
  | DecreaseCategory

type Input = Unit

type Output = Unit

data HandleActionOutput
  = NewState (Maybe (Tuple World GeometryState))
  | RaiseComponent (Widget HTML (Maybe (Tuple World GeometryState)))
  | ReplaceCategory (Tuple World GeometryState)

focusInputById :: String -> Effect Unit
focusInputById id = window >>= document >>= toDocument >>> toNonElementParentNode >>> getElementById id >>= case _ of
  Nothing -> pure unit
  Just elem -> pure unit <* log "blah" <* (sequence $ focus <$> fromElement elem)

modalInputComponent :: String -> String -> Widget HTML (Maybe String)
modalInputComponent question placeholder = do 
  e <- D.div [ P.className "modalInputComponentBackground", Nothing <$ onKeyEscape ]
    [ D.div [ P.className "modalInputComponentBody" ] 
      [ D.h2 [ P.className "modalInputComponentQuestion" ] [ D.text question ]
      , D.input  [ Just <$> P.onKeyEnter, P.placeholder placeholder, P.className "modalInputComponentInput", P._id "modalInputComponentInput" ]
      , D.button [ Nothing <$ P.onClick, P.className "modalInputComponentCancel" ] [ D.text "Close" ]
      ]
    ]
  new <- pure $ P.unsafeTargetValue <$> e
  _ <- liftEffect $ sequence $ P.resetTargetValue "" <$> e
  liftEffect $ focusInputById "modalInputComponentInput"
  pure new
    where
      onKeyEscape :: ReactProps SyntheticKeyboardEvent
      onKeyEscape = filterProp isEscapeEvent P.onKeyDown
      isEscapeEvent :: SyntheticKeyboardEvent -> Boolean
      isEscapeEvent e = e'.which == 27 || e'.keyCode == 27
        where
          e' = unsafeCoerce e

canvasComponent :: forall a. World -> GeometryState -> Widget HTML a
canvasComponent world st = do
  canvasRef <- liftEffect Ref.createNodeRef
  event <- D.div 
    [ P._id $ "canvasDiv" ]
    [ D.canvas
      [ P.width $ unsafePerformEffect $ (window >>= innerWidth) <#> (toNumber >>> toString)
      , P.height $ unsafePerformEffect $ (window >>= innerHeight) <#> ((flip sub 6) >>> toNumber >>> toString)
      , P._id $ "leCanvas"
      , P.ref (Ref.fromRef canvasRef)
      , (\event -> HandleEvent handleMouseDown event) <$> onMouseDown
      , (\event -> HandleEvent handleMouseMove event) <$> onMouseMove
      , (\event -> HandleEvent handleMouseUp event) <$> onMouseUp
      ] []
    , D.button [DecreaseCategory <$ P.onClick] [D.text "-"]
    , D.button [AddCategoryPress <$ P.onClick] [D.text "Add category"]
    , D.button [IncreaseCategory <$ P.onClick] [D.text "+"]
    ]

  newState <- liftEffect $ handleAction st event canvasRef

  _ <- liftEffect $ logShow world

  fromMaybe (canvasComponent world st) $ (\passedState -> case passedState of
    (NewState state) -> canvasComponent (fromMaybe world $ fst <$> state) (fromMaybe st $ snd <$> state)
    (RaiseComponent component) -> canvasComponent world st <|> component >>= \state -> canvasComponent (fromMaybe world $ fst <$> state) (fromMaybe st $ snd <$> state)
    (ReplaceCategory new) -> canvasComponent (fst new) (snd new)
  ) <$> newState

  where

  category = fromMaybe emptyCategory $ ((!!) world.categories =<< st.currentCategory)

  handleQuery :: forall b. Query b -> Effect (Maybe HandleActionOutput)
  handleQuery query = case query of
    LoadScene newState ref a -> handleAction updatedState Render ref
      where
        updatedState = st { context = newState.context, geometryCaches = newState.geometryCaches }
    Rerender ref -> handleAction st Render ref

  handleAction :: GeometryState -> Action -> Ref NativeNode -> Effect (Maybe HandleActionOutput)
  handleAction state action ref = case action of
    Render -> do
      _ <- withContext ref \ctx -> renderCanvas ctx geometryCache
      pure $ Just $ NewState Nothing
    HandleEvent handler event -> (\val -> handleForeignAction world state <$> val) <$> (withContext ref (\ctx -> handler ctx event geometryCache)) <* (handleQuery (Rerender ref))
    AddCategoryPress -> 
      (pure $ Just $ ReplaceCategory $ Tuple (world { categories = snoc world.categories emptyCategory }) newState)
      <* (handleAction newState Render ref)
      where
        newState = st { geometryCaches = snoc st.geometryCaches $ unsafePerformEffect emptyGeometryCache, currentCategory = Just $ length world.categories }
    IncreaseCategory -> 
      (pure $ Just $ ReplaceCategory $ Tuple world newState)
      <* (handleAction newState Render ref)
      where
        comparisonValue = ((==) <$> st.currentCategory) <*> (Just $ length world.categories - 1)
        newState = st { currentCategory = if (fromMaybe false comparisonValue) then st.currentCategory else flip (+) 1 <$> st.currentCategory }
    DecreaseCategory -> 
      (pure $ Just $ ReplaceCategory $ Tuple world newState)
      <* (handleAction newState Render ref)
      where
        newState = st { currentCategory = if st.currentCategory == Just 0 then st.currentCategory else flip (-) 1 <$> st.currentCategory }
    where
      geometryCache = fromMaybe (unsafePerformEffect emptyGeometryCache) $ ((!!) state.geometryCaches =<< state.currentCategory)