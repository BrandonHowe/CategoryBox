module CategoryBox.Render where

import Prelude

import CategoryBox.Data.Main (composeMorphisms, createMorphism, emptyCategory, getFunctorCategory)
import CategoryBox.Data.Types (Category, Morphism(..), Object(..), World)
import CategoryBox.Foreign.ForeignAction (ForeignAction(..))
import CategoryBox.Foreign.Render (Context2d, GeomMouseEventHandler, GeomWheelEventHandler, GeometryCache, createForeignMorphism, createForeignObject, emptyGeometryCache, getContext, handleMouseDown, handleMouseMove, handleMouseUp, handleScroll, renderCanvas, startDragging, startMorphism, stopDragging)
import Concur.Core (Widget)
import Concur.Core.Props (filterProp)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (ReactProps, onMouseDown, onMouseMove, onMouseUp, onWheel)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Alt ((<|>))
import Data.Array (elemIndex, length, mapWithIndex, singleton, snoc, updateAt, (!!), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import React.Ref (NativeNode, Ref)
import React.Ref as Ref
import React.SyntheticEvent (SyntheticKeyboardEvent, SyntheticMouseEvent, SyntheticWheelEvent)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (innerHeight, innerWidth)

-- | Ways we can update the GeometryState.
data UpdateAction
  = UpdateCreateObject Int Int String
  | UpdateCreateMorphism Int Int String
  | UpdateComposeMorphisms Int Int String
  | NoUpdate

-- | State that contains all the geometry caches.
type GeometryState =
  { context :: Maybe Context2d
  , geometryCaches :: Array GeometryCache
  , mainGeometryCache :: GeometryCache
  , currentCategory :: Int
  }

-- | Actions that the user can execute via the DOM.
data Action = 
  Render
  | HandleMouseEvent GeomMouseEventHandler SyntheticMouseEvent
  | HandleWheelEvent GeomWheelEventHandler SyntheticWheelEvent
  | AddCategoryPress
  | SwitchCategoryTo Int

-- | Tuple of World and GeometryState, wrapped in something and used after handling a typescript event.
data HandleActionOutput
  = NewState (Maybe (Tuple World GeometryState))
  | RaiseComponent (Widget HTML (Maybe (Tuple World GeometryState)))
  | ReplaceCategory (Tuple World GeometryState)

-- | Update the World and the GeometryState.
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
    category :: Category
    category = getCurrentCategory world geom

    geometryCache :: GeometryCache
    geometryCache = getCurrentCache geom

    updateGeometryState :: GeometryCache -> GeometryState -> GeometryState
    updateGeometryState cache state = state { geometryCaches = fromMaybe state.geometryCaches (updateAt state.currentCategory cache state.geometryCaches) }

    updateWorldCategory :: Category -> World -> World
    updateWorldCategory cat w = world { categories = fromMaybe world.categories $ updateAt geom.currentCategory cat w.categories }

-- | Handles events passed to us from the typescript side.
handleForeignAction :: World -> GeometryState -> ForeignAction -> HandleActionOutput
handleForeignAction world geom action = case action of
  -- | Use updateStateCache to update the state.
  CreateObject posX posY name -> NewState $ updateStateCache world geom (UpdateCreateObject posX posY name)
  CreateMorphism idx1 idx2 name -> NewState $ updateStateCache world geom (UpdateCreateMorphism idx1 idx2 name)
  ComposeMorphisms idx1 idx2 name -> NewState $ updateStateCache world geom (UpdateCreateMorphism idx1 idx2 name)
  -- | Start morphsims or dragging
  StartMorphism idx -> NewState $ Just $ Tuple world $ updateGeometryState (startMorphism geometryCache idx) geom
  StartDragging idx -> NewState $ Just $ Tuple world $ updateGeometryState (startDragging geometryCache idx) geom
  StopDragging -> NewState $ Just $ Tuple world $ updateGeometryState (stopDragging geometryCache) geom
  -- | Pass a component back to the user for further input.
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
    category = getCurrentCategory world geom
    geometryCache = getCurrentCache geom

    updateGeometryState :: GeometryCache -> GeometryState -> GeometryState
    updateGeometryState cache state = state { geometryCaches = fromMaybe state.geometryCaches (updateAt state.currentCategory cache state.geometryCaches) }

render :: Effect Unit
render = runWidgetInDom "app" $ canvasComponent defaultWorld defaultState
  where
    functorCategory :: GeometryCache
    functorCategory = createForeignObject (unsafePerformEffect emptyGeometryCache) 0 0 "Category 1"
    defaultWorld :: World
    defaultWorld = { categories: [emptyCategory], functors: [] }
    defaultState :: GeometryState
    defaultState = { context: Nothing, geometryCaches: [unsafePerformEffect emptyGeometryCache], mainGeometryCache: functorCategory, currentCategory: 0 }

-- | Run a computation which requires access to a canvas rendering context.
withContext :: forall a. Ref NativeNode -> (Context2d -> Effect a) -> Effect (Maybe a)
withContext ref comp = do
  matchingRef <- liftEffect $ Ref.getCurrentRef ref
  case matchingRef of
    Nothing -> pure Nothing
    Just element -> do
      context <- getContext (unsafeCoerce element)
      sequence $ Just $ comp context

-- | Get current category based off the world and current state.
getCurrentCategory :: World -> GeometryState -> Category
getCurrentCategory world st = if st.currentCategory == 0 then functorCategory else fromMaybe functorCategory ((!!) world.categories (st.currentCategory - 1))
  where
    functorCategory :: Category
    functorCategory = getFunctorCategory world

-- | Get current geometry cache based off the current state.
getCurrentCache :: GeometryState -> GeometryCache
getCurrentCache state = if state.currentCategory == 0 then state.mainGeometryCache else fromMaybe state.mainGeometryCache ((!!) state.geometryCaches (state.currentCategory - 1))

-- | Component that gathers an input from a modal.
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

  -- | Create HTML of the component, as well as gather any events.
  event <- D.div 
    [ P._id $ "canvasDiv" ]
    $ [ D.button [AddCategoryPress <$ P.onClick, P.className "categoryButton"] [D.text "Add category"]
      , D.div
          [P._id "categoryButtons"]
          (D.button [SwitchCategoryTo 0 <$ P.onClick, P.className "categoryButton"] [D.text "Functor category"] :
          mapWithIndex (\idx _ -> D.button [SwitchCategoryTo (idx + 1) <$ P.onClick, P.className "categoryButton"] [D.text $ "Category " <> (show $ idx + 1)]) world.categories)
      , D.canvas
        [ P.width $ unsafePerformEffect $ (window >>= innerWidth) <#> (flip sub 100 >>> show)
        , P.height $ unsafePerformEffect $ (window >>= innerHeight) <#> (flip sub 6 >>> show)
        , P._id $ "leCanvas"
        , P.ref (Ref.fromRef canvasRef)
        , onMouseDown <#> \event -> HandleMouseEvent handleMouseDown event
        , onMouseMove <#> \event -> HandleMouseEvent handleMouseMove event
        , onMouseUp <#> \event -> HandleMouseEvent handleMouseUp event
        , onWheel <#> \event -> HandleWheelEvent handleScroll event
        ] []
      ]

  -- | Get the new state after handling any DOM actions.
  newState <- liftEffect $ handleAction st event canvasRef

  -- | Render the next version of the component.
  fromMaybe (canvasComponent world st) $ newState <#> \passedState -> case passedState of
    (NewState state) -> canvasComponent (fromMaybe world $ fst <$> state) (fromMaybe st $ snd <$> state)
    (RaiseComponent component) -> canvasComponent world st <|> component >>= \state -> canvasComponent (fromMaybe world $ fst <$> state) (fromMaybe st $ snd <$> state)
    (ReplaceCategory new) -> canvasComponent (fst new) (snd new)

  where

  category :: Category
  category = getCurrentCategory world st

  -- | Handle action passed to us via the DOM.
  handleAction :: GeometryState -> Action -> Ref NativeNode -> Effect (Maybe HandleActionOutput)
  handleAction state action ref = case action of
    Render -> do
      _ <- withContext ref \ctx -> renderCanvas ctx geometryCache
      pure $ Just $ NewState Nothing
    HandleMouseEvent handler event ->
      (\val -> handleForeignAction world state <$> val) <$> (withContext ref (\ctx -> handler ctx event geometryCache))
      <* (handleAction state Render ref)
    HandleWheelEvent handler event ->
      (\val -> handleForeignAction world state <$> val) <$> (withContext ref (\ctx -> handler ctx event geometryCache))
      <* (handleAction state Render ref)
    AddCategoryPress -> 
      (pure $ Just $ ReplaceCategory $ Tuple (world { categories = snoc world.categories emptyCategory }) newState)
      <* (handleAction newState Render ref)
      where
        newState = st { geometryCaches = snoc st.geometryCaches $ unsafePerformEffect emptyGeometryCache, currentCategory = length world.categories }
    SwitchCategoryTo newIdx -> (pure $ Just $ ReplaceCategory $ Tuple world $ getNewState newIdx) <* (handleAction (getNewState newIdx) Render ref)
      where
        getNewState :: Int -> GeometryState
        getNewState newInt = (\x -> st { currentCategory = if x > length world.categories - 1 then st.currentCategory else if x < 0 then 0 else x + 1 }) (newInt - 1)
    where
      geometryCache = getCurrentCache state