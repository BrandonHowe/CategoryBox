module Render where

import Prelude

import Control.Monad.State (modify_)
import Data.Default (def)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (Component, getHTMLElementRef, gets, liftEffect, raise)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events (onMouseDown, onMouseMove, onMouseOver, onMouseUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML (HTMLCanvasElement)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.UIEvent.MouseEvent (MouseEvent)

-- | Stuff the ts side of things can tell us to do
data ForeignAction
  = CreateObject Int Int
  | CreateMorphism Int Int
  | NoAction

foreign import data Context2d :: Type

foreign import data GeometryCache :: Type

foreign import emptyGeometryCache :: Effect GeometryCache

foreign import renderCanvas :: Context2d -> GeometryCache -> Effect Unit

-- | Type of event handlers for the Scene component.
type NativeGeomEventHandler
  = Fn3 Context2d MouseEvent GeometryCache (Effect Unit)

type GeomEventHandler
  = Context2d -> MouseEvent -> GeometryCache -> Effect Unit

foreign import handleMouseUpImpl :: NativeGeomEventHandler

foreign import handleMouseDownImpl :: NativeGeomEventHandler

foreign import handleMouseMoveImpl :: NativeGeomEventHandler

handleMouseDown :: GeomEventHandler
handleMouseDown = runFn3 handleMouseDownImpl

handleMouseMove :: GeomEventHandler
handleMouseMove = runFn3 handleMouseMoveImpl

handleMouseUp :: GeomEventHandler
handleMouseUp = runFn3 handleMouseUpImpl

render :: Effect Unit
render = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

canvasRef :: H.RefLabel
canvasRef = H.RefLabel "canvas"

-- Halogen M which keeps track of a canvas
type CanvasHalogenM r a s o m a'
  = H.HalogenM { | ( context :: Maybe Context2d | r ) } a s o m a'

foreign import resizeCanvas :: HTMLCanvasElement -> Effect Unit

foreign import getContext :: HTMLCanvasElement -> Effect Context2d

-- | Run a computation (inside a halogen component) which requires access to a canvas rendering context.
withContext ::
  forall r a s o m.
  MonadEffect m =>
  H.RefLabel -> (Context2d -> CanvasHalogenM r a s o m Unit) -> CanvasHalogenM r a s o m Unit
withContext ref continue = do
  context <- gets _.context
  case context of
    Just ctx -> continue ctx
    Nothing -> do
      element <- (_ >>= HTMLCanvasElement.fromHTMLElement) <$> getHTMLElementRef ref
      case element of
        Nothing -> pure unit
        Just canvas -> do
          liftEffect $ resizeCanvas canvas
          ctx <- liftEffect $ getContext canvas
          modify_ _ { context = Just ctx }
          continue ctx

type State =
  { context :: Maybe Context2d
  , geometryCache :: GeometryCache
  }

data Query a
  = LoadScene GeometryCache a
  | Rerender a

data Action = Render | HandleEvent GeomEventHandler MouseEvent

type Input = Unit

type Output = Unit

component :: forall m. MonadEffect m => MonadAff m => Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: const { context: Nothing, geometryCache: unsafePerformEffect emptyGeometryCache }
    , render: renderState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  renderState state =
    HH.div_
      [ HH.canvas $ [ HP.width $ 600, HP.height $ 600, HP.id_ $ "leCanvas", HP.ref canvasRef, onMouseDown $ Just <<< HandleEvent handleMouseDown, onMouseMove $ Just <<< HandleEvent handleMouseMove, onMouseUp $ Just <<< HandleEvent handleMouseUp ]
      ]

  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    LoadScene cache a -> do
      modify_ _ { geometryCache = cache }
      handleAction Render
      pure $ Just a
    Rerender a -> do
      handleAction Render
      pure $ Just a

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Render ->
      withContext canvasRef \ctx -> do
        cache <- gets _.geometryCache
        liftEffect $ renderCanvas ctx cache

    HandleEvent handler event ->
      withContext canvasRef \ctx -> do
        cache <- gets _.geometryCache
        action <- liftEffect $ handler ctx event cache
        _ <- handleQuery $ Rerender unit
        pure unit