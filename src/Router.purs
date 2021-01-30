module CategoryBox.Router where

import CategoryBox.Render (localStorageCanvasComponent)
import CategoryBox.Tutorials.Component (tutorialComponent)
import CategoryBox.Tutorials.TutorialId (TutorialId(..))
import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Prelude (class Eq, class Ord, class Show, map, show, ($), (<<<), (*>))
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | GoTutorial TutorialId

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

routingCodec :: RouteDuplex' Route
routingCodec =
  root
    $ sum
      { "Home": noArgs
      , "GoTutorial": "tutorial" / tutorialId segment
      }

tutorialId :: RouteDuplex' String -> RouteDuplex' TutorialId
tutorialId = as show (map TutorialId <<< Right)

-- | Takes a route and returns the correct widget for that route.
renderRouter :: forall a. Route -> Widget HTML a
renderRouter route = case route of
  Home -> localStorageCanvasComponent
  GoTutorial id -> localStorageCanvasComponent <|> (tutorialComponent id *> localStorageCanvasComponent)