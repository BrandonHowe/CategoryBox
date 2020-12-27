module CategoryBox.Tutorials.GetTutorial where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import CategoryBox.Data.Types (World)
import CategoryBox.Render (GeometryState)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, (.:))
import Data.Bifunctor (lmap)
import Data.Bitraversable (bisequence)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Semigroup (append)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

type Tutorial =
  { name    :: String
  , content :: String
  , world   :: World
  , geom    :: GeometryState
  }

type Gist =
  { files :: 
    { "world.json" :: { content :: String }
    , "geom.json"  :: { content :: String }
    , "content.md" :: { content :: String }
    }
  }

gistFromJson :: Json -> Either JsonDecodeError Gist
gistFromJson = decodeJson

worldFromJson :: Json -> Either JsonDecodeError World
worldFromJson = decodeJson

worldFromString :: String -> Either JsonDecodeError World
worldFromString = parseJson >=> worldFromJson

geomFromJson :: Json -> Either JsonDecodeError GeometryState
geomFromJson json = do
  x <- decodeJson json
  currentCategory <- x .: "currentCategory"
  geometryCaches <- x .: "geometryCaches"
  mainGeometryCache <- x .: "mainGeometryCache"
  pure $
    { context: Nothing
    , currentCategory: currentCategory
    , geometryCaches: geometryCaches
    , mainGeometryCache: mainGeometryCache
    }

geomFromString :: String -> Either JsonDecodeError GeometryState
geomFromString = parseJson >=> geomFromJson

tutorialFromGist :: Gist -> Either JsonDecodeError Tutorial
tutorialFromGist gist = (bisequence $ Tuple parsedGeom parsedWorld) <#> \x -> 
  { name: "name"
  , content: gist.files."content.md".content
  , geom: fst x
  , world: snd x
  }
  where
    parsedWorld :: Either JsonDecodeError World
    parsedWorld = worldFromString gist.files."world.json".content

    parsedGeom :: Either JsonDecodeError GeometryState
    parsedGeom = geomFromString gist.files."geom.json".content

-- | Takes a URL, and returns either an error message or a parsed tutorial wrapped in the Aff monad.
getTutorialFromURL :: String -> Aff (Either String Tutorial)
getTutorialFromURL url = do
  result <- AX.get ResponseFormat.json url
  pure $ (lmap (append "first stuff " <<< printError) result) >>= (\x -> lmap (append "gist stuff " <<< show) $ gistFromJson x.body) >>= (lmap (append "tutorial stuff " <<< show) <<< tutorialFromGist)