module CategoryBox.Tutorials.TutorialId where

import Prelude

import Data.Newtype (class Newtype)

-- | String ID used to identify tutorials
newtype TutorialId = TutorialId String

derive instance eqTutorialId :: Eq TutorialId

derive instance ordTutorialId :: Ord TutorialId

derive instance newtypeTutorialId :: Newtype TutorialId _

derive newtype instance showTutorialId :: Show TutorialId
