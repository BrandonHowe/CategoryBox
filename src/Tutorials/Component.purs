module CategoryBox.Tutorials.Component where

import Prelude

import CategoryBox.Foreign.Remark (reactMarkdown)
import CategoryBox.Tutorials.GetTutorial (Tutorial, getTutorialFromURL)
import CategoryBox.Tutorials.TutorialId (TutorialId)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

xButton :: Widget HTML Unit
xButton = D.div [ P.className "xButton", unit <$ P.onClick ] [ D.span [ P.className "xButtonCloseSpan" ] [ D.text "×" ] ]

tutorialComponentImpl :: Tutorial -> Widget HTML Unit
tutorialComponentImpl tutorial = do 
  _ <- D.div [ P.className "tutorialBox" ] [ D.div [ P.className "tutorialContent" ] [ xButton, reactMarkdown [] [ D.text tutorial.content ] ] ]
  pure unit

nonexistentTutorial :: Widget HTML Unit
nonexistentTutorial = do
  _ <- D.div [ P.className "tutorialBox" ] [ D.div [ P.className "tutorialContent" ] [ xButton, reactMarkdown [] [ D.text "# This tutorial doesn't exist!" ] ] ]
  pure unit

tutorialComponent :: TutorialId -> Widget HTML Unit
tutorialComponent id = join $ liftAff $ tutorial <#> \x -> case x of
  Left err -> nonexistentTutorial
  Right tut -> tutorialComponentImpl tut
  where
    tutorial :: Aff (Either String Tutorial)
    tutorial = getTutorialFromURL $ "https://api.github.com/gists/" <> unwrap id