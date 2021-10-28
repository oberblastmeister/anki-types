{-# LANGUAGE TemplateHaskell #-}

module Anki.Types.Action where

import Anki.Types.TH (deriveSingletons)
import Data.Aeson.TH
import qualified Data.Char as Char

data Action
  = AddNote
  | AddNotes
  | CanAddNotes
  | UpdateNoteFields
  | GetNumCardsReviewedToday
  | DeckNames
  deriving (Show, Eq)

-- creates SAction
deriveSingletons ''Action

-- create ToJSON instance for SAction
deriveToJSON
  defaultOptions
    { constructorTagModifier = \s ->
        case drop 1 s of
          [] -> error "cannot be empty"
          c : cs -> Char.toLower c : cs
    }
  ''SAction