{-# LANGUAGE TemplateHaskell #-}

module Anki.Types.Notes where

import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data AddNoteParams = AddNoteParams
  { _deckName :: Text,
    _modelName :: Text,
    _fields :: HashMap Text Text,
    _options :: HashMap Text Text,
    _tags :: [Text],
    _audio :: [Media],
    _video :: [Media],
    _picture :: [Media]
  }
  deriving (Show, Eq)

data Media = Media
  { _filename :: Text,
    _target :: (TargetKind, Text),
    _skipHash :: Text,
    _fields :: [Text]
  }
  deriving (Show, Eq)

instance Aeson.ToJSON Media where
  toJSON = undefined

data TargetKind = Data | Path | Url
  deriving (Show, Eq)

data AddNoteResult = AddNoteResult

deriveToJSON defaultOptions {fieldLabelModifier = drop 1} ''AddNoteParams