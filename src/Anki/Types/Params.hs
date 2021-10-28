{-# LANGUAGE UndecidableInstances #-}

module Anki.Types.Params where

import Anki.Types.Action
import Anki.Types.Common
import Anki.Types.Notes
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

type Params :: Action -> *
type family Params a where
  Params 'AddNote = AddNoteParams
  Params 'GetNumCardsReviewedToday = Nil
  Params 'DeckNames = Nil

type IsNil :: * -> Bool
type family IsNil a where
  IsNil Nil = 'True
  IsNil _ = 'False

type Result :: Action -> *
type family Result a where
  Result 'AddNote = AddNoteResult
  Result 'GetNumCardsReviewedToday = Int
  Result 'DeckNames = [Text]

data AnkiReq a p = AnkiReq
  { action :: a,
    params :: p
  }
  deriving (Show, Eq)

instance (p ~ Params a, Aeson.ToJSON p, isVoid ~ IsNil p, KnownBool isVoid) => Aeson.ToJSON (AnkiReq (SAction a) p) where
  toJSON AnkiReq {action, params} =
    Aeson.object $
      if boolSing (Proxy @isVoid)
        then
          [ "action" .= action,
            "version" .= defaultVersion
          ]
        else
          [ "action" .= action,
            "version" .= defaultVersion,
            "params" .= params
          ]

data AnkiResult a r = AnkiResult
  { result :: r,
    ankiError :: Maybe Text
  }
  deriving (Show, Eq)

defaultVersion :: Int
defaultVersion = 6

instance (p ~ Params a, r ~ Result a, Aeson.FromJSON r) => Aeson.FromJSON (AnkiResult a r) where
  parseJSON (Aeson.Object v) =
    AnkiResult
      <$> v .: "result"
      <*> v .: "error"
  parseJSON invalid =
    Aeson.Types.prependFailure
      "parsing AnkiResult failed, "
      (Aeson.Types.typeMismatch "Object" invalid)
