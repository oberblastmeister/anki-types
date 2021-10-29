{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Anki.Types.Params where

import Anki.Types.Action
import Anki.Types.Common
import Anki.Types.Notes
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Anki.Types.Decks
import Anki.Types.Statistics
import Anki.Types.TH (deriveParams, deriveResult)

deriveParams ''Action

deriveResult ''Action

data AnkiReq a p = AnkiReq
  { action :: a,
    params :: p
  }
  deriving (Show, Eq)

instance (p ~ Params a, Aeson.ToJSON p, isVoid ~ IsNil p, KnownBool isVoid) => Aeson.ToJSON (AnkiReq (SAction a) p) where
  toJSON AnkiReq {action, params} =
    Aeson.object $
      ["params" .= params | not $ boolSing $ Proxy @isVoid]
        ++ [ "action" .= action,
             "version" .= defaultVersion
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