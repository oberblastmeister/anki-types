{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Anki.Types
  ( Action,
    SAction (..),
    Params,
    AddNoteParams,
    Media,
    TargetKind,
    Result,
    AddNoteResult,
    run,
    runWith,
    Empty (Empty),
    Nil (Nil),
  )
where

import Anki.Types.Common
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as Aeson.Types
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Base (Nat)
import GHC.TypeNats (natVal)
import Network.HTTP.Req (req, (/:))
import qualified Network.HTTP.Req as Req

data Action
  = AddNote
  | AddNotes
  | CanAddNotes
  | UpdateNoteFields
  | GetNumCardsReviewedToday
  | DeckNames
  deriving (Show, Eq)

type SAction :: Action -> *
data SAction a where
  SAddNote :: SAction 'AddNote
  SAddNotes :: SAction 'AddNotes
  SCanAddNotes :: SAction 'CanAddNotes
  SUpdateNoteFields :: SAction 'UpdateNoteFields
  SGetNumCardsReviewedToday :: SAction 'GetNumCardsReviewedToday
  SDeckNames :: SAction 'DeckNames

instance Aeson.ToJSON (SAction a) where
  toJSON = \case
    SAddNote -> "addNote"
    SAddNotes -> "addNotes"
    SCanAddNotes -> "canAddNotes"
    SUpdateNoteFields -> "updateNoteFields"
    SGetNumCardsReviewedToday -> "getNumCardsReviewedToday"
    SDeckNames -> "deckNames"

type Params :: Action -> *
type family Params a where
  Params 'AddNote = AddNoteParams
  Params 'GetNumCardsReviewedToday = Nil
  Params 'DeckNames = Nil

type IsNil :: * -> Bool
type family IsNil a where
  IsNil Nil = 'True
  IsNil _ = 'False

-- type ConvertVoid :: * -> *
-- type family ConvertVoid a where
--   ConvertVoid Nil = ()
--   ConvertVoid x = x

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

data TargetKind = Data | Path | Url
  deriving (Show, Eq)

type Result :: Action -> *
type family Result a where
  Result 'AddNote = AddNoteResult
  Result 'GetNumCardsReviewedToday = Int
  Result 'DeckNames = [Text]

data AddNoteResult = AddNoteResult

instance Aeson.ToJSON Media where
  toJSON = undefined

$(deriveToJSON defaultOptions {fieldLabelModifier = drop 1} ''AddNoteParams)

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

run ::
  forall a p r m.
  ( Req.MonadHttp m,
    p ~ Params a,
    Aeson.ToJSON p,
    KnownBool (IsNil p),
    r ~ Result a,
    Aeson.FromJSON r,
    Show r
  ) =>
  SAction a ->
  p ->
  m (Either Text r)
run = runWith 8765

runWith ::
  forall a p r m.
  ( Req.MonadHttp m,
    p ~ Params a,
    KnownBool (IsNil p),
    Aeson.ToJSON p,
    r ~ Result a,
    Aeson.FromJSON r,
    Show r
  ) =>
  Int ->
  SAction a ->
  p ->
  m (Either Text r)
runWith port action params = do
  let payload = AnkiReq {action, params}
  liftIO $ print $ Aeson.toJSON payload
  response <- req Req.POST (Req.http "localhost") (Req.ReqBodyJson payload) Req.jsonResponse (Req.port port)
  let (AnkiResult {result, ankiError} :: AnkiResult a r) = Req.responseBody response
  liftIO $ print result
  liftIO $ print ankiError
  return $ case ankiError of
    Nothing -> Right result
    Just e -> Left e