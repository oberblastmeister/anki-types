module Anki.Types.Run where

import Anki.Types.Action (SAction)
import Anki.Types.Common
import Anki.Types.Params
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Network.HTTP.Req (req)
import qualified Network.HTTP.Req as Req

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