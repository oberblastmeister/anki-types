module Anki.Types.Common where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy (Proxy)

data Empty = Empty deriving (Eq, Ord, Show)

instance Aeson.ToJSON Empty where
  toJSON Empty = Aeson.Null

instance Aeson.FromJSON Empty where
  parseJSON Aeson.Null = pure Empty
  parseJSON (Aeson.Object o) | HashMap.null o = pure Empty
  parseJSON _ = fail "expected 'null' or '{}'"

data Nil = Nil

instance Aeson.ToJSON Nil where
  toJSON Nil = error "Should not be calling toJSON on this!"

class KnownBool (b :: Bool) where
  boolSing :: Proxy b -> Bool

instance KnownBool 'True where
  boolSing _ = True

instance KnownBool 'False where
  boolSing _ = False

type IsNil :: * -> Bool
type family IsNil a where
  IsNil Nil = 'True
  IsNil _ = 'False
