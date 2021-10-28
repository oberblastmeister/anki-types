{-# LANGUAGE TemplateHaskell #-}

module Anki.Types.TH where

import Language.Haskell.TH

deriveSingletons :: Name -> Q [Dec]
deriveSingletons name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  cons'' <- traverse handleCon cons'
  pure [DataD [] sName [KindedTV (mkName "a") (ConT name)] (Just StarT) cons'' []]
  where
    sName = mkName ("S" ++ nameBase name)
    handleCon (NormalC conName []) = do
      GadtC [mkName ("S" ++ nameBase conName)] [] <$> [t|$(conT sName) $(promotedT conName)|]
    handleCon _ = fail "Unsupported data type"