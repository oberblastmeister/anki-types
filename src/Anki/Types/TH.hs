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
    handleCon (NormalC conName []) =
      GadtC [mkName ("S" ++ nameBase conName)] [] <$> [t|$(conT sName) $(promotedT conName)|]
    handleCon _ = fail "Unsupported data type"

deriveParams :: Name -> Q [Dec]
deriveParams = deriveTypeFamily $ mkName "Params"

deriveResult :: Name -> Q [Dec]
deriveResult = deriveTypeFamily $ mkName "Result"

deriveTypeFamily :: Name -> Name -> Q [Dec]
deriveTypeFamily suffix name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  eqs <- traverse handleCon cons'
  pure [ClosedTypeFamilyD tyHead eqs]
  where
    tyHead = TypeFamilyHead suffix [KindedTV (mkName "a") (ConT name)] (KindSig StarT) Nothing
    handleCon (NormalC conName []) = do
      pure $
        TySynEqn
          Nothing
          (AppT (ConT suffix) (PromotedT conName))
          (ConT (mkName (nameBase conName ++ nameBase suffix)))
    handleCon _ = fail "unsupported data type"