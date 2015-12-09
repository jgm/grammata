{-# LANGUAGE TemplateHaskell #-}
module Grammata.TH (toTypeSpec) where

import Grammata.Types
import Data.Maybe
import Language.Haskell.TH hiding (Inline)
-- import Debug.Trace
import Data.Text (Text)

toTypeSpec :: String -> ExpQ
toTypeSpec cmd = do
  mbName <- lookupValueName cmd
  case mbName of
       Nothing -> listE []
       Just name -> do
         info <- reify name
         listE $ map stringE $ fromMaybe [] $ extractTypeSpec info

extractTypeSpec :: Info -> Maybe [String]
extractTypeSpec (VarI _ ty _ _) = typeSpecOf ty
extractTypeSpec _ = Nothing

typeSpecOf :: Type -> Maybe [String]
typeSpecOf ty =
  case ty of
    ConT n1   -> sequence [fromName n1]
    AppT
      (AppT
        (ConT n1)
        _)
      (ConT n2)
      | n1 == ''Doc -> sequence [fromName n2]
    AppT
      (AppT ArrowT x)
      y -> case (typeSpecOf x, typeSpecOf y) of
                (Just xt, Just yt) -> Just (xt ++ yt)
                _                  -> Nothing
    ForallT
      [KindedTV _ (AppT (AppT ArrowT StarT) StarT)]
      [AppT (ConT n0) (VarT _)]
      x | n0 == ''Monad -> typeSpecOf x
    _ -> Nothing

fromName :: Name -> Maybe String
fromName n =
  case n of
     _ | n == ''Inline       -> Just "Inline"
       | n == ''Block        -> Just "Block"
       | n == ''Int          -> Just "Int"
       | n == ''Text         -> Just "Text"
       | n == ''HeadingLevel -> Just "HeadingLevel"
       | otherwise           -> Just (show n)

