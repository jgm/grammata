{-# LANGUAGE TemplateHaskell #-}
module Grammata.TH (TypeSpec(..), toTypeSpec) where

import Grammata.Types
import Language.Haskell.TH hiding (Inline)
import Data.List (isPrefixOf)
import Data.Data
import Data.Maybe
import Debug.Trace

data TypeSpec =
  TyInt | TyText | TyInline | TyBlock
  deriving (Show, Read, Eq, Ord)

toTypeSpec :: String -> ExpQ
toTypeSpec cmd = do
  info <- reify (mkName cmd)
  listE $ map (stringE . show)
        $ fromMaybe [] $ extractTypeSpec info

extractTypeSpec :: Info -> Maybe [TypeSpec]
extractTypeSpec (VarI _ ty _ _) =
  case ty of
    ForallT
      [KindedTV _ (AppT (AppT ArrowT StarT) StarT)]
      [AppT (ConT n0) (VarT _)]
      (AppT
        (AppT
          ArrowT
          (AppT
            (AppT
              (ConT n1)
              (VarT _))
            (ConT n2)))
        (AppT
          (AppT
            (ConT n3)
            (VarT _))
          (ConT n4)))
      | n0 == ''Monad && n1 == ''Doc && n3 == ''Doc
        -> sequence $ map fromName [n2, n4]
    _ -> Nothing
extractTypeSpec _ = Nothing

fromName :: Name -> Maybe TypeSpec
fromName n =
  case n of
     _ | n == ''Inline -> Just TyInline
       | n == ''Block  -> Just TyBlock
       | otherwise     -> Nothing

