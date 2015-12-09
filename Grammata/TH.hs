{-# LANGUAGE TemplateHaskell #-}
module Grammata.TH (toTypeSpec) where

import Grammata.Types
import Language.Haskell.TH hiding (Inline)
import Data.List (isPrefixOf)
import Data.Data
import Data.Maybe
import Debug.Trace
import Data.Text (Text)

toTypeSpec :: String -> ExpQ
toTypeSpec cmd = do
  info <- reify (mkName cmd)
  listE $ map stringE $ fromMaybe [] $ extractTypeSpec info

extractTypeSpec :: Info -> Maybe [String]
extractTypeSpec (VarI _ ty _ _) =
  case traceShowId ty of
    AppT
      (AppT
        (ConT n1)
        (ConT _))
      (ConT n2)
      | n1 == ''Doc -> sequence [fromName n2]
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

fromName :: Name -> Maybe String
fromName n =
  case n of
     _ | n == ''Inline -> Just "Inline"
       | n == ''Block  -> Just "Block"
       | n == ''Int    -> Just "Int"
       | n == ''Text   -> Just "Text"
       | otherwise     -> Nothing

