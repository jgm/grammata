{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Grammata.Format.TeX (TeX) where

import Data.Foldable
import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Data
import Data.Typeable
import Grammata.Types

newtype TeX c = TeX Text
  deriving (Read, Show, Eq, Ord, Monoid, Data, Typeable, Functor, Foldable, Traversable)

instance Format TeX where
  text = return . escapeTeX
  toText (TeX x) = x

instance ToEmph TeX where
  emph = fmap (\t -> TeX "{\\em " <> t <> TeX "}")

instance ToPara TeX where
  para x = do (TeX t) <- x
              return (TeX (t <> "\n\n"))

escapeTeX :: Text -> TeX Inline
escapeTeX = TeX . T.concatMap escapeTeXChar

-- TODO incomplete
escapeTeXChar :: Char -> Text
escapeTeXChar '\\' = "\\\\"
escapeTeXChar '{'  = "\\{"
escapeTeXChar '}'  = "\\}"
escapeTeXChar '%'  = "\\%"
escapeTeXChar '$'  = "\\$"
escapeTeXChar c    = T.singleton c

