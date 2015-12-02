{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata.Format.TeX where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Grammata.Types

data TeX

instance Format TeX where
  lit t = Doc mempty (escapeTeX t)

instance ToEmph TeX where
  emph (Doc v t) = Doc v ("{\\em " <> t <> "}")

instance ToPara TeX where
  para (Doc v t) = Doc v t

escapeTeX :: Text -> Text
escapeTeX = T.concatMap escapeTeXChar

-- TODO incomplete
escapeTeXChar :: Char -> Text
escapeTeXChar '\\' = "\\\\"
escapeTeXChar '{'  = "\\{"
escapeTeXChar '}'  = "\\}"
escapeTeXChar '%'  = "\\%"
escapeTeXChar '$'  = "\\$"
escapeTeXChar c    = T.singleton c

