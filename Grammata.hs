{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))

-- | Formats
data Html
data TeX
data PlainText

-- | A document or document-part.
-- @f@ is a phantom type to track the format.
newtype Doc f = Doc { render :: Text }
 deriving (Show, Read, Eq, Ord, Data, Typeable, Monoid)

-- | 'lit' gives you a document with literal text contents
-- (properly escaped).
class FromText a where
  lit :: Text -> Doc a

instance FromText Html where
  lit = Doc . escapeHtml

instance FromText TeX where
  lit = Doc . escapeTeX

instance FromText PlainText where
  lit = Doc

-- character-escaping parsers
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeHtmlChar

escapeHtmlChar :: Char -> Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = T.singleton c

escapeTeX :: Text -> Text
escapeTeX = T.concatMap escapeHtmlChar

-- TODO incomplete
escapeTeXChar :: Char -> Text
escapeTeXChar '\\' = "\\\\"
escapeTeXChar '{'  = "\\{"
escapeTeXChar '}'  = "\\}"
escapeTeXChar '%'  = "\\%"
escapeTeXChar '$'  = "\\$"
escapeTeXChar c    = T.singleton c

-- Examples
hi :: Doc Html
hi = Doc "<em>hi</em>"

lo :: Doc Html
lo = Doc "lo"

foo :: Doc TeX
foo = Doc "{\\em hi}"

-- hi <> lo is a Doc Html
-- hi <> foo is a type erro


