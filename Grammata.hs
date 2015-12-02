{-# LANGUAGE FlexibleInstances #-}
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

-- | Contexts
data Inline
data Block

-- | A document or document-part.
-- @c@ is a phantom type to track the context.
-- @f@ is a phantom type to track the format.
newtype Doc c f = Doc { render :: Text }
 deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Monoid (Doc Inline f) where
  mappend (Doc t1) (Doc t2) = Doc (t1 <> t2)
  mempty = Doc mempty

instance Monoid (Doc Block f) where
  mappend (Doc t1) (Doc t2) = Doc (t1 <> "\n\n" <> t2)
  mempty = Doc mempty

class Format a where
  lit :: Text -> Doc Inline a

instance Format Html where
  lit = Doc . escapeHtml

instance Format TeX where
  lit = Doc . escapeTeX

instance Format PlainText where
  lit = Doc . id

instance Format f => IsString (Doc Inline f) where
  fromString = lit . fromString

litfoo :: Doc Inline Html
litfoo = "foo&bar" -- same as lit "foo&bar" because of IsString instance

-- Examples
hi :: Doc Inline Html
hi = Doc "<em>hi</em>"

lo :: Doc Inline Html
lo = Doc "lo"

hiblock :: Doc Block Html
hiblock = Doc "<p>hi</p>"

-- hi <> lo is a Doc Html
-- hiblock <> lo is a type erro

class ToEmph a where
  emph :: Doc Inline a -> Doc Inline a

instance ToEmph Html where
  emph (Doc t) = Doc (inTag "em" t)

instance ToEmph PlainText where
  emph (Doc t) = Doc t

instance ToEmph TeX where
  emph (Doc t) = Doc ("{\\em " <> t <> "}")

emphLo :: Doc Inline Html
emphLo = emph lo

-- emph hiblock should be an error

-- emph "hi" :: Doc Inline TeX
-- but not: emph "hi" :: Doc Block TeX

class ToPara a where
  para :: Doc Inline a -> Doc Block a

instance ToPara Html where
  para (Doc t) = Doc (inTag "p" t)

instance ToPara TeX where
  para (Doc t) = Doc t

instance ToPara PlainText where
  para (Doc t) = Doc t

-- para (emph "hi" <> "lo") <> para "lo" :: Doc Block TeX
-- para (emph "hi" <> "lo") <> para "lo" :: Doc Block Html

---
-- Now, let's say I want to introduce a new format.  All I
-- need to do is define some type class instances, depending
-- on the language features I need:

data Xml

instance Format Xml where
  lit = Doc . escapeHtml

instance ToPara Xml where
  para (Doc t) = Doc (inTag "para" t)

instance ToEmph Xml where
  emph (Doc t) = Doc (inTag "emphasis" t)

-- para (emph "hi" <> "lo") <> para "lo" :: Doc Block Xml
---


-- utility functions
inTag :: Text -> Text -> Text
inTag tag t = "<" <> tag <> ">" <> t <> "</" <> tag <> ">"

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


