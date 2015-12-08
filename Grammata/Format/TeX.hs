{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.TeX (text, emph, para, heading) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Grammata.Types

text :: Text -> Doc Inline
text = lit . escapeTeX

emph :: Doc Inline -> Doc Inline
emph t = "{\\em " <> t <> "}"

para :: Doc Inline -> Doc Block
para = fmap (Block . (<> "\n\n") . toText)

heading :: HeadingLevel -> Doc Inline -> Doc Block
heading lev = fmap (Block . toText)

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

