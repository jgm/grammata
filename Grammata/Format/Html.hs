{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.Html (text, emph, para, heading) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Grammata.Types

text :: Text -> Doc Inline
text = lit . escapeHtml

emph :: Doc Inline -> Doc Inline
emph = fmap (Inline . inTag "em" . toText)

para :: Doc Inline -> Doc Block
para = fmap (Block . inTag "p" . toText)

heading :: HeadingLevel -> Doc Inline -> Doc Block
heading lev = fmap (Block . inTag ("h" <> T.pack (unHeadingLevel lev)) . toText)

-- utility functions
inTag :: Text -> Text -> Text
inTag tag t = "<" <> tag <> ">" <> t <> "</" <> tag <> ">"

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeHtmlChar

escapeHtmlChar :: Char -> Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = T.singleton c
