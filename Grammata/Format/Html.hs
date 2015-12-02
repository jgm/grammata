{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata.Format.Html where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Grammata.Types

data Html

instance Format Html where
  lit = Doc . escapeHtml

instance ToEmph Html where
  emph (Doc t) = Doc (inTag "em" t)

instance ToPara Html where
  para (Doc t) = Doc (inTag "p" t)

instance ToHeading Html where
  heading lev (Doc t) = Doc (inTag ("h" <> T.pack (show lev)) t)

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

