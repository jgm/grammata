{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Grammata.Format.Html (Html) where

import Data.Foldable
import Data.Traversable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Data
import Data.Typeable
import Grammata.Types

newtype Html c = Html Text
  deriving (Read, Show, Eq, Ord, Monoid, Data, Typeable, Functor, Foldable, Traversable)

instance Format Html where
  text = return . escapeHtml
  toText (Html x) = x

instance ToEmph Html where
  emph = fmap (inTag "em")

instance ToPara Html where
  para = fmap (inTag "p")

instance ToHeading Html where
  heading lev = fmap (inTag ("h" <> T.pack (show lev)))


-- utility functions
inTag :: Text -> Html c -> Html d
inTag tag (Html t) =
  Html ("<" <> tag <> ">") <> Html t <> Html ("</" <> tag <> ">")

escapeHtml :: Text -> Html Inline
escapeHtml = Html . T.concatMap escapeHtmlChar

escapeHtmlChar :: Char -> Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = T.singleton c
