{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.Html (lit, emph, para, heading, today, doc) where

import Grammata.Types
import Control.Monad.RWS
import Data.Time
import Data.ByteString.Builder (Builder, charUtf8, stringUtf8)

lit :: Monad m => String -> Doc m Inline
lit = return . escapeHtml

emph :: Monad m => Doc m Inline -> Doc m Inline
emph = fmap (Inline . inTag "em" . unInline)

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . inTag "p" . unInline)

heading :: Monad m => Int -> Doc m Inline -> Doc m Block
heading lev ils = do
  if lev < 1 || lev > 6
     then fail $ "Illegal header level " <> show lev
     else fmap (Block . inTag ("h" <> show lev) . unInline) ils

today :: Doc IO Inline
today = escapeHtml . show <$> liftIO (utctDay <$> getCurrentTime)

doc :: Monad m => Doc m Block -> Doc m Block
doc = id

-- utility functions
inTag :: String -> Builder -> Builder
inTag tag b = charUtf8 '<' <> stringUtf8 tag <> charUtf8 '>' <> b <>
  stringUtf8 "</" <> stringUtf8 tag <> charUtf8 '>'

escapeHtml :: String -> Inline
escapeHtml = Inline . mconcat . map escapeHtmlChar

escapeHtmlChar :: Char -> Builder
escapeHtmlChar '<' = stringUtf8 "&lt;"
escapeHtmlChar '>' = stringUtf8 "&gt;"
escapeHtmlChar '&' = stringUtf8 "&amp;"
escapeHtmlChar '"' = stringUtf8 "&quot;"
escapeHtmlChar c   = charUtf8 c
