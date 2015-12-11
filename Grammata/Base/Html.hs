{-# LANGUAGE OverloadedStrings #-}

module Grammata.Base.Html (lit, emph, today, chart, para, heading, doc) where

import Grammata.Types
import Data.Monoid
import Grammata.Base.Common (todayS)
import Data.ByteString.Builder (Builder, charUtf8, stringUtf8, lazyByteString)
import Grammata.Chart
import Grammata.Util (showInterpreterError)
import Control.Monad.RWS

lit :: Monad m => String -> Doc m Inline
lit = return . escapeHtml

emph :: Monad m => Doc m Inline -> Doc m Inline
emph = fmap (Inline . inTag "em" . unInline)

today :: Doc IO Inline
today = escapeHtml <$> todayS

chart :: String -> Doc IO Block
chart chraw = do
  res <- parseChart chraw
  case res of
       Right ch -> (Block . lazyByteString) <$> liftIO (toSVG (300.0, 300.0) ch)
       Left err -> fail (showInterpreterError err)

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . inTag "p" . unInline)

heading :: Monad m => HeaderLevel -> Doc m Inline -> Doc m Block
heading (HeaderLevel lev) ils = fmap (Block . inTag ("h" <> lev) . unInline) ils

doc :: Monad m => Doc m Block -> Doc m Block
doc d = return (Block "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n</head>\n<body>\n") <> d <>
        return (Block "\n</body>\n</html>")

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
