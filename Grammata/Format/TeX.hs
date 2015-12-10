{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.TeX (lit, today, emph, para, heading, doc) where

import Grammata.Types
import Control.Monad.RWS
import Data.Time
import Data.ByteString.Builder (Builder, stringUtf8, charUtf8)

lit :: Monad m => String -> Doc m Inline
lit = return . escapeTeX

today :: Doc IO Inline
today = escapeTeX . show <$> liftIO (utctDay <$> getCurrentTime)

emph :: Monad m => Doc m Inline -> Doc m Inline
emph t = "{\\it " <> t <> "}"

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . (<> "\n") . unInline)

heading :: Monad m => Int -> Doc m Inline -> Doc m Block
heading lev = fmap (Block . unInline)

doc :: Monad m => Doc m Block -> Doc m Block
doc d = d <> para "\\bye"

escapeTeX :: String -> Inline
escapeTeX = Inline . mconcat . map escapeTeXChar

-- TODO incomplete
escapeTeXChar :: Char -> Builder
escapeTeXChar '\\' = stringUtf8 "\\\\"
escapeTeXChar '{'  = stringUtf8 "\\{"
escapeTeXChar '}'  = stringUtf8 "\\}"
escapeTeXChar '%'  = stringUtf8 "\\%"
escapeTeXChar '$'  = stringUtf8 "\\$"
escapeTeXChar c    = charUtf8 c

