{-# LANGUAGE OverloadedStrings #-}

module Grammata.Base.Tex (lit, emph, today, para, heading, doc) where

import Grammata.Types
import Data.Monoid
import Grammata.Base.Common (todayS)
import Data.ByteString.Builder (Builder, stringUtf8, charUtf8)

lit :: Monad m => String -> Doc m Inline
lit = return . escapeTeX

emph :: Monad m => Doc m Inline -> Doc m Inline
emph t = "{\\it " <> t <> "}"

today :: Doc IO Inline
today = escapeTeX <$> todayS

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . (<> "\n") . unInline)

heading :: Monad m => HeaderLevel -> Doc m Inline -> Doc m Block
heading _lev = fmap (Block . (\t -> "\\beginsection " <> t <> "\n") . unInline)

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

