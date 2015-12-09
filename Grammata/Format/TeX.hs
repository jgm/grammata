{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.TeX (lit, cpuTime, emph, para, heading) where

import Grammata.Types
import Control.Monad.RWS
import System.CPUTime
import Data.ByteString.Builder (Builder, stringUtf8, charUtf8)

lit :: Monad m => String -> Doc m Inline
lit = return . escapeTeX

cpuTime :: Doc IO Inline
cpuTime = escapeTeX . show <$> liftIO getCPUTime

emph :: Monad m => Doc m Inline -> Doc m Inline
emph t = "{\\it " <> t <> "}"

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . (<> "\n") . unInline)

heading :: Monad m => HeadingLevel -> Doc m Inline -> Doc m Block
heading lev = fmap (Block . unInline)

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

