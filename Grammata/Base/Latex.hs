{-# LANGUAGE OverloadedStrings #-}

module Grammata.Base.Latex (lit, emph, today, para, heading, doc) where

import Grammata.Types
import Data.Monoid
import Grammata.Base.Common (todayS)
import Data.ByteString.Builder (Builder, stringUtf8, charUtf8)

lit :: Monad m => String -> Doc m Inline
lit = return . escapeTeX

emph :: Monad m => Doc m Inline -> Doc m Inline
emph t = "\\emph{" <> t <> "}"

today :: Doc IO Inline
today = escapeTeX <$> todayS

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . (<> "\n") . unInline)

heading :: Monad m => HeaderLevel -> Doc m Inline -> Doc m Block
heading (HeaderLevel lev) =
  fmap (Block . (\t -> "\\" <> cmd <> "{" <> t <> "}\n") . unInline)
  where cmd = case lev of
                    "1" -> "section*"
                    "2" -> "subsection*"
                    "3" -> "paragraph*"
                    "4" -> "subparagraph*"
                    _   -> "para"

doc :: Monad m => Doc m Block -> Doc m Block
doc d = para "\\documentclass{article}\n\\usepackage{fontspec}\n\\begin{document}\n\\fontsize{12}{15}\\fontspec[Ligatures={Common,Rare}, Alternate=1]{Hoefler Text}\n" <> d <> para "\\end{document}"

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

