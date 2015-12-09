{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.TeX (lit, cpuTime, emph, para, heading) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Grammata.Types
import Control.Monad.RWS
import System.CPUTime

lit :: Monad m => Text -> Doc m Inline
lit = return . escapeTeX

cpuTime :: Doc IO Inline
cpuTime = escapeTeX . T.pack . show <$> liftIO getCPUTime

emph :: Monad m => Doc m Inline -> Doc m Inline
emph t = "{\\it " <> t <> "}"

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . (<> "\n\n") . toText)

heading :: Monad m => HeadingLevel -> Doc m Inline -> Doc m Block
heading lev = fmap (Block . toText)

escapeTeX :: Text -> Inline
escapeTeX = Inline . T.concatMap escapeTeXChar

-- TODO incomplete
escapeTeXChar :: Char -> Text
escapeTeXChar '\\' = "\\\\"
escapeTeXChar '{'  = "\\{"
escapeTeXChar '}'  = "\\}"
escapeTeXChar '%'  = "\\%"
escapeTeXChar '$'  = "\\$"
escapeTeXChar c    = T.singleton c

