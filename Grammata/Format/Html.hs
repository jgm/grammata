{-# LANGUAGE OverloadedStrings #-}

module Grammata.Format.Html (lit, emph, para, heading, cpuTime) where

import Data.Text (Text)
import qualified Data.Text as T
import Grammata.Types
import Control.Monad.RWS
import System.CPUTime

lit :: Monad m => Text -> Doc m Inline
lit = return . escapeHtml

emph :: Monad m => Doc m Inline -> Doc m Inline
emph = fmap (Inline . inTag "em" . toText)

para :: Monad m => Doc m Inline -> Doc m Block
para = fmap (Block . inTag "p" . toText)

heading :: Monad m => HeadingLevel -> Doc m Inline -> Doc m Block
heading lev = fmap (Block . inTag ("h" <> T.pack (unHeadingLevel lev)) . toText)

cpuTime :: Doc IO Inline
cpuTime = escapeHtml . T.pack . show <$> liftIO getCPUTime

-- utility functions
inTag :: Text -> Text -> Text
inTag tag t = "<" <> tag <> ">" <> t <> "</" <> tag <> ">"

escapeHtml :: Text -> Inline
escapeHtml = Inline . T.concatMap escapeHtmlChar

escapeHtmlChar :: Char -> Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = T.singleton c
