{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Grammata.Parse where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))
import Data.Map as M
import qualified Data.Attoparsec.Text as P
import Grammata.Types
import Control.Applicative

type Parser = P.Parser

controlSeq :: Text -> Parser ()
controlSeq name = P.char '\\' *> P.string name *> P.skipSpace

parseBlocks :: (Foldable f, Format f, ToPara f) => Text -> Either String (Doc (f Block))
parseBlocks t = P.parseOnly (mconcat <$> P.many1 pBlock) t

parseInlines :: (Foldable f, Format f) => Text -> Either String (Doc (f Inline))
parseInlines t = P.parseOnly (mconcat <$> P.many1 pInline) t

pBlock :: (Foldable f, Format f, ToPara f) => P.Parser (Doc (f Block))
pBlock = pPara

pInline :: Format f => P.Parser (Doc (f Inline))
pInline = pText <|> pNewline

pText :: Format f => P.Parser (Doc (f Inline))
pText = text <$> P.takeWhile1 (P.notInClass "\n\\{}%")

pNewline :: Format f => P.Parser (Doc (f Inline))
pNewline = do
  P.char '\n'
  P.skipWhile (/= ' ')
  mbc <- P.peekChar
  if mbc == Just '\n' || mbc == Nothing
     then fail "blankline"
     else return (text " ")

pPara :: (Foldable f, Format f, ToPara f) => P.Parser (Doc (f Block))
pPara = para . mconcat <$> P.many1 pInline <* P.skipSpace

