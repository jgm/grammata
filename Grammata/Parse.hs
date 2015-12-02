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

parseBlocks :: (Format a, ToPara a) => Text -> Either String (Doc Block a)
parseBlocks t = P.parseOnly (mconcat <$> P.many1 pBlock) t

parseInlines :: Format a => Text -> Either String (Doc Inline a)
parseInlines t = P.parseOnly (mconcat <$> P.many1 pInline) t

pBlock :: (Format a, ToPara a) => P.Parser (Doc Block a)
pBlock = pPara

pInline :: Format a => P.Parser (Doc Inline a)
pInline = pText <|> pNewline

pText :: Format a => P.Parser (Doc Inline a)
pText = lit <$> P.takeWhile1 (P.notInClass "\n\\{}%")

pNewline :: Format a => P.Parser (Doc Inline a)
pNewline = do
  P.char '\n'
  P.skipWhile (/= ' ')
  mbc <- P.peekChar
  if mbc == Just '\n' || mbc == Nothing
     then fail "blankline"
     else return (lit " ")

pPara :: (Format a, ToPara a) => P.Parser (Doc Block a)
pPara = para . mconcat <$> P.many1 pInline <* P.skipSpace

