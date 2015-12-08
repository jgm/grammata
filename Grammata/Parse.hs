{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Grammata.Parse (parse) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Data.Attoparsec.Text as P
import Control.Applicative
import Data.Char

type Parser = P.Parser

controlSeq :: Parser Text
controlSeq = P.char '\\' *> P.takeWhile1 isAlphaNum <* P.skipSpace

braced :: Parser Text
braced = do
  P.char '{'
  contents <- many (command <|> regular)
  P.char '}'
  return $ " (" <> (T.intercalate " <> " contents) <> ") "

command :: Parser Text
command = do
  cs <- controlSeq
  args <- many braced
  return $ cs <> mconcat args

regular :: Parser Text
regular = (T.pack . show) <$> P.takeWhile1 (P.notInClass "\\{}")

parse :: Text -> Either String Text
parse = P.parseOnly (T.intercalate " <> " <$> many (P.skipWhile isSpace >> command))

{-
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

parseInlines :: (Monoid (f Inline), Format f) => Text -> Either String (Doc (f Inline))
parseInlines t = P.parseOnly (mconcat <$> P.many1 pInline) t

parseBlocks :: (Monoid (f Block), Monoid (f Inline), Format f, ToPara f) => Text -> Either String (Doc (f Block))
parseBlocks t = P.parseOnly (mconcat <$> P.many1 pBlock) t

pBlock :: (Monoid (f Block), Monoid (f Inline), Format f, ToPara f) => P.Parser (Doc (f Block))
pBlock = pPara

pPara :: (Monoid (f Inline), Format f, ToPara f) => P.Parser (Doc (f Block))
pPara = para . mconcat <$> P.many1 pInline <* P.skipSpace
-}
