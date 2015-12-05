{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Grammata.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))
import Data.Map as M
import qualified Data.Attoparsec.Text as P
import Control.Monad.RWS

data Block
data Inline

data DocState = DocState {
    vars :: M.Map Text Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

type Doc = RWS DocState () DocState

runDoc :: Doc f -> (f, DocState)
runDoc doc =
  let (_, s, _)   = runRWS doc (DocState mempty) (DocState mempty)
      (res, s', _) = runRWS doc (DocState mempty) s
  in  (res, s')

render :: Format f => Doc (f c) -> Text
render = toText . fst . runDoc

instance Show f => Show (Doc f) where
  show x = "<" ++ show res ++ ", " ++ show s ++ ">"
      where (res, s) = runDoc x

instance Monoid a => Monoid (Doc a) where
  mempty = return mempty
  mappend x y = do{ xres <- x; yres <- y; return (xres <> yres) }

class Format f where
  text   :: Text -> Doc (f Inline)
  toText :: f c -> Text

instance Format f => IsString (Doc (f Inline)) where
  fromString = text . fromString

-- Inlines

class ToEmph f where
  emph :: Doc (f Inline) -> Doc (f Inline)

class ToStrong f where
  strong :: Doc (f Inline) -> Doc (f Inline)

data LinkData = LinkData { uri :: Text, title :: Text }

class ToLink f where
  link :: LinkData -> Doc (f Inline) -> Doc (f Inline)

-- Blocks

class ToPara f where
  para :: Doc (f Inline) -> Doc (f Block)

class ToBlockQuote f where
  blockQuote :: Doc (f Block) -> Doc (f Block)

data ListType =
    BulletList
  | OrderedList { start :: Int }
  deriving (Show, Read, Eq, Ord)

class ToList f where
  list :: ListType -> [Doc (f Block)] -> Doc (f Block)

class ToHeading f where
  heading :: Int -> Doc (f Inline) -> Doc (f Block)
