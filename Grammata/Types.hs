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

newtype Block = Block Text
  deriving (Read, Show, Eq, Ord, Monoid, Data, Typeable)

newtype Inline = Inline Text
  deriving (Read, Show, Eq, Ord, Monoid, Data, Typeable)

class Renderable a where
  toText :: a -> Text

instance Renderable Inline where
  toText (Inline t) = t

instance Renderable Block where
  toText (Block t) = t

data DocState = DocState {
    vars :: M.Map Text Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

type Doc = RWS DocState () DocState

runDoc :: Doc a -> (a, DocState)
runDoc doc =
  let (_, s, _)   = runRWS doc (DocState mempty) (DocState mempty)
      (res, s', _) = runRWS doc (DocState mempty) s
  in  (res, s')

instance IsString (Doc Inline) where
  fromString = return . Inline . fromString

render :: Renderable a => Doc a -> Text
render = toText . fst . runDoc

instance Show a => Show (Doc a) where
  show x = "<" ++ show res ++ ", " ++ show s ++ ">"
      where (res, s) = runDoc x

instance Monoid (Doc Inline) where
  mempty = return mempty
  mappend x y = do{ xres <- x; yres <- y; return (xres <> yres) }

instance Monoid (Doc Block) where
  mempty = return mempty
  mappend x y = do Block xres <- x
                   let endsInNewline = T.null xres || T.last xres == '\n'
                   Block yres <- y
                   return $ Block
                     (xres <> if endsInNewline then yres else ("\n" <> yres))

newtype HeadingLevel = HeadingLevel { unHeadingLevel :: String }

instance IsString HeadingLevel where
  fromString x = HeadingLevel x

{-
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
-}
