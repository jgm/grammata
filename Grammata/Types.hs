{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grammata.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Map as M
import Data.Data
import Data.Monoid

newtype Block = Block Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance Monoid Block where
  mempty = mempty
  mappend (Block x) (Block y) =
    let endsInNewline = T.null x || T.last x == '\n'
    in  Block (x <> if endsInNewline then y else ("\n" <> y))

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

defDocState :: DocState
defDocState = DocState mempty

newtype Doc a = Doc { run :: DocState -> DocState -> (a, DocState) }

doc :: a -> Doc a
doc a = Doc $ \s _ -> (a, s)

ask :: (DocState -> a) -> Doc a
ask f = Doc $ \s r -> (f r, s)

get :: (DocState -> a) -> Doc a
get f = Doc $ \s _ -> (f s, s)

modify :: Monoid m => (DocState -> DocState) -> Doc m
modify f = Doc $ \s _ -> (mempty, f s)

instance Functor Doc where
  fmap f (Doc g) = Doc $ \s r -> let (a, s') = g s r in  (f a, s')

instance Monoid a => Monoid (Doc a) where
  mempty = Doc $ \_ _ -> (mempty, defDocState)
  mappend (Doc f) (Doc g) = Doc $ \s r ->
    let (a1, s1) = f s r
        (a2, s2) = g s1 r
    in  (mappend a1 a2, s2)

runDoc :: Doc a -> (a, DocState)
runDoc d =
  let (_, s)    = run d defDocState defDocState
      (res, s') = run d defDocState s
  in  (res, s')

lit :: Text -> Doc Inline
lit t = doc (Inline t)

instance IsString (Doc Inline) where
  fromString = lit . fromString

render :: Renderable a => Doc a -> Text
render = toText . fst . runDoc

instance Show a => Show (Doc a) where
  show x = "<" ++ show res ++ ", " ++ show s ++ ">"
      where (res, s) = runDoc x

newtype HeadingLevel = HeadingLevel { unHeadingLevel :: String }

instance IsString HeadingLevel where
  fromString = HeadingLevel

