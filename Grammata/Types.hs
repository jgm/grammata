{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grammata.Types where

import Data.Text (Text)
import Data.String
import Data.Map as M
import Data.Data
import Control.Monad.RWS
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy as BL

newtype Block = Block { unBlock :: Builder }

instance Monoid Block where
  mempty = Block mempty
  mappend (Block x) (Block y) = Block (x <> "\n" <> y)

newtype Inline = Inline { unInline :: Builder }
  deriving Monoid

class Renderable a where
  toBuilder :: a -> Builder

instance Renderable Inline where
  toBuilder (Inline b) = b

instance Renderable Block where
  toBuilder (Block b) = b

data DocState = DocState {
    vars :: M.Map Text Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

defDocState :: DocState
defDocState = DocState M.empty

data Message = Message Int Int Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

type Doc = RWST DocState [Message] DocState

runDoc :: Monad m => Doc m a -> m (a, DocState, [Message])
runDoc doc = do
  (_, s, _) <- runRWST doc defDocState defDocState
  runRWST doc defDocState s

instance Monad m => IsString (Doc m Inline) where
  fromString = return . Inline . B.stringUtf8

render :: (Monad m, Renderable a) => Doc m a -> m BL.ByteString
render = fmap (B.toLazyByteString . toBuilder . getFst) . runDoc
  where getFst (x, _, _) = x

instance (Monad m, Monoid a) => Monoid (Doc m a) where
  mempty = return mempty
  mappend = liftM2 mappend

