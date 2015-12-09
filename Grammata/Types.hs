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

newtype Block = Block Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

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
defDocState = DocState M.empty

data Message = Message Int Int Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

type Doc = RWST DocState [Message] DocState

runDoc :: Monad m => Doc m a -> m (a, DocState, [Message])
runDoc doc = do
  (_, s, _) <- runRWST doc defDocState defDocState
  runRWST doc defDocState s

instance Monad m => IsString (Doc m Inline) where
  fromString = return . Inline . fromString

render :: (Monad m, Renderable a) => Doc m a -> m Text
render = fmap (toText . getFst) . runDoc
  where getFst (x, _, _) = x

instance (Monad m, Monoid a) => Monoid (Doc m a) where
  mempty = return mempty
  mappend = liftM2 mappend

instance Monoid Block where
  mempty = Block mempty
  mappend (Block x) (Block y) = Block (x <> "\n" <> y)

newtype HeadingLevel = HeadingLevel { unHeadingLevel :: String }

instance IsString HeadingLevel where
  fromString = HeadingLevel

