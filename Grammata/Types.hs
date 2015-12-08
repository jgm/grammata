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

type Doc = RWS DocState [Message] DocState

runDoc :: Doc a -> (a, DocState, [Message])
runDoc doc =
  let (_, s, _)    = runRWS doc defDocState defDocState
  in  runRWS doc defDocState s

instance IsString (Doc Inline) where
  fromString = return . Inline . fromString

render :: Renderable a => Doc a -> Text
render d = toText x
  where (x, _, _) = runDoc d

instance Show a => Show (Doc a) where
  show x = "<" ++ show res ++ ", " ++ show s ++ ", " ++ show ms ++ ">"
      where (res, s, ms) = runDoc x

instance Monoid a => Monoid (Doc a) where
  mempty = return mempty
  mappend = liftM2 mappend

instance Monoid Block where
  mempty = mempty
  mappend (Block x) (Block y) =
    let endsInNewline = T.null x || T.last x == '\n'
    in  Block (x <> if endsInNewline then y else ("\n" <> y))

newtype HeadingLevel = HeadingLevel { unHeadingLevel :: String }

instance IsString HeadingLevel where
  fromString = HeadingLevel

