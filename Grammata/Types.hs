{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))

-- | Contexts
data Inline
data Block

-- | A document or document-part.
-- @c@ is a phantom type to track the context.
-- @f@ is a phantom type to track the format.
newtype Doc c f = Doc { render :: Text }
 deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Monoid (Doc Inline f) where
  mappend (Doc t1) (Doc t2) = Doc (t1 <> t2)
  mempty = Doc mempty

instance Monoid (Doc Block f) where
  mappend (Doc t1) (Doc t2) = Doc (t1 <> "\n\n" <> t2)
  mempty = Doc mempty

class Format a where
  lit :: Text -> Doc Inline a

instance Format f => IsString (Doc Inline f) where
  fromString = lit . fromString

class ToEmph a where
  emph :: Doc Inline a -> Doc Inline a

class ToPara a where
  para :: Doc Inline a -> Doc Block a

