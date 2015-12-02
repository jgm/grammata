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
import Data.Map as M

-- | Contexts
data Inline
data Block

-- | A document or document-part.
-- @c@ is a phantom type to track the context.
-- @f@ is a phantom type to track the format.
data Doc c f = Doc { variables :: M.Map Text Text
                   , body      :: Text
                   }
 deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Monoid (Doc Inline f) where
  mappend (Doc v1 t1) (Doc v2 t2) = Doc (v1 <> v2) (t1 <> t2)
  mempty = Doc mempty mempty

instance Monoid (Doc Block f) where
  mappend (Doc v1 t1) (Doc v2 t2) = Doc (v1 <> v2) (t1 <> "\n\n" <> t2)
  mempty = Doc mempty mempty

class Format a where
  lit :: Text -> Doc Inline a

instance Format f => IsString (Doc Inline f) where
  fromString = lit . fromString

class ToEmph a where
  emph :: Doc Inline a -> Doc Inline a

class ToStrong a where
  strong :: Doc Inline a -> Doc Inline a

data LinkData = LinkData { uri :: Text, title :: Text }

class ToLink a where
  link :: LinkData -> Doc Inline a -> Doc Inline a

class ToPara a where
  para :: Doc Inline a -> Doc Block a

class ToBlockQuote a where
  blockQuote :: Doc Block a -> Doc Block a

data ListType =
    BulletList
  | OrderedList { start :: Int }
  deriving (Show, Read, Eq, Ord)

class ToList a where
  list :: ListType -> [Doc Block a] -> Doc Block a

class ToHeading a where
  heading :: Int -> Doc Inline a -> Doc Block a

