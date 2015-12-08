{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata (
  module Grammata.Types,
  module Grammata.Parse,
  ) where

import Grammata.Types
import Grammata.Parse

{-
-- example
mydoc :: Doc Block
mydoc = double $
  para ("This is a " <> emph "nice" <> "document")
  <> para "Another paragraph"

renderedDoc :: Text
renderedDoc = render mydoc

-- Generic (insensitive to format):
double :: Monoid a => Doc a -> Doc a
double t = t <> t
-}
