{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata (
  module Grammata.Types,
  -- module Grammata.Parse,
  module Grammata.Format.Html,
  module Grammata.Format.TeX
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))

import Grammata.Types
-- import Grammata.Parse
import Grammata.Format.Html
import Grammata.Format.TeX

-- Generic (insensitive to format):
double :: Monoid a => Doc a -> Doc a
double t = t <> t

