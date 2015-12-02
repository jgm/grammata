{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata (
  module Grammata.Types,
  module Grammata.Format.Html,
  module Grammata.Format.PlainText,
  module Grammata.Format.TeX
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.String
import Data.Data
import Data.Typeable
import Data.Monoid ((<>))

import Grammata.Types
import Grammata.Format.Html
import Grammata.Format.PlainText
import Grammata.Format.TeX

litfoo :: Doc Inline Html
litfoo = "foo&bar" -- same as lit "foo&bar" because of IsString instance

-- Examples
hi :: Doc Inline Html
hi = Doc "<em>hi</em>"

lo :: Doc Inline Html
lo = Doc "lo"

hiblock :: Doc Block Html
hiblock = Doc "<p>hi</p>"

-- hi <> lo is a Doc Html
-- hiblock <> lo is a type erro

emphLo :: Doc Inline Html
emphLo = emph lo

-- emph hiblock should be an error

-- emph "hi" :: Doc Inline TeX
-- but not: emph "hi" :: Doc Block TeX

-- para (emph "hi" <> "lo") <> para "lo" :: Doc Block TeX
-- para (emph "hi" <> "lo") <> para "lo" :: Doc Block Html

