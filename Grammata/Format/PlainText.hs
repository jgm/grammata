{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata.Format.PlainText (PlainText) where

import Grammata.Types

data PlainText

instance Format PlainText where
  lit = Doc mempty . id

instance ToEmph PlainText where
  emph = id

instance ToPara PlainText where
  para (Doc v t) = Doc v t
