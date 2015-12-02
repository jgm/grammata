{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grammata.Format.PlainText where

import Grammata.Types

data PlainText

instance Format PlainText where
  lit = Doc . id

instance ToEmph PlainText where
  emph (Doc t) = Doc t

instance ToPara PlainText where
  para (Doc t) = Doc t
