{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

{-|

Copyright:
  This file is part of the package iolaus. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Configuration for encryption.

-}
module Iolaus.Crypto.Config
  ( Config(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)
import Dhall (Interpret)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key (Key, Unchecked)

--------------------------------------------------------------------------------
-- | Configuration for the crypto module.
data Config = Config
  { key :: Key Unchecked
    -- ^ Unverified encryption key text.

  , salt :: Text
    -- ^ Unverified shared salt.

  } deriving (Generic, Interpret)
