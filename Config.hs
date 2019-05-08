{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    git://code.devalot.com/sthenauth.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Configuration for encryption.

-}
module Sthenauth.Crypto.Config
  ( Config(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Text (Text)
import Dhall (Interpret)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Key (Key, Unchecked)

--------------------------------------------------------------------------------
-- | Configuration for the crypto module.
data Config = Config
  { key :: Key Unchecked
    -- ^ Unverified encryption key text.

  , salt :: Text
    -- ^ Unverified shared salt.

  } deriving (Generic, Interpret)
