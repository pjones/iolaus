{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RecordWildCards   #-}

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

Run-time environment for encryption.

-}
module Sthenauth.Crypto.Env
  ( Crypto(..)
  , Error
  , env
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Except (MonadError, liftEither)
import Crypto.Cipher.AES (AES256)
import Data.Bifunctor (first)
import Data.Text.Encoding (encodeUtf8)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Config (Config)
import qualified Sthenauth.Crypto.Config as Config
import Sthenauth.Crypto.Key (Key)
import qualified Sthenauth.Crypto.Key as Key
import Sthenauth.Crypto.Salt (SharedSalt(..))
import qualified Sthenauth.Crypto.Salt as Salt

--------------------------------------------------------------------------------
-- | Environment needed for encryption.
data Crypto = Crypto
  { key :: Key AES256
    -- ^ Encryption key to protect fields in the database.

  , salt :: SharedSalt
    -- ^ Shared salt for hashing passwords and other values.

  }

--------------------------------------------------------------------------------
-- | Errors that might occur while constructing an environment.
data Error = InvalidKeyLength
           | InvalidSaltLength
           deriving Show

--------------------------------------------------------------------------------
-- | Construct a crypto environment from a configuration value.
env :: (MonadError Error m) => Config -> m Crypto
env config = do
  key <- liftEither $ first (const InvalidKeyLength) $
    Key.convert (Config.key config)

  -- FIXME: make it impossible to create invalid salt values.
  let saltBS = encodeUtf8 (Config.salt config) 
                               -- 
  salt <- liftEither $ first (const InvalidSaltLength) $
    Salt.sharedSalt saltBS

  pure Crypto{..}
