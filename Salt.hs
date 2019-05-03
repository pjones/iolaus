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

Salt: random data mixed with a password to secure it while in rest
(i.e. stored in a database).

-}
module Sthenauth.Crypto.Salt
  ( Salt(..)
  , SharedSalt(..)
  , recommended
  , generate
  , generate'
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Random (MonadRandom(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Char8 (ByteString)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Encoding (Encoding(..))

--------------------------------------------------------------------------------
-- | A binary salt that should only be used for a single secret.
newtype Salt = Salt { getSalt :: ByteString }
  deriving Eq

--------------------------------------------------------------------------------
instance Show Salt where
  show (Salt bs) = show (Encoding bs)

--------------------------------------------------------------------------------
instance ToJSON Salt where
  toJSON (Salt bs) = toJSON (Encoding bs)
  toEncoding (Salt bs) = toEncoding (Encoding bs)

--------------------------------------------------------------------------------
instance FromJSON Salt where
  parseJSON = fmap (Salt . getBytes) . parseJSON

--------------------------------------------------------------------------------
-- | A password salt that is shared by all secrets in the system.
newtype SharedSalt = SharedSalt Salt
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | The recommended salt length (as per RFC 8018 section 4).
recommended :: Int
recommended = 8 -- 64 bits.

--------------------------------------------------------------------------------
-- | Generate a salt with the recommended length.
generate :: (MonadRandom m) => m Salt
generate = generate' recommended

--------------------------------------------------------------------------------
-- | Generate a salt with the given number of bytes (which may not be
-- less than the recommended value).
--
-- Attempting to generate fewer bytes than the recommended length will
-- automatically upgrade the length to the recommended value.
generate' :: (MonadRandom m) => Int -> m Salt
generate' = fmap Salt . getRandomBytes . max recommended
