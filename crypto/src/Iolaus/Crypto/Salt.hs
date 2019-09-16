{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

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
  ( Salt
  , SharedSalt(..)
  , getSalt
  , salt
  , sharedSalt
  , recommended
  , generate
  , generate'
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Iolaus.Opaleye.Newtype (makeNewtypeInstances)
import Crypto.Random (MonadRandom(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Opaleye.SqlTypes (SqlBytea)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Encoding (Encoding(..))
import Sthenauth.Crypto.Error (CryptoError(..))

--------------------------------------------------------------------------------
-- | A binary salt that should only be used for a single secret.
newtype Salt = Salt { getSalt :: ByteString } deriving Eq

makeNewtypeInstances ''Salt ''SqlBytea

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
-- | Convert a 'ByteString' into 'Salt'.
salt :: ByteString -> Either CryptoError Salt
salt bs = if ByteString.length bs >= recommended
             then Right (Salt bs)
             else Left InvalidSaltLength

--------------------------------------------------------------------------------
-- | Convert a 'ByteString' into 'SharedSalt'.
sharedSalt :: ByteString -> Either CryptoError SharedSalt
sharedSalt = fmap SharedSalt . salt

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
