{-|

Copyright:
  This file is part of the package iolaus. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

Salt: random data mixed with a password to secure it while in rest
(i.e. stored in a database).

-}
module Iolaus.Crypto.Salt
  ( Salt
  , getSalt
  , SharedSalt(..)
  , recommendedSaltLen
  , generateSalt
  , generateSalt'
  , encodeSalt
  , decodeSalt
  , toSalt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import Iolaus.Database.Newtype (makeNewtypeInstances)
import Opaleye.SqlTypes (SqlBytea)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error (CryptoError(..))
import Control.Monad.Crypto.Class

--------------------------------------------------------------------------------
-- | A binary salt that should only be used for a single secret.
newtype Salt = Salt
  { getSalt :: ByteString -- ^ Access the raw bytes of a salt.
  } deriving Eq

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
newtype SharedSalt = SharedSalt { getSharedSalt :: Salt }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | The recommended salt length (as per RFC 8018 section 4).
recommendedSaltLen :: Int
recommendedSaltLen = 8 -- 64 bits.

--------------------------------------------------------------------------------
-- | Generate a salt with the recommended length.
generateSalt :: (MonadCrypto k m) => m Salt
generateSalt = generateSalt' recommendedSaltLen

--------------------------------------------------------------------------------
-- | Generate a salt with the given number of bytes (which may not be
-- less than the recommended value).
--
-- Attempting to generate fewer bytes than the recommended length will
-- automatically upgrade the length to the recommended value.
generateSalt' :: (MonadCrypto k m) => Int -> m Salt
generateSalt' = fmap Salt . generateRandomBytes . max recommendedSaltLen

--------------------------------------------------------------------------------
-- | Encode salt for writing to a safe location.
encodeSalt :: Salt -> Text
encodeSalt = Encoding.encode . Encoding . getSalt

--------------------------------------------------------------------------------
-- | The inverse of 'encodeSalt'.
decodeSalt :: Text -> Either CryptoError Salt
decodeSalt t = do
  e <- maybe (Left InvalidKeyLength) Right (Encoding.decode t)
  toSalt (getBytes e)

--------------------------------------------------------------------------------
-- | Convert an existing 'ByteString' to a salt.
toSalt :: ByteString -> Either CryptoError Salt
toSalt bs =
  if ByteString.length bs >= recommendedSaltLen
    then Right (Salt bs)
    else Left InvalidSaltLength
