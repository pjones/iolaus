{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

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
  , SharedSalt(..)
  , getSalt
  , salt
  , sharedSalt
  , recommended
  , generate
  , generate'
  , encode
  , pack
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Random (MonadRandom(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import Iolaus.Opaleye.Newtype (makeNewtypeInstances)
import Opaleye.SqlTypes (SqlBytea)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error (CryptoError(..))

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
-- | Convert from a text-encoded 'Salt'.  This expects the text to
-- have come from the 'encode' function.
salt :: Text -> Either CryptoError Salt
salt = pack . getBytes . Encoding.decode

--------------------------------------------------------------------------------
-- | Convert from a text-encoded 'Salt'.  This expects the text to
-- have come from the 'encode' function.
sharedSalt :: Text -> Either CryptoError SharedSalt
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

--------------------------------------------------------------------------------
-- | Encode salt for writing to a safe location.
encode :: Salt -> Text
encode = Encoding.encode . Encoding . getSalt

--------------------------------------------------------------------------------
-- | Use an existing 'ByteString' as 'Salt'.  The 'ByteString' must
-- contain as least as many bytes as recommended.
pack :: ByteString -> Either CryptoError Salt
pack bs =
  if ByteString.length bs >= recommended
  then Right (Salt bs)
  else Left InvalidSaltLength
