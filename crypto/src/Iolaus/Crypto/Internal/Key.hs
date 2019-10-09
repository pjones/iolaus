{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

Encryption keys.

-}
module Iolaus.Crypto.Internal.Key
  ( Key(..)
  , Unchecked
  , convert
  , generate
  , generate'
  , encode
  , pack
  , packBS
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.Types (Cipher(cipherKeySize), KeySizeSpecifier(..))
import Crypto.Random (MonadRandom(..))
import Data.Aeson (FromJSON(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.String (IsString(..))
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error (CryptoError(..))
import qualified Iolaus.Crypto.Cipher as Cipher

--------------------------------------------------------------------------------
-- | Type alias to remind you that a key can't be used for encryption.
type Unchecked = ()

--------------------------------------------------------------------------------
-- | An encryption key that is the proper length for a specific cipher.
newtype Key a = Key { getKey :: ByteString }

instance IsString (Key Unchecked) where
  fromString = Key . fromString

--------------------------------------------------------------------------------
instance FromJSON (Key Unchecked) where
  parseJSON = fmap (Key . Encoding.getBytes) . parseJSON

--------------------------------------------------------------------------------
-- | Discover the appropriate key size for a symmetric cipher.
--
-- >>> keySize (undefined :: AES256)
-- 32
keySize :: (Cipher c) => c -> Int
keySize c =
  case cipherKeySize c of
    KeySizeRange _ n -> n
    KeySizeFixed n   -> n
    KeySizeEnum ns   -> foldr max 32 ns

--------------------------------------------------------------------------------
-- | Attempt to convert a 'Key' to one that will work for a specific cipher.
--
-- This is necessary because when a key is read from disk or the
-- network it won't be tied to any particular cipher.  Instead it will
-- be of type @Key Unchecked@.
convert :: forall c. (Cipher c) => Key Unchecked -> Either CryptoError (Key c)
convert (Key bs) =
  if ByteString.length bs == keySize (undefined :: c)
     then Right (Key bs)
     else Left InvalidKeyLength

--------------------------------------------------------------------------------
-- | Generate a key that is appropriate for the default cipher.
generate :: (MonadRandom m) => m (Key Cipher.DefaultCipher)
generate = generate'

--------------------------------------------------------------------------------
-- | Generate a key that is appropriate for the given cipher.
generate' :: forall m c. (MonadRandom m, Cipher c) => m (Key c)
generate' = Key <$> getRandomBytes (keySize (undefined :: c))

--------------------------------------------------------------------------------
-- | Encode a key for writing to a safe location.
encode :: Key c -> Text
encode = Encoding.encode . Encoding . getKey

--------------------------------------------------------------------------------
-- | The inverse of 'encode'
pack :: Text -> Key Unchecked
pack = Key . getBytes . Encoding.decode

--------------------------------------------------------------------------------
-- | Convert a 'ByteString' into a 'Key'.  You probably want to use
-- 'pack' instead.
packBS:: ByteString -> Key Unchecked
packBS = Key
