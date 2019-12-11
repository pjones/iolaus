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

Encrypting and decrypting with a symmetric cipher.

-}
module Iolaus.Crypto.Cryptonite.Symmetric
  ( SymmetricKey(..)
  , generateKey
  , toKey
  , encrypt
  , encrypt'
  , decrypt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.Types (Cipher(..), BlockCipher(..), KeySizeSpecifier(..))
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Error (eitherCryptoError)
import qualified Crypto.Error as CE
import Crypto.Random (MonadRandom(..))
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Error
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Key (Label)

--------------------------------------------------------------------------------
-- | An initialization vector for cipher @c@.
newtype IV c = IV { getIV :: ByteString }

--------------------------------------------------------------------------------
-- | Generate an initialization vector.
generateIV :: forall m c. (MonadRandom m, BlockCipher c) => m (IV c)
generateIV = IV <$> getRandomBytes (blockSize (undefined :: c))

--------------------------------------------------------------------------------
-- | Convert an IV to what Cryptonite expects.
toCryptoniteIV :: (BlockCipher c) => IV c -> Either CryptoError (Cryptonite.IV c)
toCryptoniteIV (IV bs) =
  case Cryptonite.makeIV bs of
    Just x  -> Right x
    Nothing -> Left (wrappedCryptoError CE.CryptoError_IvSizeInvalid)

--------------------------------------------------------------------------------
newtype SymmetricKey c = SymmetricKey ByteString

--------------------------------------------------------------------------------
-- | Discover the appropriate key size for a symmetric cipher.
--
-- >>> keySize (Proxy :: Proxy AES256)
-- 32
keySize :: forall c. (Cipher c) => Proxy c -> Int
keySize _ =
  case cipherKeySize (undefined :: c) of
    KeySizeRange _ n -> n
    KeySizeFixed n   -> n
    KeySizeEnum ns   -> foldr max 32 ns

--------------------------------------------------------------------------------
-- | Generate a key that is appropriate for the given cipher.
generateKey :: forall m c. (MonadRandom m, Cipher c) => m (SymmetricKey c)
generateKey = SymmetricKey <$> getRandomBytes (keySize (Proxy :: Proxy c))

--------------------------------------------------------------------------------
-- | Attempt to convert a key to one that will work for a specific cipher.
--
-- This is necessary because when a key is read from disk or the
-- network it won't be tied to any particular cipher.
toKey
  :: forall c. (Cipher c)
  => ByteString
  -> Either CryptoError (SymmetricKey c)
toKey bs =
  if ByteString.length bs == keySize (Proxy :: Proxy c)
     then Right (SymmetricKey bs)
     else Left InvalidKeyLength

--------------------------------------------------------------------------------
-- | Encrypt a secret.
--
-- This version generates a unique initialization vector which is
-- stored with the encrypted secret.
encrypt
  :: forall a c m.
    ( MonadRandom m
    , BlockCipher c
    , Binary a
    )
  => Label
  -- ^ The label of the key that is being used.

  -> SymmetricKey c
  -- ^ The encryption key.

  -> a
  -- ^ The value to encrypt.

  -> m (Either CryptoError (Secret a))
  -- ^ If successful, the encrypted secret.
encrypt l k s = encrypt' <$> generateIV <*> pure l <*> pure k <*> pure s

--------------------------------------------------------------------------------
-- | Encrypt a secret given a pre-generated IV.
encrypt'
  :: forall a c.
     ( Binary a
     , BlockCipher c
     )
  => IV c
  -- ^ The initialization vector to use.  Should be unique.

  -> Label
  -- ^ The label of the key that is being used.

  -> SymmetricKey c
  -- ^ The encryption key.

  -> a
  -- ^ The value to encrypt.

  -> Either CryptoError (Secret a)
  -- ^ If successful, the encrypted secret.
encrypt' iv label (SymmetricKey key) x = do
  cIV     <- toCryptoniteIV iv
  context <- first wrappedCryptoError $ eitherCryptoError $ cipherInit key
  let bs = ctrCombine context cIV (LBS.toStrict $ Binary.encode x)
  return (Secret (getIV iv <> bs) label)

--------------------------------------------------------------------------------
-- | Decrypt a value that was previously encrypted.
decrypt
  :: forall a c.
     ( Binary a
     , BlockCipher c
     )
  => SymmetricKey c
  -- ^ The encryption key used to encrypt the value.

  -> Secret a
  -- ^ The previously encrypted secret.

  -> Either CryptoError a
  -- ^ If successful, the decrypted value.
decrypt (SymmetricKey key) (Secret bs _) = do
  let size = blockSize (undefined :: c)
      iv = IV (ByteString.take size bs) :: IV c
      bytes = ByteString.drop size bs

  cIV <- toCryptoniteIV iv
  context <- first wrappedCryptoError $ eitherCryptoError $ cipherInit key

  let bin = ctrCombine context cIV bytes
      x   = Binary.decode (LBS.fromStrict bin)

  return x
