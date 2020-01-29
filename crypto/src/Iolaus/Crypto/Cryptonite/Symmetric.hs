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
  , fromKey
  , encrypt
  , encrypt'
  , decrypt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Cipher.AES as C
import qualified Crypto.Cipher.Types as C
import qualified Crypto.Error as CE
import Crypto.Random (MonadRandom(..))
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteArray as Mem
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import GHC.Generics (Generic)

import Crypto.Cipher.Types
  ( BlockCipher
  , KeySizeSpecifier(..)
  , AEADMode(..)
  , AuthTag(..)
  , AEAD
  , aeadEncrypt
  , aeadDecrypt
  , aeadFinalize
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Error
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Key

--------------------------------------------------------------------------------
withBlockCipher :: forall r. Cipher -> (forall c'. (BlockCipher c') => Proxy c' -> r) -> r
withBlockCipher c f =
  case c of
    AES128 -> f (Proxy :: Proxy C.AES128)
    AES192 -> f (Proxy :: Proxy C.AES192)
    AES256 -> f (Proxy :: Proxy C.AES256)

--------------------------------------------------------------------------------
-- | Discover the appropriate key size for a symmetric cipher.
keySize :: Cipher -> Int
keySize c = withBlockCipher c go
  where
    go  :: forall c'. (BlockCipher c') => Proxy c' -> Int
    go _ = case C.cipherKeySize (undefined :: c') of
      KeySizeRange _ n -> n
      KeySizeFixed n   -> n
      KeySizeEnum ns   -> foldr max 32 ns

--------------------------------------------------------------------------------
blockSize :: Cipher -> Int
blockSize c = withBlockCipher c go
  where
    go :: forall c'. (BlockCipher c') => Proxy c' -> Int
    go _ = C.blockSize (undefined :: c')

--------------------------------------------------------------------------------
-- | An initialization vector for cipher @c@.
newtype IV = IV { getIV :: ByteString }

--------------------------------------------------------------------------------
-- | Generate an initialization vector.
generateIV :: (MonadRandom m) => Cipher -> m IV
generateIV = fmap IV . getRandomBytes . blockSize

--------------------------------------------------------------------------------
-- | Convert an IV to what Cryptonite expects.
toCryptoniteIV :: (BlockCipher c) => IV -> Either CryptoError (C.IV c)
toCryptoniteIV (IV bs) =
  case C.makeIV bs of
    Just x  -> Right x
    Nothing -> Left (wrappedCryptoError CE.CryptoError_IvSizeInvalid)

--------------------------------------------------------------------------------
data SymmetricKey = SymmetricKey
  { keyc  :: Cipher
  , keybs :: ByteString
  } deriving (Generic, Binary)

--------------------------------------------------------------------------------
-- | Generate a key that is appropriate for the given cipher.
generateKey :: (MonadRandom m) => Cipher -> m SymmetricKey
generateKey c = SymmetricKey c <$> getRandomBytes (keySize c)

--------------------------------------------------------------------------------
-- | Recreate a key from a 'ByteString'.
toKey :: Label -> ByteString -> Either CryptoError SymmetricKey
toKey label bs = do
  key <- decodeBinaryKey label bs
  assert (ByteString.length (keybs key) == keySize (keyc key)) InvalidKeyLength
  return key

--------------------------------------------------------------------------------
-- | Serialize a key to a 'ByteString'.
fromKey :: SymmetricKey -> ByteString
fromKey = LBS.toStrict . Binary.encode

--------------------------------------------------------------------------------
-- | Encrypt a secret.
--
-- This version generates a unique initialization vector which is
-- stored with the encrypted secret.
encrypt
  :: (MonadRandom m)

  => Label
  -- ^ The label of the key that is being used.

  -> SymmetricKey
  -- ^ The encryption key.

  -> ByteString
  -- ^ The value to encrypt.

  -> m (Either CryptoError (Secret ByteString))
  -- ^ If successful, the encrypted secret.

encrypt l k s =
  encrypt' <$> generateIV (keyc k)
           <*> pure l
           <*> pure k
           <*> pure s

--------------------------------------------------------------------------------
-- | Encrypt a secret given a pre-generated IV.
encrypt'
  :: IV

  -- ^ The initialization vector to use.  Should be unique.

  -> Label
  -- ^ The label of the key that is being used.

  -> SymmetricKey
  -- ^ The encryption key.

  -> ByteString
  -- ^ The value to encrypt.

  -> Either CryptoError (Secret ByteString)
  -- ^ If successful, the encrypted secret.

encrypt' iv label key bs =
    withBlockCipher (keyc key) go
  where
    go :: forall c. (BlockCipher c) => Proxy c -> Either CryptoError (Secret ByteString)
    go _ = do
      aead <- symInit iv key :: Either CryptoError (AEAD c)
      let (output, final) = aeadEncrypt aead bs
          mac = unAuthTag $ aeadFinalize final (blockSize (keyc key))
      return (Secret (getIV iv <> output) (Just . Mem.convert $ mac) label)

--------------------------------------------------------------------------------
-- | Decrypt a value that was previously encrypted.
decrypt
  :: SymmetricKey
  -- ^ The encryption key used to encrypt the value.

  -> Secret ByteString
  -- ^ The previously encrypted secret.

  -> Either CryptoError ByteString
  -- ^ If successful, the decrypted value.

decrypt key (Secret bs mac _) =
    withBlockCipher (keyc key) go
  where
    go :: forall c. (BlockCipher c) => Proxy c -> Either CryptoError ByteString
    go _ = do
      let size = blockSize (keyc key)
          iv = IV (ByteString.take size bs)
          bytes = ByteString.drop size bs

      aead <- symInit iv key :: Either CryptoError (AEAD c)
      tag  <- checkMAC mac

      let (result, final) = aeadDecrypt aead bytes
          tag' = aeadFinalize final (Mem.length tag)

      if tag == tag'
        then return result
        else Left AuthTagMismatchError

    checkMAC :: Maybe ByteString -> Either CryptoError AuthTag
    checkMAC Nothing = Left MissingAuthTagError
    checkMAC (Just b) = Right (AuthTag (Mem.pack . Mem.unpack $ b))

--------------------------------------------------------------------------------
symInit
  :: forall c. (BlockCipher c)
  => IV -> SymmetricKey
  -> Either CryptoError (AEAD c)
symInit iv (SymmetricKey _ key) = do
  cIV <- toCryptoniteIV iv :: Either CryptoError (C.IV c)
  context <- fromFailable (C.cipherInit key) :: Either CryptoError c
  fromFailable (C.aeadInit AEAD_GCM context cIV)
