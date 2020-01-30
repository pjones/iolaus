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

The 'MonadCrypto' class and helper functions.

-}
module Control.Monad.Crypto.Class
  ( -- * MonadCrypto
    MonadCrypto
  , Key
  , KeyPair

    -- * Random Bytes
  , generateRandomBytes

    -- * Symmetric Primitives
  , generateKey
  , fetchKey
  , encrypt
  , encrypt'
  , decrypt
  , decrypt'

    -- * Asymmetric Primitives
  , generateKeyPair
  , fetchKeyPair
  , toPublicKey
  , asymmetricEncrypt
  , asymmetricEncrypt'
  , asymmetricDecrypt
  , asymmetricDecrypt'
  , asymmetricSign
  , asymmetricSign'
  , verifySignature
  , verifySignature'

    -- * Private Keys
  , MonadKeyAccess
  , encodeKey
  , decodeKey
  , encodePrivateKey
  , decodePrivateKey
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.Crypto.Internal (MonadCrypto(..), Key, KeyPair)
import qualified Control.Monad.Crypto.Internal as M
import Control.Monad.Crypto.KeyAccess (MonadKeyAccess(..))
import qualified Control.Monad.Crypto.KeyAccess as K
import Iolaus.Crypto.Error
import Iolaus.Crypto.Key
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
type LByteString = LByteString.ByteString

--------------------------------------------------------------------------------
-- | Generate random bytes for a cryptographic purpose.
generateRandomBytes :: MonadCrypto k m => Int -> m ByteString
generateRandomBytes = liftCryptoOpt . M.generateRandomBytes

--------------------------------------------------------------------------------
-- | Generate a new symmetric key given a 'Cipher' and 'Label'.
generateKey :: MonadCrypto k m => Cipher -> Label -> m (Key k)
generateKey = (liftCryptoOpt .) . M.generateKey

--------------------------------------------------------------------------------
-- | Fetch an existing symmetric key.
fetchKey :: MonadCrypto k m => Label -> m (Maybe (Key k))
fetchKey = liftCryptoOpt . M.fetchKey

--------------------------------------------------------------------------------
-- | Encrypt a 'ByteString' using the given symmetric key.
encrypt :: MonadCrypto k m => Key k -> ByteString -> m (Secret ByteString)
encrypt = (liftCryptoOpt .) . M.encrypt

--------------------------------------------------------------------------------
-- | Symmetric encryption of any type that can be converted to 'Binary'.
encrypt' :: (MonadCrypto k m, Binary a) => Key k -> a -> m (Secret a)
encrypt' k  = fmap reSec . encrypt k . toB

--------------------------------------------------------------------------------
-- | Decrypt a 'ByteString' using the given symmetric key.
decrypt :: MonadCrypto k m => Key k -> Secret ByteString -> m ByteString
decrypt = (liftCryptoOpt .) . M.decrypt

--------------------------------------------------------------------------------
-- | Symmetric decryption of any type that can be converted from 'Binary'.
--
-- If the value you are decrypting fails to deserialize from 'Binary'
-- an error will be thrown.  This usually indicates that you used the
-- wrong encryption key but could also indicate that the 'Secret' data
-- is corrupt.
decrypt'
  :: ( MonadCrypto k m
     , MonadError  e m
     , AsCryptoError e
     , Binary a
     )
  => Key k
  -> Secret a
  -> m a
decrypt' k = fromB <=< decrypt k . reSec

--------------------------------------------------------------------------------
-- | Generate a private/public asymmetric key.
generateKeyPair :: MonadCrypto k m => Algo -> Label -> m (KeyPair k)
generateKeyPair = (liftCryptoOpt .) . M.generateKeyPair

--------------------------------------------------------------------------------
-- | Fetch an existing asymmetric key pair.
fetchKeyPair :: MonadCrypto k m => Label -> m (Maybe (KeyPair k))
fetchKeyPair = liftCryptoOpt . M.fetchKeyPair

--------------------------------------------------------------------------------
-- | Extract a public key from the given private key.
toPublicKey :: MonadCrypto k m => KeyPair k -> m PublicKey
toPublicKey = liftCryptoOpt . M.toPublicKey

--------------------------------------------------------------------------------
-- | Encrypt the given 'ByteString' using a public key.
asymmetricEncrypt :: MonadCrypto k m => PublicKey -> ByteString -> m (Secret ByteString)
asymmetricEncrypt = (liftCryptoOpt .) . M.asymmetricEncrypt

--------------------------------------------------------------------------------
-- | Asymmetric encryption of any type that can be converted to
-- 'Binary'.
asymmetricEncrypt' :: (MonadCrypto k m, Binary a) => PublicKey -> a -> m (Secret a)
asymmetricEncrypt' k = fmap reSec . asymmetricEncrypt k . toB

--------------------------------------------------------------------------------
-- | Decrypt a 'ByteString' using the given private key.
asymmetricDecrypt :: MonadCrypto k m => KeyPair k -> Secret ByteString -> m ByteString
asymmetricDecrypt = (liftCryptoOpt .) . M.asymmetricDecrypt

--------------------------------------------------------------------------------
-- | Asymmetric decryption of any type that can be converted from
-- 'Binary'.  Decryption is done using the private key component of
-- the 'KeyPair'.
--
-- If the value you are decrypting fails to deserialize from 'Binary'
-- an error will be thrown.  This usually indicates that you used the
-- wrong encryption key but could also indicate that the 'Secret' data
-- is corrupt.
asymmetricDecrypt'
  :: ( MonadCrypto k m
     , MonadError  e m
     , AsCryptoError e
     , Binary a
     )
  => KeyPair k -> Secret a -> m a
asymmetricDecrypt' k = fromB <=< asymmetricDecrypt k  . reSec

--------------------------------------------------------------------------------
-- | Create a cryptograpic signature of the given 'ByteString'.
asymmetricSign :: MonadCrypto k m
  => KeyPair k -> Hash -> ByteString -> m (Signature ByteString)
asymmetricSign k = (liftCryptoOpt .) . M.asymmetricSign k

--------------------------------------------------------------------------------
-- | The value to be signed is converted to 'Binary', hashed using a
-- hashing algorithm, and then the hashed value is encrypted with the
-- private key component of the 'KeyPair'.
asymmetricSign'
  :: ( MonadCrypto k m
     , Binary a
     )
  => KeyPair k -> Hash -> a -> m (Signature a)
asymmetricSign' k h = fmap reSig . asymmetricSign k h . toB

--------------------------------------------------------------------------------
-- | Verify a previously generated signature for the given 'ByteString'.
verifySignature :: MonadCrypto k m => PublicKey -> Signature ByteString -> ByteString -> m SigStatus
verifySignature k = (liftCryptoOpt .) . M.verifySignature k

--------------------------------------------------------------------------------
-- | Verify a signature.
verifySignature'
  :: ( MonadCrypto k m
     , Binary a
     )
  => PublicKey
  -> Signature a
  -> a
  -> m SigStatus
verifySignature' k s = verifySignature k (reSig s) . toB

--------------------------------------------------------------------------------
-- | Expose a symmetric key as binary data.
encodeKey :: MonadKeyAccess k m => Key k -> m LByteString
encodeKey = liftKeyAccessOpt . K.encodeKey

--------------------------------------------------------------------------------
-- | Attempt to decode a symmetric key from binary data.
decodeKey :: MonadKeyAccess k m => LByteString -> m (Maybe (Key k))
decodeKey = liftKeyAccessOpt . K.decodeKey

--------------------------------------------------------------------------------
-- | Expose the private key as PEM-encoded data.
encodePrivateKey :: MonadKeyAccess k m => KeyPair k -> m LByteString
encodePrivateKey = liftKeyAccessOpt . K.encodePrivateKey

--------------------------------------------------------------------------------
-- | Attempt to decode a PEM-encoded private key.
decodePrivateKey :: MonadKeyAccess k m => Label -> LByteString -> m (Maybe (KeyPair k))
decodePrivateKey = (liftKeyAccessOpt .) . K.decodePrivateKey

--------------------------------------------------------------------------------
-- Utility functions:
toB :: (Binary a) => a -> ByteString
toB = LByteString.toStrict . Binary.encode

fromB :: (Binary a, MonadError e m, AsCryptoError e) => ByteString -> m a
fromB bs =
  case Binary.decodeOrFail (LByteString.fromStrict bs) of
    Left _ -> throwing _MalformedCipherTextError ()
    Right (bs', _, k)
      | LByteString.null bs' -> return k
      | otherwise -> throwing _MalformedCipherTextError ()

reSec :: Secret a -> Secret b
reSec Secret{..} = Secret{..}

reSig :: Signature a -> Signature b
reSig Signature{..} = Signature{..}
