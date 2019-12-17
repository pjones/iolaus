{-# LANGUAGE RecordWildCards     #-}
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

-}
module Iolaus.Crypto.API
  ( generateRandom
  , generateKey
  , fetchKey
  , encrypt
  , decrypt
  , generateKeyPair
  , fetchKeyPair
  , toPublicKey
  , asymmetricEncrypt
  , asymmetricDecrypt
  , asymmetricSign
  , verifySignature
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Monad (MonadCrypto(..))
import qualified Iolaus.Crypto.Monad as M
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
-- | Generate random bytes.
generateRandom :: (MonadCrypto m) => Int -> m ByteString
generateRandom = liftCryptoOpt . M.generateRandom

--------------------------------------------------------------------------------
-- | Generate a new symmetric cryptography key which will be
-- identified by the given 'Label'.
generateKey :: (MonadCrypto m) => Cipher -> Label -> m (Key m)
generateKey = (liftCryptoOpt .) . M.generateKey

--------------------------------------------------------------------------------
-- | Locate a previously generated symmetric cryptography key given
-- its 'Label'.
fetchKey :: (MonadCrypto m) => Cipher -> Label -> m (Maybe (Key m))
fetchKey = (liftCryptoOpt .) . M.fetchKey

--------------------------------------------------------------------------------
-- | Symmetric encryption of any type that can be converted to 'Binary'.
encrypt :: forall m a. (MonadCrypto m, Binary a) => Key m -> a -> m (Secret a)
encrypt k x = liftCryptoOpt (reSec <$> M.encrypt k (toB x))

--------------------------------------------------------------------------------
-- | Symmetric decryption of any type that can be converted from 'Binary'.
decrypt :: (MonadCrypto m, Binary a) => Key m -> Secret a -> m a
decrypt k s = liftCryptoOpt (fromB <$> M.decrypt k (reSec s))

--------------------------------------------------------------------------------
-- | Generate a new asymmetric key pair (private key and public key)
-- which will be identified by the given 'Label'.
generateKeyPair :: (MonadCrypto m) => Algo -> Label -> m (KeyPair m)
generateKeyPair = (liftCryptoOpt .) . M.generateKeyPair

--------------------------------------------------------------------------------
-- | Locate an existing key pair given its 'Label'.
fetchKeyPair :: (MonadCrypto m) => Algo -> Label -> m (Maybe (KeyPair m))
fetchKeyPair = (liftCryptoOpt .) . M.fetchKeyPair

--------------------------------------------------------------------------------
-- | Extract the public key from a 'KeyPair'.
toPublicKey :: (MonadCrypto m) => KeyPair m -> m PublicKey
toPublicKey = liftCryptoOpt . M.toPublicKey

--------------------------------------------------------------------------------
-- | Asymmetric encryption of any type that can be converted to
-- 'Binary'.
asymmetricEncrypt :: (MonadCrypto m, Binary a) => Label -> PublicKey -> a -> m (Secret a)
asymmetricEncrypt l k x = liftCryptoOpt (reSec <$> M.asymmetricEncrypt l k (toB x))

--------------------------------------------------------------------------------
-- | Asymmetric decryption of any type that can be converted from
-- 'Binary'.  Decryption is done using the private key component of
-- the 'KeyPair'.
asymmetricDecrypt :: (MonadCrypto m, Binary a) => KeyPair m -> Secret a -> m a
asymmetricDecrypt k s = liftCryptoOpt (fromB <$> M.asymmetricDecrypt k (reSec s))

--------------------------------------------------------------------------------
-- | The value to be signed is converted to 'Binary', hashed using a
-- hashing algorithm, and then the hashed value is encrypted with the
-- private key component of the 'KeyPair'.
asymmetricSign :: (MonadCrypto m, Binary a) => KeyPair m -> Hash -> a -> m (Signature a)
asymmetricSign k h x = liftCryptoOpt (reSig <$> M.asymmetricSign k h (toB x))

--------------------------------------------------------------------------------
-- | Verify a signature.
verifySignature :: (MonadCrypto m, Binary a) => PublicKey -> Signature a -> a -> m SigStatus
verifySignature k s x = liftCryptoOpt (M.verifySignature k (reSig s) (toB x))

--------------------------------------------------------------------------------
-- Utility functions:
toB :: (Binary a) => a -> ByteString
toB = LByteString.toStrict . Binary.encode

fromB :: (Binary a) => ByteString -> a
fromB = Binary.decode . LByteString.fromStrict

reSec :: Secret a -> Secret b
reSec Secret{..} = Secret{..}

reSig :: Signature a -> Signature b
reSig Signature{..} = Signature{..}
