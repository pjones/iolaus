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
import Iolaus.Crypto.Monad (MonadCrypto(..), Key, KeyPair)
import qualified Iolaus.Crypto.Monad as M
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
-- | Generate random bytes.
generateRandom :: (MonadCrypto k m) => Int -> m ByteString
generateRandom = liftCryptoOpt . M.generateRandom

--------------------------------------------------------------------------------
-- | Generate a new symmetric cryptography key which will be
-- identified by the given 'Label'.
generateKey :: (MonadCrypto k m) => Cipher -> Label -> m (Key k)
generateKey = (liftCryptoOpt .) . M.generateKey

--------------------------------------------------------------------------------
-- | Locate a previously generated symmetric cryptography key given
-- its 'Label'.
fetchKey :: (MonadCrypto k m) => Label -> m (Maybe (Key k))
fetchKey = liftCryptoOpt . M.fetchKey

--------------------------------------------------------------------------------
-- | Symmetric encryption of any type that can be converted to 'Binary'.
encrypt :: (MonadCrypto k m, Binary a) => Key k -> a -> m (Secret a)
encrypt k x = liftCryptoOpt (reSec <$> M.encrypt k (toB x))

--------------------------------------------------------------------------------
-- | Symmetric decryption of any type that can be converted from 'Binary'.
decrypt :: (MonadCrypto k m, Binary a) => Key k -> Secret a -> m a
decrypt k s = liftCryptoOpt (fromB <$> M.decrypt k (reSec s))

--------------------------------------------------------------------------------
-- | Generate a new asymmetric key pair (private key and public key)
-- which will be identified by the given 'Label'.
generateKeyPair :: (MonadCrypto k m) => Algo -> Label -> m (KeyPair k)
generateKeyPair = (liftCryptoOpt .) . M.generateKeyPair

--------------------------------------------------------------------------------
-- | Locate an existing key pair given its 'Label'.
fetchKeyPair :: (MonadCrypto k m) => Label -> m (Maybe (KeyPair k))
fetchKeyPair = liftCryptoOpt . M.fetchKeyPair

--------------------------------------------------------------------------------
-- | Extract the public key from a 'KeyPair'.
toPublicKey :: (MonadCrypto k m) => KeyPair k -> m PublicKey
toPublicKey = liftCryptoOpt . M.toPublicKey

--------------------------------------------------------------------------------
-- | Asymmetric encryption of any type that can be converted to
-- 'Binary'.
asymmetricEncrypt :: (MonadCrypto k m, Binary a) => Label -> PublicKey -> a -> m (Secret a)
asymmetricEncrypt l k x = liftCryptoOpt (reSec <$> M.asymmetricEncrypt l k (toB x))

--------------------------------------------------------------------------------
-- | Asymmetric decryption of any type that can be converted from
-- 'Binary'.  Decryption is done using the private key component of
-- the 'KeyPair'.
asymmetricDecrypt :: (MonadCrypto k m, Binary a) => KeyPair k -> Secret a -> m a
asymmetricDecrypt k s = liftCryptoOpt (fromB <$> M.asymmetricDecrypt k (reSec s))

--------------------------------------------------------------------------------
-- | The value to be signed is converted to 'Binary', hashed using a
-- hashing algorithm, and then the hashed value is encrypted with the
-- private key component of the 'KeyPair'.
asymmetricSign :: (MonadCrypto k m, Binary a) => KeyPair k -> Hash -> a -> m (Signature a)
asymmetricSign k h x = liftCryptoOpt (reSig <$> M.asymmetricSign k h (toB x))

--------------------------------------------------------------------------------
-- | Verify a signature.
verifySignature :: (MonadCrypto k m, Binary a) => PublicKey -> Signature a -> a -> m SigStatus
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
