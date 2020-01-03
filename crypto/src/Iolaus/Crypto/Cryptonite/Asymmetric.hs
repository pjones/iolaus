{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
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
module Iolaus.Crypto.Cryptonite.Asymmetric
  ( AsymmetricKey
  , toKey
  , fromKey
  , toX509PrivKey
  , fromX509PrivKey
  , generateKeyPair
  , toPublicKey
  , encrypt
  , decrypt
  , sign
  , verify
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except
import qualified Crypto.Hash.Algorithms as C
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.Types as RSA (private_size)
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Random (MonadRandom(..))
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.X509 as X509
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Error
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
class AsyAlgo a where
  -- | Size of keys, in bytes.
  keySizeBytes :: a -> Int

  -- | Maximum number of bytes that can be encrypted at once.
  chunkSize :: a -> Int
  chunkSize a = keySizeBytes a - 11

instance AsyAlgo Algo where
  keySizeBytes = \case
    RSA2048 -> 256
    RSA4096 -> 512

--------------------------------------------------------------------------------
-- | Internal type for representing an RSA public/private key pair.
data RSAKeyPair = RSAKeyPair
  { rsaSize :: Int
  , rsaN    :: Integer
  , rsaE    :: Integer
  , rsaD    :: Integer
  , rsaP    :: Integer
  , rsaQ    :: Integer
  , rsaDP   :: Integer
  , rsaDQ   :: Integer
  , rsaQI   :: Integer
  } deriving (Generic, Binary)

--------------------------------------------------------------------------------
-- | Create an internal 'RSAKeyPair' type from an RSA private key.
fromRSAPrivateKey :: RSA.PrivateKey -> RSAKeyPair
fromRSAPrivateKey rsa =
  RSAKeyPair { rsaSize = RSA.public_size (RSA.private_pub rsa)
             , rsaN    = RSA.public_n (RSA.private_pub rsa)
             , rsaE    = RSA.public_e (RSA.private_pub rsa)
             , rsaD    = RSA.private_d rsa
             , rsaP    = RSA.private_p rsa
             , rsaQ    = RSA.private_q rsa
             , rsaDP   = RSA.private_dP rsa
             , rsaDQ   = RSA.private_dQ rsa
             , rsaQI   = RSA.private_qinv rsa
             }

--------------------------------------------------------------------------------
-- | Extract an RSA public key from the internal 'RSAKeyPair' type.
toRSAPublicKey :: RSAKeyPair -> RSA.PublicKey
toRSAPublicKey RSAKeyPair{..} =
  RSA.PublicKey { RSA.public_size = rsaSize
                , RSA.public_n    = rsaN
                , RSA.public_e    = rsaE
                }

--------------------------------------------------------------------------------
toRSAPrivateKey :: RSAKeyPair -> RSA.PrivateKey
toRSAPrivateKey key@RSAKeyPair{..} =
  RSA.PrivateKey { RSA.private_pub  = toRSAPublicKey key
                 , RSA.private_d    = rsaD
                 , RSA.private_p    = rsaP
                 , RSA.private_q    = rsaQ
                 , RSA.private_dP   = rsaDP
                 , RSA.private_dQ   = rsaDQ
                 , RSA.private_qinv = rsaQI
                 }

--------------------------------------------------------------------------------
-- | Internal key type.
data AsymmetricKey
  = AKRSA Algo RSAKeyPair
  deriving (Generic, Binary)

instance AsyAlgo AsymmetricKey where
  keySizeBytes = \case
    AKRSA algo _ -> keySizeBytes algo

--------------------------------------------------------------------------------
-- | Recreate a key from a 'ByteString'.
toKey :: Label -> ByteString -> Either CryptoError AsymmetricKey
toKey = decodeBinaryKey

--------------------------------------------------------------------------------
-- | Serialize a key to a 'ByteString'.
fromKey :: AsymmetricKey -> ByteString
fromKey = LBS.toStrict . Binary.encode

--------------------------------------------------------------------------------
toX509PrivKey :: AsymmetricKey -> X509.PrivKey
toX509PrivKey = \case
  AKRSA _ k -> X509.PrivKeyRSA (toRSAPrivateKey k)

--------------------------------------------------------------------------------
fromX509PrivKey :: X509.PrivKey -> Maybe AsymmetricKey
fromX509PrivKey = \case
  X509.PrivKeyRSA k
    | RSA.private_size k == 256  -> Just (AKRSA RSA2048 (fromRSAPrivateKey k))
    | RSA.private_size k == 512  -> Just (AKRSA RSA4096 (fromRSAPrivateKey k))
    | otherwise -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- | Asymmetric key generation.
generateKeyPair :: (MonadRandom m) => Algo -> m AsymmetricKey
generateKeyPair algo =
  case algo of
    RSA2048 -> genRSA
    RSA4096 -> genRSA

  where
    genRSA =
      AKRSA algo . fromRSAPrivateKey . snd <$>
      RSA.generate (keySizeBytes algo) 65537

--------------------------------------------------------------------------------
toPublicKey :: AsymmetricKey -> PublicKey
toPublicKey = \case
  AKRSA algo rsa -> RSAPubKey (algo, toRSAPublicKey rsa)

--------------------------------------------------------------------------------
-- | Asymmetric encryption.
encrypt
  :: forall m e.
     ( MonadRandom m
     , MonadError e m
     , AsCryptoError e
     )
  => Label
  -> PublicKey
  -> ByteString
  -> m (Secret ByteString)
encrypt label = \case
  RSAPubKey (algo, rsa) -> encryptRSA algo rsa

  where
    encryptRSA :: Algo -> RSA.PublicKey -> ByteString -> m (Secret ByteString)
    encryptRSA algo pub val = do
      bs <- chunkM (chunkSize algo) val (chunkRSA pub) mempty
      return (Secret (LBS.toStrict (Builder.toLazyByteString bs)) Nothing label)

    chunkRSA :: RSA.PublicKey -> Builder -> ByteString -> m Builder
    chunkRSA key enc bs = do
      enc' <- liftRSAError =<< RSA.encrypt key bs
      return (enc <> Builder.byteString enc')

--------------------------------------------------------------------------------
decrypt
  :: forall m e.
     ( MonadRandom m
     , MonadError e m
     , AsCryptoError e
     )
  => AsymmetricKey
  -> Secret ByteString
  -> m ByteString
decrypt = \case
  AKRSA algo rsa -> decryptRSA algo rsa

  where
    decryptRSA :: Algo -> RSAKeyPair -> Secret ByteString -> m ByteString
    decryptRSA algo key Secret{secretBytes} = do
      let priv = toRSAPrivateKey key
      blinder <- RSA.generateBlinder (rsaN key)
      bs <- chunkM (keySizeBytes algo) secretBytes (chunkRSA priv blinder) mempty
      return (LBS.toStrict (Builder.toLazyByteString bs))

    chunkRSA :: RSA.PrivateKey
             -> RSA.Blinder
             -> Builder
             -> ByteString
             -> m Builder
    chunkRSA key blinder dec bs = do
      dec' <- liftRSAError (RSA.decrypt (Just blinder) key bs)
      return (dec <> Builder.byteString dec')

--------------------------------------------------------------------------------
-- | Sign a 'ByteString'.
sign
  :: forall m e.
     ( MonadRandom m
     , MonadError e m
     , AsCryptoError e
     )
  => AsymmetricKey
  -> Hash
  -> ByteString
  -> m (Signature ByteString)
sign = \case
  AKRSA algo rsa -> signRSA algo rsa

  where
    signRSA :: Algo -> RSAKeyPair -> Hash -> ByteString -> m (Signature ByteString)
    signRSA algo key hash val = do
      binder <- RSA.generateBlinder (rsaN key)
      let priv = toRSAPrivateKey key
          go h = liftRSAError (RSA.sign (Just binder) (Just h) priv val)
      sig <- withHashAlgo hash go
      return (Signature sig algo hash)

--------------------------------------------------------------------------------
verify
  :: forall m. (MonadRandom m)
  => PublicKey
  -> Signature ByteString
  -> ByteString
  -> m SigStatus
verify = \case
  RSAPubKey (_, rsa) -> verifyRSA rsa

  where
    verifyRSA :: RSA.PublicKey -> Signature ByteString -> ByteString -> m SigStatus
    verifyRSA key (Signature sig _ hash) val = do
      let go h = RSA.verify (Just h) key val sig
      if withHashAlgo hash go
        then return SignatureVerified
        else return SignatureMismatch


--------------------------------------------------------------------------------
-- | Feed chunks of a 'ByteString' into a monadic function.
chunkM :: forall m a. (Monad m) => Int -> ByteString -> (a -> ByteString -> m a) -> a -> m a
chunkM n start f = go start
  where
    go :: ByteString -> a -> m a
    go bs acc
      | ByteString.null bs = return acc
      | otherwise = do
          let (chunk, remaining) = ByteString.splitAt n bs
          acc' <- f acc chunk
          go remaining acc'

--------------------------------------------------------------------------------
liftRSAError :: (MonadError e m, AsCryptoError e) => Either RSA.Error a -> m a
liftRSAError (Right a) = return a
liftRSAError (Left  e) =
  case e of
    RSA.MessageSizeIncorrect -> throwing _InvalidKeyLength ()
    RSA.MessageTooLong       -> throwing _InvalidKeyLength ()
    RSA.MessageNotRecognized -> throwing _MalformedCipherTextError ()
    RSA.SignatureTooLong     -> throwing _MalformedSignatureTextError ()
    RSA.InvalidParameters    -> throwing _InvalidKeyLength ()

--------------------------------------------------------------------------------
withHashAlgo :: Hash -> (forall h. (RSA.HashAlgorithmASN1 h) => h -> r) -> r
withHashAlgo h f =
  case h of
    SHA2_256 -> f C.SHA256
    SHA2_384 -> f C.SHA384
    SHA2_512 -> f C.SHA512
