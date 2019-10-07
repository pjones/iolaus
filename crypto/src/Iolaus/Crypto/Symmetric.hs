{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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

Encrypting and decrypting with a symmetric cipher (AES256).

-}
module Iolaus.Crypto.Symmetric
  ( Secret
  , encrypt
  , encrypt'
  , decrypt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (when)
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Error (eitherCryptoError)
import Crypto.Random (MonadRandom(..))
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor.Product.Default (Default(def))
import Data.Text (Text)

import Database.PostgreSQL.Simple.FromField
  ( FromField(..)
  , Conversion
  )

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , SqlJsonb
  , fieldQueryRunnerColumn
  , sqlValueJSONB
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error (CryptoError, wrappedCryptoError)
import Iolaus.Crypto.Internal.IV (IV(..))
import qualified Iolaus.Crypto.Internal.IV as IV
import Iolaus.Crypto.Internal.Key (Key(..))

--------------------------------------------------------------------------------
newtype Secret c a = Secret { getSecret :: Text }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance (Cryptonite.Cipher c) => ToJSON (Secret c a) where
  toJSON s =
    Aeson.object [ "data"   .= getSecret s
                 , "cipher" .= Cryptonite.cipherName (undefined :: c)
                 ]

instance (Cryptonite.Cipher c) => FromJSON (Secret c a) where
  parseJSON (Aeson.Object v) = do
    let c' = Cryptonite.cipherName (undefined :: c)
    c <- v .: "cipher"

    when (c /= c') $
      fail ("cipher mismatch, expected " <> c' <> " but JSON contains " <> c)

    Secret <$> v .: "data"
  parseJSON invalid = Aeson.typeMismatch "Secret" invalid

--------------------------------------------------------------------------------
instance (Cryptonite.Cipher c) => FromField (Secret c a) where
  fromField f b = go =<< fromField f b
    where
      go :: Aeson.Value -> Conversion (Secret c a)
      go v = case Aeson.fromJSON v of
               Aeson.Success x -> pure x
               Aeson.Error e   -> fail e

instance (Cryptonite.Cipher c) => QueryRunnerColumnDefault SqlJsonb (Secret c a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance (Cryptonite.Cipher c) => Default Constant (Secret c a) (Column SqlJsonb) where
  def = Constant sqlValueJSONB

--------------------------------------------------------------------------------
-- | Encrypt a secret.
--
-- This version generates a unique initialization vector which is
-- stored with the encrypted secret.
encrypt
  :: forall a c m.
    ( MonadRandom m
    , Binary a
    , Cryptonite.BlockCipher c
     )
  => Key c
  -- ^ The encryption key.

  -> a
  -- ^ The text to encrypt.

  -> m (Either CryptoError (Secret c a))
  -- ^ If successful, the encrypted secret.
encrypt k s = encrypt' <$> IV.generate <*> pure k <*> pure s

--------------------------------------------------------------------------------
-- | Encrypt a secret given a pre-generated IV.
encrypt'
  :: forall a c.
     ( Binary a
     , Cryptonite.BlockCipher c
     )
  => IV c
  -- ^ The initialization vector to use.  Should be unique.

  -> Key c
  -- ^ The encryption key.

  -> a
  -- ^ The text to encrypt.

  -> Either CryptoError (Secret c a)
  -- ^ If successful, the encrypted secret.
encrypt' iv (Key key) x = do
  cIV     <- IV.toCryptonite iv
  context <- first wrappedCryptoError $ eitherCryptoError $ Cryptonite.cipherInit key

  let bs = Cryptonite.ctrCombine context cIV (LBS.toStrict $ Binary.encode x)
      ts = Encoding.encode $ Encoding (getIV iv <> bs) -- Store IV in the secret.
  pure $ Secret ts

--------------------------------------------------------------------------------
-- | Decrypt text that was previously encrypted.
decrypt
  :: forall a c.
     ( Binary a
     , Cryptonite.BlockCipher c
     )
  => Key c
  -- ^ The encryption key used to encrypt the text.

  -> Secret c a
  -- ^ The previously encrypted secret.

  -> Either CryptoError a
  -- ^ If successful, the decrypted text.
decrypt (Key key) (Secret ts) = do
  let bs = getBytes (Encoding.decode ts)
      size = Cryptonite.blockSize (undefined :: c)
      iv = IV (ByteString.take size bs) :: IV c
      secret = ByteString.drop size bs

  cIV <- IV.toCryptonite iv
  context <- first wrappedCryptoError $ eitherCryptoError $ Cryptonite.cipherInit key

  let bin = Cryptonite.ctrCombine context cIV secret
      x   = Binary.decode (LBS.fromStrict bin)

  pure x
