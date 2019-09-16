{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    git://code.devalot.com/sthenauth.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Encrypting and decrypting with a symmetric cipher (AES256).

-}
module Sthenauth.Crypto.Symmetric
  ( Secret
  , encrypt
  , encrypt'
  , decrypt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.AES (AES256)
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Error (eitherCryptoError)
import Crypto.Random (MonadRandom(..))
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LBS
import Data.Profunctor.Product.Default (Default(def))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , SqlBytea
  , fieldQueryRunnerColumn
  , toFields
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Error (CryptoError, wrappedCryptoError)
import Sthenauth.Crypto.Internal.IV (IV(..))
import qualified Sthenauth.Crypto.Internal.IV as IV
import Sthenauth.Crypto.Internal.Key (Key(..))

--------------------------------------------------------------------------------
newtype Secret a = Secret { getSecret :: ByteString }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromField (Secret a) where
  fromField f b = Secret <$> fromField f b

instance QueryRunnerColumnDefault SqlBytea (Secret a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (Secret a) (Column SqlBytea) where
  def = Constant (toFields . getSecret)

--------------------------------------------------------------------------------
-- | Encrypt a secret.
--
-- This version generates a unique initialization vector which is
-- stored with the encrypted secret.
encrypt
  :: ( MonadRandom m
     , Binary a
     )
  => Key AES256
  -- ^ The encryption key.

  -> a
  -- ^ The text to encrypt.

  -> m (Either CryptoError (Secret a))
  -- ^ If successful, the encrypted secret.
encrypt k s = encrypt' <$> IV.generate <*> pure k <*> pure s

--------------------------------------------------------------------------------
-- | Encrypt a secret given a pre-generated IV.
encrypt'
  :: ( Binary a
     )
  => IV AES256
  -- ^ The initialization vector to use.  Should be unique.

  -> Key AES256
  -- ^ The encryption key.

  -> a
  -- ^ The text to encrypt.

  -> Either CryptoError (Secret a)
  -- ^ If successful, the encrypted secret.
encrypt' iv (Key key) x = do
  cIV     <- IV.toCryptonite iv
  context <- first wrappedCryptoError $ eitherCryptoError $ Cryptonite.cipherInit key

  let bs = Cryptonite.ctrCombine context cIV (LBS.toStrict $ Binary.encode x)
  pure $ Secret (getIV iv <> bs) -- Store IV in the secret.

--------------------------------------------------------------------------------
-- | Decrypt text that was previously encrypted.
decrypt
  :: ( Binary a
     )
  => Key AES256
  -- ^ The encryption key used to encrypt the text.

  -> Secret a
  -- ^ The previously encrypted secret.

  -> Either CryptoError a
  -- ^ If successful, the decrypted text.
decrypt (Key key) (Secret bs) = do
  let size = Cryptonite.blockSize (undefined :: AES256)
      iv = IV (ByteString.take size bs) :: IV AES256
      secret = ByteString.drop size bs

  cIV <- IV.toCryptonite iv
  context <- first wrappedCryptoError $ eitherCryptoError $ Cryptonite.cipherInit key

  let bin = Cryptonite.ctrCombine context cIV secret
      x   = Binary.decode (LBS.fromStrict bin)

  pure x
