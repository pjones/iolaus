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
  , Clear
  , Encrypted
  , makeSecret
  , readSecret
  , encrypt
  , encrypt'
  , decrypt
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.AES (AES256)
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Error (CryptoError, eitherCryptoError)
import Crypto.Random (MonadRandom(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Internal.IV (IV(..))
import qualified Sthenauth.Crypto.Internal.IV as IV
import Sthenauth.Crypto.Internal.Key (Key(..))

--------------------------------------------------------------------------------
-- | Phantom type to mark a secret as clear (decrypted).
data Clear

--------------------------------------------------------------------------------
-- | Phantom type to mark a secret as encrypted.
data Encrypted

--------------------------------------------------------------------------------
newtype Secret a = Secret { getSecret :: ByteString }

--------------------------------------------------------------------------------
-- | Create a clear secret from a 'Text'.
makeSecret :: Text -> Secret Clear
makeSecret = Secret . encodeUtf8

--------------------------------------------------------------------------------
-- | Access the clear text inside a secret.
readSecret :: Secret Clear -> Text
readSecret = decodeUtf8 . getSecret

--------------------------------------------------------------------------------
-- | Encrypt a secret.
--
-- This version generates a unique initialization vector which is
-- stored with the encrypted secret.
encrypt
  :: ( MonadRandom m
     )
  => Key AES256
  -- ^ The encryption key.

  -> Secret Clear
  -- ^ The text to encrypt.

  -> m (Either CryptoError (Secret Encrypted))
  -- ^ If successful, the encrypted secret.
encrypt k s = encrypt' <$> IV.generate <*> pure k <*> pure s

--------------------------------------------------------------------------------
-- | Encrypt a secret given a pre-generated IV.
encrypt'
  :: IV AES256
  -- ^ The initialization vector to use.  Should be unique.

  -> Key AES256
  -- ^ The encryption key.

  -> Secret Clear
  -- ^ The text to encrypt.

  -> Either CryptoError (Secret Encrypted)
  -- ^ If successful, the encrypted secret.
encrypt' iv (Key key) (Secret secret) = do
  cIV     <- IV.toCryptonite iv
  context <- eitherCryptoError $ Cryptonite.cipherInit key

  let bs = Cryptonite.ctrCombine context cIV secret
  pure $ Secret (getIV iv <> bs) -- Store IV in the secret.

--------------------------------------------------------------------------------
-- | Decrypt text that was previously encrypted.
decrypt
  :: Key AES256
  -- ^ The encryption key used to encrypt the text.

  -> Secret Encrypted
  -- ^ The previously encrypted secret.

  -> Either CryptoError (Secret Clear)
  -- ^ If successful, the decrypted text.
decrypt (Key key) (Secret bs) = do
  let size = Cryptonite.blockSize (undefined :: AES256)
      iv = IV (ByteString.take size bs) :: IV AES256
      secret = ByteString.drop size bs

  cIV <- IV.toCryptonite iv
  context <- eitherCryptoError $ Cryptonite.cipherInit key
  pure (Secret $ Cryptonite.ctrCombine context cIV secret)
