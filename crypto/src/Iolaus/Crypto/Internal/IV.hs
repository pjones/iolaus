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

License: Apache-2.0

Internal module for initialization vectors.

-}
module Iolaus.Crypto.Internal.IV
  ( IV(..)
  , generate
  , pack
  , toCryptonite
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.Types (BlockCipher(blockSize))
import qualified Crypto.Cipher.Types as Cryptonite
import qualified Crypto.Error as CE
import Crypto.Random (MonadRandom(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Error (CryptoError, wrappedCryptoError)

--------------------------------------------------------------------------------
-- | An initialization vector for cipher @c@.
newtype IV c = IV { getIV :: ByteString }

--------------------------------------------------------------------------------
-- | Generate an initialization vector.
generate :: forall m c. (MonadRandom m, BlockCipher c) => m (IV c)
generate = IV <$> getRandomBytes (blockSize (undefined :: c))

--------------------------------------------------------------------------------
-- | Pack an existing 'ByteString' into an initialization vector.
pack :: forall c. (BlockCipher c) => ByteString -> Either CryptoError (IV c)
pack bs =
  if ByteString.length bs == blockSize (undefined :: c)
     then Right (IV bs)
     else Left (wrappedCryptoError CE.CryptoError_IvSizeInvalid)

--------------------------------------------------------------------------------
-- | Convert an IV to what Cryptonite expects.
toCryptonite :: (BlockCipher c) => IV c -> Either CryptoError (Cryptonite.IV c)
toCryptonite (IV bs) =
  case Cryptonite.makeIV bs of
    Just x  -> Right x
    Nothing -> Left (wrappedCryptoError CE.CryptoError_IvSizeInvalid)
