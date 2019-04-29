{-# LANGUAGE ScopedTypeVariables #-}

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

Internal module for initialization vectors.

-}
module Sthenauth.Crypto.Internal.IV
  ( IV(..)
  , generate
  , pack
  , toCryptonite
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.Types (BlockCipher(blockSize))
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Error (CryptoError(CryptoError_IvSizeInvalid))
import Crypto.Random (MonadRandom(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString

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
     else Left CryptoError_IvSizeInvalid

--------------------------------------------------------------------------------
-- | Convert an IV to what Cryptonite expects.
toCryptonite :: (BlockCipher c) => IV c -> Either CryptoError (Cryptonite.IV c)
toCryptonite (IV bs) = 
  case Cryptonite.makeIV bs of
    Nothing -> Left CryptoError_IvSizeInvalid
    Just x  -> Right x
