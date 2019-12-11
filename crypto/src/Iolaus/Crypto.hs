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
module Iolaus.Crypto (
  -- * A Monad for Cryptography
  MonadCrypto,
  Key,

  -- * Core Cryptography Operations
  generateRandom,
  generateKey,
  fetchKey,
  encrypt,
  decrypt,

  -- * Passwords and Salts
  module Iolaus.Crypto.Password,
  module Iolaus.Crypto.Salt,

  -- * Salting, Hashing, and Encrypting
  module Iolaus.Crypto.SaltedHash,
  module Iolaus.Crypto.HashedSecret,

  -- * Ancillary Types and Functions
  module Iolaus.Crypto.Secret,

  Label,
  toLabel,
  getLabel,
  getLabelText,
  Cipher(..),
  Algo(..),

  module Iolaus.Crypto.Error,
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.ByteString (ByteString)
import Data.Binary (Binary)

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Error (CryptoError(..), AsCryptoError(..))
import Iolaus.Crypto.HashedSecret
import Iolaus.Crypto.Key
import Iolaus.Crypto.Monad (MonadCrypto(..))
import qualified Iolaus.Crypto.Monad as M
import Iolaus.Crypto.Password
import Iolaus.Crypto.Salt
import Iolaus.Crypto.SaltedHash
import Iolaus.Crypto.Secret

--------------------------------------------------------------------------------
-- | Generate random bytes.
generateRandom :: (MonadCrypto m) => Int -> m ByteString
generateRandom = liftCryptoOpt . M.generateRandom

--------------------------------------------------------------------------------
-- | Generate a new symmetric cryptography key which will be
-- identified by the given 'Label'.
generateKey :: (MonadCrypto m) => Label -> m (Key m c)
generateKey = liftCryptoOpt. M.generateKey

--------------------------------------------------------------------------------
-- | Locate a previously generated symmetric cryptography key given
-- its 'Label'.
fetchKey :: (MonadCrypto m) => Label -> m (Maybe (Key m c))
fetchKey = liftCryptoOpt . M.fetchKey

--------------------------------------------------------------------------------
-- | Symmetric encryption of any type that can be converted to 'Binary'.
encrypt :: (MonadCrypto m, Binary a) => Key m c -> a -> m (Secret a)
encrypt = (liftCryptoOpt .) . M.encrypt

--------------------------------------------------------------------------------
-- | Symmetric decryption of any type that can be converted from 'Binary'.
decrypt :: (MonadCrypto m, Binary a) => Key m c -> Secret a -> m a
decrypt = (liftCryptoOpt .) . M.decrypt
