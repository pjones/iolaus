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
  KeyPair,

  -- * Core Symmetric Cryptography Operations
  generateRandom,
  generateKey,
  fetchKey,
  encrypt,
  decrypt,

  -- * Core Asymmetric Cryptography Operations
  generateKeyPair,
  fetchKeyPair,
  toPublicKey,
  asymmetricEncrypt,
  asymmetricDecrypt,
  asymmetricSign,
  verifySignature,

  -- * Passwords and Salts
  module Iolaus.Crypto.Password,
  module Iolaus.Crypto.Salt,

  -- * Salting, Hashing, and Encrypting
  module Iolaus.Crypto.SaltedHash,
  module Iolaus.Crypto.HashedSecret,

  -- * Ancillary Types and Functions
  Secret, secretBytes, secretMAC, secretLabel,
  Signature(..), SigStatus(..),
  Label, toLabel, getLabel, getLabelText,

  -- * Public and Private Keys
  PublicKey, encodePublicKey, decodePublicKey,
  HasKeyAccess(..),

  Cipher(..),
  Algo(..),
  Hash(..),

  module Iolaus.Crypto.Error,
  ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.API
import Iolaus.Crypto.Error (CryptoError(..), AsCryptoError(..))
import Iolaus.Crypto.HashedSecret
import Iolaus.Crypto.Key
import Iolaus.Crypto.Monad (MonadCrypto(..), Key, KeyPair, HasKeyAccess(..))
import Iolaus.Crypto.Password
import Iolaus.Crypto.Salt
import Iolaus.Crypto.SaltedHash
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature (Signature(..), SigStatus(..))
