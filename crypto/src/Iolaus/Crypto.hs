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
module Iolaus.Crypto
  (
    -- * A Class of Monads for Cryptography
    MonadCrypto
  , Key
  , KeyPair

    -- * Random Bytes
  , generateRandomBytes

    -- * Symmetric Primitives
  , generateKey
  , fetchKey
  , encrypt
  , encrypt'
  , decrypt
  , decrypt'

    -- * Asymmetric Primitives
  , generateKeyPair
  , fetchKeyPair
  , toPublicKey
  , asymmetricEncrypt
  , asymmetricEncrypt'
  , asymmetricDecrypt
  , asymmetricDecrypt'
  , asymmetricSign
  , asymmetricSign'
  , verifySignature
  , verifySignature'

    -- * Passwords and Salts
  , module Iolaus.Crypto.Password
  , module Iolaus.Crypto.Salt

    -- * Salting, Hashing, and Encrypting
  , module Iolaus.Crypto.SaltedHash
  , module Iolaus.Crypto.HashedSecret

    -- * Public Keys
  , PublicKey
  , encodePublicKey
  , decodePublicKey

    -- * Private Keys
  , MonadKeyAccess
  , encodeKey
  , decodeKey
  , encodePrivateKey
  , decodePrivateKey

    -- * Ancillary Types and Functions
  , Secret
  , secretBytes
  , secretMAC
  , secretLabel
  , Signature(..)
  , SigStatus(..)
  , Label
  , toLabel
  , getLabel
  , getLabelText
  , Cipher(..)
  , Algo(..)
  , Hash(..)

  , module Iolaus.Crypto.Error
  ) where

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.Crypto
import Iolaus.Crypto.Error (CryptoError(..), AsCryptoError(..))
import Iolaus.Crypto.HashedSecret
import Iolaus.Crypto.Key
import Iolaus.Crypto.Password
import Iolaus.Crypto.Salt
import Iolaus.Crypto.SaltedHash
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature (Signature(..), SigStatus(..))
