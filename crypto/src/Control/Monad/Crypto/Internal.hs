{-# LANGUAGE UndecidableInstances #-}

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
module Control.Monad.Crypto.Internal
  ( MonadCrypto(..)
  , Key
  , KeyPair
  , CryptoOpt
  , CryptoOptF(..)
  , KeyManager(..)
  , FileExtension(..)
  , GetStatus(..)
  , PutStatus(..)
  , generateRandomBytes
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
import Control.Monad.Database (DatabaseT)
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- For MTL Instances:
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict as SState

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Internal.Key
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

-- | Instance-specific symmetric key type.
--
-- Due to the fact that keys may be stored in a hardware device
-- users are not allowed to access the implementation details of a
-- key.  Therefore this type is completely opaque.
data family Key :: * -> *

-- | Instance-specific asymmetric key type.
data family KeyPair :: * -> *

--------------------------------------------------------------------------------
-- | A class of monads that can perform cryptographic operations.
--
-- Choose a concrete implementation from the instances list, or write
-- your own.
class (Monad m) => MonadCrypto k (m :: * -> *) | m -> k where
  -- | The primary method of a cryptographic monad, evaluate a crypto
  -- operation.
  liftCryptoOpt :: CryptoOpt k a -> m a

  default liftCryptoOpt :: (MonadTrans t, MonadCrypto k m1, m ~ t m1) => CryptoOpt k a -> m a
  liftCryptoOpt = lift . liftCryptoOpt

--------------------------------------------------------------------------------
instance MonadCrypto k m => MonadCrypto k (ExceptT e m)
instance MonadCrypto k m => MonadCrypto k (StateT s m)
instance MonadCrypto k m => MonadCrypto k (SState.StateT s m)
instance MonadCrypto k m => MonadCrypto k (ReaderT r m)
instance MonadCrypto k m => MonadCrypto k (IdentityT m)
instance MonadCrypto k m => MonadCrypto k (ContT r m)
instance MonadCrypto k m => MonadCrypto k (DatabaseT m)

--------------------------------------------------------------------------------
-- | Primitive cryptographic operations as a Free Monad.
data CryptoOptF (k :: *) f
  = GenerateRandomBytes Int (ByteString -> f)
  | GenerateKey Cipher Label (Key k -> f)
  | FetchKey Label (Maybe (Key k) -> f)
  | Encrypt (Key k) ByteString (Secret ByteString -> f)
  | Decrypt (Key k) (Secret ByteString) (ByteString -> f)
  | GenerateKeyPair Algo Label (KeyPair k -> f)
  | FetchKeyPair Label (Maybe (KeyPair k) -> f)
  | ToPublicKey (KeyPair k) (PublicKey -> f)
  | AsymmetricEncrypt PublicKey ByteString (Secret ByteString -> f)
  | AsymmetricDecrypt (KeyPair k) (Secret ByteString) (ByteString -> f)
  | AsymmetricSign (KeyPair k) Hash ByteString (Signature ByteString -> f)
  | VerifySignature PublicKey (Signature ByteString) ByteString (SigStatus -> f)

deriving instance Functor (CryptoOptF k)

-- | The 'CryptoOptF' functor as a Church-encoded Free Monad.
type CryptoOpt k = F (CryptoOptF k)

makeFree ''CryptoOptF
