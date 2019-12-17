{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

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
module Iolaus.Crypto.Monad (
  -- * A Monad for Cryptography
  MonadCrypto(..),
  Key,
  KeyPair,

  -- * Cryptograpic Operations (Free Monad)
  CryptoOpt,
  CryptoOptF(..),

  -- * Key Management for Software Crypto
  KeyManager(..),
  GetStatus(..),
  PutStatus(..),

  -- * Smart Constructors for the Free Monad
  generateRandom,
  generateKey,
  fetchKey,
  encrypt,
  decrypt,

  generateKeyPair,
  fetchKeyPair,
  toPublicKey,
  asymmetricEncrypt,
  asymmetricDecrypt,
  asymmetricSign,
  verifySignature
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.ByteString (ByteString)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Cont

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

-- | Instance-specific symmetric key type.
--
-- Due to the fact that keys may be stored in a hardware device
-- users are not allowed to access the implementation details of a
-- key.  Therefore this type is completely opaque.
data family Key (k :: *)

-- | Instance-specific asymmetric key type.
data family KeyPair (k :: *)

--------------------------------------------------------------------------------
-- | A class of monads that can perform cryptographic operations.
--
-- Choose a concrete implementation from the instances list, or write
-- your own.
class (Monad m) => MonadCrypto k (m :: * -> *) | m -> k where
  -- | The primary method of a cryptographic monad, evaluate a crypto
  -- operation.
  liftCryptoOpt :: CryptoOpt k a -> m a

--------------------------------------------------------------------------------
instance (MonadCrypto k m) => MonadCrypto k (ExceptT e m) where
  liftCryptoOpt = lift . liftCryptoOpt
instance (MonadCrypto k m) => MonadCrypto k (StateT s m) where
  liftCryptoOpt = lift . liftCryptoOpt
instance (MonadCrypto k m) => MonadCrypto k (ReaderT r m) where
  liftCryptoOpt = lift . liftCryptoOpt
instance (MonadCrypto k m) => MonadCrypto k (IdentityT m) where
  liftCryptoOpt = lift . liftCryptoOpt
instance (MonadCrypto k m) => MonadCrypto k (ContT r m) where
  liftCryptoOpt = lift . liftCryptoOpt

--------------------------------------------------------------------------------
-- | Primitive cryptographic operations as a Free Monad.
data CryptoOptF (k :: *) f
  = GenerateRandom Int (ByteString -> f)
  | GenerateKey Cipher Label (Key k -> f)
  | FetchKey Cipher Label (Maybe (Key k) -> f)
  | Encrypt (Key k) ByteString (Secret ByteString -> f)
  | Decrypt (Key k) (Secret ByteString) (ByteString -> f)
  | GenerateKeyPair Algo Label (KeyPair k -> f)
  | FetchKeyPair Algo Label (Maybe (KeyPair k) -> f)
  | ToPublicKey (KeyPair k) (PublicKey -> f)
  | AsymmetricEncrypt Label PublicKey ByteString (Secret ByteString -> f)
  | AsymmetricDecrypt (KeyPair k) (Secret ByteString) (ByteString -> f)
  | AsymmetricSign (KeyPair k) Hash ByteString (Signature ByteString -> f)
  | VerifySignature PublicKey (Signature ByteString) ByteString (SigStatus -> f)

deriving instance Functor (CryptoOptF k)

-- | The 'CryptoOptF' functor as a Church-encoded Free Monad.
type CryptoOpt k = F (CryptoOptF k)

makeFree ''CryptoOptF
