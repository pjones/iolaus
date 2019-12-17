{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

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
import Crypto.Random (MonadRandom)
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
-- | A class of monads that can perform cryptographic operations.
--
-- Choose a concrete implementation from the instances list, or write
-- your own.
class (MonadRandom m) => MonadCrypto (m :: * -> *) where
  -- | Instance-specific key type.
  --
  -- Due to the fact that keys may be stored in a hardware device
  -- users are not allowed to access the implementation details of a
  -- key.  Therefore this type is completely opaque.
  data Key m

  data KeyPair m

  -- | The primary method of a cryptographic monad, evaluate a crypto
  -- operation.
  liftCryptoOpt :: CryptoOpt m a -> m a

--------------------------------------------------------------------------------
-- | Primitive cryptographic operations as a Free Monad.
data CryptoOptF (m :: * -> *) f
  = GenerateRandom Int (ByteString -> f)
  | GenerateKey Cipher Label (Key m -> f)
  | FetchKey Cipher Label (Maybe (Key m) -> f)
  | Encrypt (Key m) ByteString (Secret ByteString -> f)
  | Decrypt (Key m) (Secret ByteString) (ByteString -> f)
  | GenerateKeyPair Algo Label (KeyPair m -> f)
  | FetchKeyPair Algo Label (Maybe (KeyPair m) -> f)
  | ToPublicKey (KeyPair m) (PublicKey -> f)
  | AsymmetricEncrypt Label PublicKey ByteString (Secret ByteString -> f)
  | AsymmetricDecrypt (KeyPair m) (Secret ByteString) (ByteString -> f)
  | AsymmetricSign (KeyPair m) Hash ByteString (Signature ByteString -> f)
  | VerifySignature PublicKey (Signature ByteString) ByteString (SigStatus -> f)

deriving instance Functor (CryptoOptF m)

-- | The 'CryptoOptF' functor as a Church-encoded Free Monad.
type CryptoOpt m = F (CryptoOptF m)

makeFree ''CryptoOptF
