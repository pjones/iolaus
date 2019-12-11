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
  -- , generateKeyPair
  fetchKey,
  -- , fetchKeyPair
  encrypt,
  -- , encryptA
  decrypt
  -- , decryptA
  -- , signA
  -- , verifyA
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Random (MonadRandom)
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.Binary (Binary)
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Secret

--------------------------------------------------------------------------------
-- data Signature a

--------------------------------------------------------------------------------
-- | A class of monads that can execute cryptographic operations.
--
-- Choose a concrete implementation from the instances list, or write
-- your own.
class (MonadRandom m) => MonadCrypto (m :: * -> *) where
  -- | Instance-specific key type.
  --
  -- Due to the fact that keys may be stored in a hardware device
  -- users are not allowed to access the implementation details of a
  -- key.  Therefore this type is completely opaque.
  data Key m (c :: Cipher)

  -- data KeyPair m (g :: Algo)

  -- | The primary method of a cryptographic monad, evaluate a crypto
  -- operation.
  liftCryptoOpt :: CryptoOpt m a -> m a

--------------------------------------------------------------------------------
-- | Primitive cryptographic operations as a Free Monad.
data CryptoOptF (m :: * -> *) f
  =               GenerateRandom Int (ByteString -> f)
  | forall t.   GenerateKey Label (Key m t -> f)
  --  forall t.   GenerateKeyPair Label (KeyPair m t -> f)
  | forall t.   FetchKey Label (Maybe (Key m t) -> f)
  --  forall t.   FetchKeyPair Label (Maybe (KeyPair m t) -> f)
  | forall t a. (Binary a) => Encrypt (Key m t) a (Secret a -> f)
  --  forall t a. (Binary a) => EncryptA (KeyPair m t) a (Secret a -> f)
  | forall t a. (Binary a) => Decrypt (Key m t) (Secret a) (a -> f)
  --  forall t a. (Binary a) => DecryptA (KeyPair m t) (Secret a) (a -> f)
  --  forall t a. (Binary a) => SignA (KeyPair m t) a (Signature a -> f)
  --  forall t a. VerifyA (KeyPair m t) (Signature a) (Bool -> f)


deriving instance Functor (CryptoOptF m)

-- | The 'CryptoOptF' functor as a Church-encoded Free Monad.
type CryptoOpt m = F (CryptoOptF m)

makeFree ''CryptoOptF
