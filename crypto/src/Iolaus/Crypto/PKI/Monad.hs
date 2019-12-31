{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
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
module Iolaus.Crypto.PKI.Monad
  ( CaOpt
  , CaOptF(..)
  , MonadCertAuth(..)
  , nextSerialNumber
  , fetchHashAndAlgo
  , fetchRootCert
  , fetchIntermediateCert
  , signWithIntermediateCert
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.X509 (Certificate, SignedCertificate)

--------------------------------------------------------------------------------
-- Imports for MTL Instances:
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Cont

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key

--------------------------------------------------------------------------------
-- | A class of monads that can act like a Certificate Authority.
class (Monad m) => MonadCertAuth m where
  liftCaOpt :: CaOpt a -> m a

--------------------------------------------------------------------------------
instance (MonadCertAuth m) => MonadCertAuth (ExceptT e m) where
  liftCaOpt = lift . liftCaOpt
instance (MonadCertAuth m) => MonadCertAuth (StateT s m) where
  liftCaOpt = lift . liftCaOpt
instance (MonadCertAuth m) => MonadCertAuth (ReaderT r m) where
  liftCaOpt = lift . liftCaOpt
instance (MonadCertAuth m) => MonadCertAuth (IdentityT m) where
  liftCaOpt = lift . liftCaOpt
instance (MonadCertAuth m) => MonadCertAuth (ContT r m) where
  liftCaOpt = lift . liftCaOpt

--------------------------------------------------------------------------------
-- | Operations performed by a Certificate Authority.
data CaOptF f
  = NextSerialNumber (Integer -> f)
    -- ^ Must generate a new, unique number for each call.

  | FetchHashAndAlgo ((Hash, Algo) -> f)
    -- ^ Yield the hashing and asymmetric encryption algorithms that
    -- should be used.

  | FetchRootCert (SignedCertificate -> f)
    -- ^ Yield the signed root certificate.

  | FetchIntermediateCert (SignedCertificate -> f)
    -- ^ Yield the intermediate certificate that is currently active.
    -- Note that you may have to generate the certificate and sign it
    -- with the root certificate.

  | SignWithIntermediateCert Certificate (SignedCertificate -> f)
    -- ^ Sign the given certificate using the active intermediate
    -- certificate.  Note that you may have to generate the
    -- intermediate certificate and sign it with the root certificate
    -- first.

deriving instance Functor CaOptF

type CaOpt = F CaOptF

makeFree ''CaOptF
