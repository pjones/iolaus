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
module Control.Monad.CertAuth
  ( CaOpt
  , CaOptF(..)
  , MonadCertAuth(..)
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
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict as SState

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key

--------------------------------------------------------------------------------
-- | A class of monads that can act like a Certificate Authority.
class (Monad m) => MonadCertAuth m where
  liftCaOpt :: CaOpt a -> m a

  default liftCaOpt :: (MonadTrans t, MonadCertAuth m1, m ~ t m1) => CaOpt a -> m a
  liftCaOpt = lift . liftCaOpt

--------------------------------------------------------------------------------
instance (MonadCertAuth m) => MonadCertAuth (ExceptT e m)
instance (MonadCertAuth m) => MonadCertAuth (StateT s m)
instance (MonadCertAuth m) => MonadCertAuth (SState.StateT s m)
instance (MonadCertAuth m) => MonadCertAuth (ReaderT r m)
instance (MonadCertAuth m) => MonadCertAuth (IdentityT m)
instance (MonadCertAuth m) => MonadCertAuth (ContT r m)

--------------------------------------------------------------------------------
-- | Operations performed by a Certificate Authority.
data CaOptF f
  = FetchHashAndAlgo ((Hash, Algo) -> f)
  | FetchRootCert (SignedCertificate -> f)
  | FetchIntermediateCert (SignedCertificate -> f)
  | SignWithIntermediateCert Certificate (SignedCertificate -> f)
  deriving (Functor)

type CaOpt = F CaOptF
makeFree ''CaOptF
