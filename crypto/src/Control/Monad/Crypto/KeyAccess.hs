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
module Control.Monad.Crypto.KeyAccess
  ( MonadKeyAccess(..)
  , KeyAccessOp
  , KeyAccessOpF(..)
  , encodeKey
  , decodeKey
  , toX509PrivKey
  , encodePrivateKey
  , decodePrivateKey
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Database (DatabaseT)
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Data.ByteString.Lazy (ByteString)
import qualified Data.X509 as X509

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
import Control.Monad.Crypto.Internal
import Iolaus.Crypto.Key

--------------------------------------------------------------------------------
-- | An extension to 'MonadCrypto' that can expose keys.  This can only
-- be implemented for software-based cryptography as hardware devices
-- typically don't allow access to the raw key data.
class MonadCrypto k m => MonadKeyAccess k (m :: * -> *) | m -> k where
  liftKeyAccessOp :: KeyAccessOp k a -> m a

  default liftKeyAccessOp :: (MonadTrans t, MonadKeyAccess k m1, m ~ t m1) => KeyAccessOp k a -> m a
  liftKeyAccessOp = lift . liftKeyAccessOp

--------------------------------------------------------------------------------
instance MonadKeyAccess k m => MonadKeyAccess k (ExceptT e m)
instance MonadKeyAccess k m => MonadKeyAccess k (StateT s m)
instance MonadKeyAccess k m => MonadKeyAccess k (SState.StateT s m)
instance MonadKeyAccess k m => MonadKeyAccess k (ReaderT r m)
instance MonadKeyAccess k m => MonadKeyAccess k (IdentityT m)
instance MonadKeyAccess k m => MonadKeyAccess k (ContT r m)
instance MonadKeyAccess k m => MonadKeyAccess k (DatabaseT m)

--------------------------------------------------------------------------------
data KeyAccessOpF (k :: *) f
  = EncodeKey (Key k) (ByteString -> f)
  | DecodeKey ByteString (Maybe (Key k) -> f)
  | ToX509PrivKey (KeyPair k) (X509.PrivKey -> f)
  | EncodePrivateKey (KeyPair k) (ByteString -> f)
  | DecodePrivateKey Label ByteString (Maybe (KeyPair k) -> f)

deriving instance Functor (KeyAccessOpF k)
type KeyAccessOp k = F (KeyAccessOpF k)
makeFree ''KeyAccessOpF
