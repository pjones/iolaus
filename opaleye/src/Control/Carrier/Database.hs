{-# LANGUAGE UndecidableInstances #-}

 {-|

Copyright:
  This file is part of the package iolaus.  It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

-}
module Control.Carrier.Database
  ( DatabaseC
  , runDatabase

    -- * Re-exports
  , Runtime
  , initRuntime
  , module Control.Effect.Database
  ) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Database
import Control.Effect.Database.Internal
import Control.Monad.Database
import qualified Control.Monad.Database.Class as D
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (MonadTrans(..))

--------------------------------------------------------------------------------
-- | @since 1.0.0.0
newtype DatabaseC m a = DatabaseC
  { runDatabaseC :: ReaderC Runtime m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans)

--------------------------------------------------------------------------------
-- | Lift a 'DatabaseT' operation into 'DatabaseC'.
liftDatabaseT :: Algebra sig m => DatabaseT m a -> DatabaseC m a
liftDatabaseT op = DatabaseC (lift . (`runDatabaseT` op) =<< ask)

--------------------------------------------------------------------------------
instance (MonadIO m, Algebra sig m) => Algebra (Database :+: sig) (DatabaseC m) where
  alg = \case
    R other -> DatabaseC (alg (R (handleCoercible other)))
    L (RunQuery q k) -> liftDatabaseT (D.runQueryEither q) >>= k
    L (Transaction m q k) -> liftDatabaseT (D.transactionEitherWith m q) >>= k
    L (Migrate p v k) -> liftDatabaseT (D.migrate p v) >>= k
    L (MigrationTableExists k) -> liftDatabaseT D.migrationTableExists >>= k

--------------------------------------------------------------------------------
-- | Discharge the 'Database' effect.
--
-- To create a 'Runtime' value you can use the 'initRuntime' function.
--
-- @since 1.0.0.0
runDatabase :: Runtime -> DatabaseC m a -> m a
runDatabase rt = runReader rt . runDatabaseC
