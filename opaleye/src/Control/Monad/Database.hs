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

This module provides the 'DatabaseT' monad transformer which
implements the 'MonadDatabase' class.

-}
module Control.Monad.Database
  ( -- * DatabaseT
    DatabaseT
  , runDatabaseT

    -- * Re-exports
  , MonadDatabase
  , module Control.Monad.Database.Class

  , Runtime
  , initRuntime
  , TransactionMode(..)
  , IsolationLevel(..)
  , ReadWriteMode(..)
  , MigrationVerbosity(..)
  , MigrationResult(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.Transaction hiding (rollback)

--------------------------------------------------------------------------------
-- For MTL Instances:
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.Database.Class hiding (MonadDatabase)
import Iolaus.Database.Error
import qualified Iolaus.Database.Migrate as M
import Iolaus.Database.Query.Internal (Query(..))
import Iolaus.Database.Runtime
import qualified Iolaus.Database.Transaction as T

import Control.Monad.Database.Internal
  ( MonadDatabase(..)
  , DatabaseOptF(..)
  , DatabaseOpt
  )

--------------------------------------------------------------------------------
-- | A monad transformer which implements 'MonadDatabase'.
--
-- @since 0.1.0.0
newtype DatabaseT m a = DatabaseT
  { unDatabaseT :: ReaderT Runtime m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)
  deriving newtype (MonadTrans, MonadCont, MonadError e, MonadState s)

--------------------------------------------------------------------------------
instance MonadReader r m => MonadReader r (DatabaseT m) where
  ask = lift ask
  reader = lift . reader
  local f m = do
    env <- DatabaseT ask
    DatabaseT (lift (local f (runDatabaseT env m)))

--------------------------------------------------------------------------------
instance MonadIO m => MonadDatabase (DatabaseT m) where
  liftDatabaseOp = evalDatabaseOpt

--------------------------------------------------------------------------------
evalDatabaseOpt
  :: MonadIO m
  => DatabaseOpt a
  -> DatabaseT m a
evalDatabaseOpt opt = DatabaseT . runF opt pure $ \case
    RunQuery q next -> runQ q >>= next
    Transaction t q next -> runT t q >>= next
    Migrate p v next -> ask >>= \rt -> M.migrate rt p v >>= next
    MigrationTableExists k -> ask >>= M.initialized >>= k

--------------------------------------------------------------------------------
runQ :: MonadIO m => Query a -> ReaderT Runtime m (Either DbError a)
runQ (Query q) = do
  rt <- ask
  liftIO $ catchQueryErrors (unsafeRunPg rt $ \c -> runReaderT q (rt, c))

--------------------------------------------------------------------------------
runT :: MonadIO m => TransactionMode -> Query a -> ReaderT Runtime m (Either DbError a)
runT t (Query q) = do
  rt <- ask
  liftIO $ T.transaction rt t (\c -> runReaderT q (rt, c))

--------------------------------------------------------------------------------
-- | Given a 'Runtime' value, discharge the 'MonadDatabase' constraint.
--
-- To create a 'Runtime' value you can use the 'initRuntime' function.
--
-- @since 0.1.0.0
runDatabaseT :: Runtime -> DatabaseT m a -> m a
runDatabaseT rt = (`runReaderT` rt) . unDatabaseT
