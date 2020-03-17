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

An effect that provides access to a PostgreSQL database.

For a concrete implementation of the 'MonadDatabase' class please see
"Control.Monad.Database".

-}
module Control.Monad.Database.Class
  (
    -- * Database Class
    MonadDatabase

    -- * Running Queries Outside a Transaction
  , runQuery
  , runQueryEither

    -- * Running Queries Inside a Transaction
  , transaction
  , transactionEither
  , transactionWith
  , transactionEitherWith

    -- * Schema Migrations
  , migrate
  , migrationTableExists

    -- * Re-exports
  , MigrationVerbosity(..)
  , MigrationResult(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((#))
import Control.Monad.Except
import qualified Database.PostgreSQL.Simple.Transaction as PostgreSQL
import Database.PostgreSQL.Simple.Transaction hiding (rollback)

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Control.Monad.Database.Internal as M
import Control.Monad.Database.Internal (MonadDatabase(..))
import Iolaus.Database.Error
import Iolaus.Database.Migrate (MigrationVerbosity(..), MigrationResult(..))
import Iolaus.Database.Query

--------------------------------------------------------------------------------
-- | Execute a query and use 'Either' to encode errors.
--
-- The 'Query' is /not/ executed in a transaction.  To do that use the
-- 'transactionEitherWith' function or one of its variants.
--
-- Some (but not all) exceptions are caught and embedded in the
-- 'DbError' value.  Most notably the 'SqlError' exception is caught.
-- For more information see "Iolaus.Database".
--
-- @since 0.1.0.0
runQueryEither :: MonadDatabase m => Query a -> m (Either DbError a)
runQueryEither = liftDatabaseOp . M.runQuery

--------------------------------------------------------------------------------
-- | Execute a query using 'throwError' to encode errors.
--
-- For more information please see 'runQueryEither'.
--
-- @since 0.1.0.0
runQuery
  :: ( MonadDatabase m
     , MonadError e m
     , AsDbError  e
     )
  => Query a
  -> m a
runQuery = either (throwError . (_DbError #)) pure <=< runQueryEither

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the default 'TransactionMode'.
--
-- Errors are encoded as an 'Either'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionEither
  :: MonadDatabase m
  => Query a
  -> m (Either DbError a)
transactionEither = transactionEitherWith PostgreSQL.defaultTransactionMode

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the default 'TransactionMode'.
--
-- Errors are encoded with 'throwError'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transaction
  :: ( MonadDatabase m
     , MonadError e m
     , AsDbError e
     )
  => Query a
  -> m a
transaction = either (throwError . (_DbError #)) pure <=< transactionEither

--------------------------------------------------------------------------------
-- | Execute a query inside a transaction, providing the
-- 'TransactionMode' to use.
--
-- The given query will be run inside a transaction with the following
-- properties:
--
--   * /Any/ exception (other than 'PostgreSQL.SqlError') will cause
--     the transaction to rollback and the original exception will
--     resume propagation.
--
--   * 'PostgreSQL.SqlError' exceptions will cause a rollback and the
--      transaction will be retried based on the values in 'Config'.
--
--   * If all retries are exhausted a 'DbError' will be returned.
--
-- @since 0.1.0.0
transactionEitherWith
  :: MonadDatabase m
  => TransactionMode
  -> Query a
  -> m (Either DbError a)
transactionEitherWith = (liftDatabaseOp .) . M.transaction

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the given 'TransactionMode'.
--
-- Errors are encoded with 'throwError'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionWith
  :: ( MonadDatabase m
     , MonadError e m
     , AsDbError e
     )
  => TransactionMode
  -> Query a
  -> m a
transactionWith t =
  either (throwError . (_DbError #)) pure <=< transactionEitherWith t

--------------------------------------------------------------------------------
-- | Migrate the database.
--
-- @since 0.1.0.0
migrate
  :: (MonadDatabase m)
  => FilePath                   -- ^ Directory containing SQL files.
  -> MigrationVerbosity         -- ^ Verbosity level.
  -> m (MigrationResult String) -- ^ The migration results.
migrate = (liftDatabaseOp .) . M.migrate

--------------------------------------------------------------------------------
-- | Check to see if the table that holds migration data exists.  This
-- is useful for testing if you need to initialize the database or as
-- a trigger to know if your application is running for the first
-- time.
--
-- @since 0.1.0.0
migrationTableExists :: MonadDatabase m => m Bool
migrationTableExists = liftDatabaseOp M.migrationTableExists
