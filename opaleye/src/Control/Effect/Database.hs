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

A database effect.

Predefined carriers:

  * "Control.Carrier.Database"

-}
module Control.Effect.Database
  (
    -- * Database Effect
    Database

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
  , TransactionMode(..)
  , IsolationLevel(..)
  , ReadWriteMode(..)
  , MigrationVerbosity(..)
  , MigrationResult(..)
  , Algebra
  , Effect
  , Has
  , run
  ) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Algebra
import Control.Effect.Database.Internal
import Control.Effect.Throw
import Control.Monad ((<=<))
import Database.PostgreSQL.Simple.Transaction
import qualified Database.PostgreSQL.Simple.Transaction as PostgreSQL
import Iolaus.Database.Error
import Iolaus.Database.Migrate (MigrationVerbosity(..), MigrationResult(..))
import Iolaus.Database.Query.Internal (Query)

--------------------------------------------------------------------------------
-- | Execute a query and use 'Either' to encode errors.
--
-- For more details please see the MTL version of
-- 'Control.Monad.Database.Class.runQueryEither'.
--
-- @since 0.1.0.0
runQueryEither :: Has Database sig m => Query a -> m (Either DbError a)
runQueryEither = send . (`RunQuery` pure)

--------------------------------------------------------------------------------
-- | Execute a query using 'throwError' to encode errors.
--
-- For more information please see 'runQueryEither'.
--
-- @since 0.1.0.0
runQuery :: (Has Database sig m, Has (Throw DbError) sig m) => Query a -> m a
runQuery = either throwError pure <=< runQueryEither

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the default 'TransactionMode'.
--
-- Errors are encoded as an 'Either'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionEither :: Has Database sig m => Query a -> m (Either DbError a)
transactionEither = transactionEitherWith PostgreSQL.defaultTransactionMode

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the default 'TransactionMode'.
--
-- Errors are encoded with 'throwError'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transaction :: (Has Database sig m, Has (Throw DbError) sig m) => Query a -> m a
transaction = either throwError pure <=< transactionEither

--------------------------------------------------------------------------------
-- | Execute a query inside a transaction, providing the
-- 'TransactionMode' to use.
--
-- For more details please see the MTL version of
-- 'Control.Monad.Database.Class.transactionEitherWith'.
--
-- @since 0.1.0.0
transactionEitherWith
  :: Has Database sig m
  => TransactionMode
  -> Query a
  -> m (Either DbError a)
transactionEitherWith m q = send (Transaction m q pure)

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the given 'TransactionMode'.
--
-- Errors are encoded with 'throwError'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionWith
  :: (Has Database sig m, Has (Throw DbError) sig m)
  => TransactionMode
  -> Query a
  -> m a
transactionWith t =
  either throwError pure <=< transactionEitherWith t

--------------------------------------------------------------------------------
-- | Migrate the database.
--
-- @since 0.1.0.0
migrate
  :: (Has Database sig m)
  => FilePath                   -- ^ Directory containing SQL files.
  -> MigrationVerbosity         -- ^ Verbosity level.
  -> m (MigrationResult String) -- ^ The migration results.
migrate p v = send (Migrate p v pure)

--------------------------------------------------------------------------------
-- | Check to see if the table that holds migration data exists.
--
-- For more details please see the MTL version of
-- 'Control.Monad.Database.Class.migrationTableExists'.
--
-- @since 0.1.0.0
migrationTableExists :: Has Database sig m => m Bool
migrationTableExists = send (MigrationTableExists pure)
