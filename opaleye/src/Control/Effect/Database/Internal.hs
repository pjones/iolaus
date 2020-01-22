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
module Control.Effect.Database.Internal
  ( Database(..)
  , runQueryEither
  , runQuery
  , transactionEither
  , transaction
  , transactionEitherWith
  , transactionWith
  , rollback
  , migrate
  , migrationTableExists
  ) where


--------------------------------------------------------------------------------
import Control.Algebra
import Control.Effect.Throw
import Control.Monad ((<=<))
import Database.PostgreSQL.Simple.Transaction (TransactionMode)
import qualified Database.PostgreSQL.Simple.Transaction as PostgreSQL

--------------------------------------------------------------------------------
import Iolaus.Database.Error
import Iolaus.Database.Migrate (MigrationVerbosity, MigrationResult)
import Iolaus.Database.Query.Internal (Query)

--------------------------------------------------------------------------------
-- | Actions that can be taken with a database connection.
--
-- @since 0.1.0.0
data Database m k
  = forall a .
    RunQuery (Query a) (Either DbError a -> m k)

  | forall a .
    Transaction TransactionMode (Query a) (Either DbError a -> m k)

  | forall a .
    ThrowRollback (a -> m k)

  | Migrate FilePath MigrationVerbosity (MigrationResult String -> m k)

  | MigrationTableExists (Bool -> m k)

deriving instance Functor m => Functor (Database m)

instance HFunctor Database where
  hmap f = \case
    RunQuery q k           -> RunQuery q (f . k)
    Transaction t q k      -> Transaction t q (f . k)
    ThrowRollback k        -> ThrowRollback (f . k)
    Migrate p v k          -> Migrate p v (f . k)
    MigrationTableExists k -> MigrationTableExists (f . k)

instance Effect Database where
  thread ctx handler = \case
    RunQuery q k           -> RunQuery q (handler . (<$ ctx) . k)
    Transaction t q k      -> Transaction t q (handler . (<$ ctx) . k)
    ThrowRollback k        -> ThrowRollback (handler . (<$ ctx) . k)
    Migrate p v k          -> Migrate p v (handler . (<$ ctx) . k)
    MigrationTableExists k -> MigrationTableExists (handler . (<$ ctx) . k)

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
runQueryEither :: Has Database sig m => Query a -> m (Either DbError a)
runQueryEither q = send (RunQuery q pure)

--------------------------------------------------------------------------------
-- | Execute a query using 'throwError' to encode errors.
--
-- For more information please see 'runQueryEither'.
--
-- @since 0.1.0.0
runQuery
  :: ( Has Database sig m
     , Has (Throw DbError) sig m
     )
  => Query a
  -> m a
runQuery = either throwError pure <=< runQueryEither

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the default 'TransactionMode'.
--
-- Errors are encoded as an 'Either'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionEither
  :: Has Database sig m
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
  :: ( Has Database        sig m
     , Has (Throw DbError) sig m
     )
  => Query a
  -> m a
transaction = either throwError pure <=< transactionEither

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
  :: Has Database sig m
  => TransactionMode
  -> Query a
  -> m (Either DbError a)
transactionEitherWith t q = send (Transaction t q pure)

--------------------------------------------------------------------------------
-- | Run a query inside a transaction using the given 'TransactionMode'.
--
-- Errors are encoded with 'throwError'.
--
-- For more information please see 'transactionEitherWith'.
--
-- @since 0.1.0.0
transactionWith
  :: ( Has Database        sig m
     , Has (Throw DbError) sig m
     )
  => TransactionMode
  -> Query a
  -> m a
transactionWith t = either throwError pure <=< transactionEitherWith t

--------------------------------------------------------------------------------
-- | Abort and rollback the current transaction.
--
-- The running transaction (or query) will terminate with a
--' RollbackError' error.
rollback :: Has Database sig m => m a
rollback = send (ThrowRollback pure)

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
-- | Check to see if the table that holds migration data exists.  This
-- is useful for testing if you need to initialize the database or as
-- a trigger to know if your application is running for the first
-- time.
--
-- @since 0.1.0.0
migrationTableExists :: Has Database sig m => m Bool
migrationTableExists = send (MigrationTableExists pure)
