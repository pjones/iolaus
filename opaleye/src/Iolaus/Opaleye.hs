{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

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

The goal of this library is to provide a simple wrapper around the
[opaleye](https://hackage.haskell.org/package/opaleye) and
[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
packages without leaking a 'MonadIO' interface into your application's
monad, all while exposing an mtl + lens style of composing the
major components of an application.

Using this library is fairly straight forward:

  1. Create your monad transformer stack and then make it an instance
     of 'AsOpaleyeError' and 'HasOpaleye'.

  2. Parse a 'Config' value from a configuration file.

  3. Call 'initOpaleye' to create the 'MonadReader' value you'll need.

  4. Use the query functions inside your transformer stack!

For more details, including a tutorial, please see the @example.hs@
file that is part of this distribution.

-}
module Iolaus.Opaleye
  ( Query

  -- * Creating Queries
  --
  -- | These functions are simple wrappers around those found in the
  -- Opaleye package.
  , select
  , insert
  , update
  , delete

  -- * Database Transactions
  --
  -- | Run a 'Query' inside a transaction.
  , transaction
  , transaction'

  -- * Running Queries Outside a Transaction
  --
  -- | Run a 'Query' without needing to be in a transaction.
  --
  -- The 'PostgreSQL.SqlError' exception will be caught and turned
  -- into a 'MonadError' error.
  , CanOpaleye(..)

  -- * Reader Environment
  , initOpaleye
  , Opaleye
  , HasOpaleye(opaleye)

  -- * Configuration
  , Config(..)
  , defaultConfig

  -- * Errors
  , OpaleyeError(..)
  , AsOpaleyeError(..)

  -- * Schema Migrations
  , migrate

  -- * Raw Connection Access
  , unsafeRunPg
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Exception (SomeException, try)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Catch (Handler(..))
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Int (Int64)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.Transaction (TransactionMode)
import qualified Database.PostgreSQL.Simple.Transaction as PostgreSQL
import Opaleye (FromFields, Select, Insert, Update, Delete)
import qualified Opaleye as O
import qualified System.Metrics as Metrics
import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter

--------------------------------------------------------------------------------
-- For database migrations:
import Database.PostgreSQL.Simple.Util (existsTable)
import qualified Database.PostgreSQL.Simple.Migration as Migrate

--------------------------------------------------------------------------------
-- Retry failed queries:
import Control.Retry
  ( RetryPolicyM
  , recovering
  , exponentialBackoff
  , limitRetries
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Opaleye.Config
import Iolaus.Opaleye.Error

--------------------------------------------------------------------------------
-- | Run time environment for Opaleye.
data Opaleye = Opaleye
  { _pool         :: Pool Connection
  , _queryCounter :: Maybe Counter
  , _retryCounter :: Maybe Counter
  , _config       :: Config
  }

makeClassy ''Opaleye

--------------------------------------------------------------------------------
-- | The monad that Opaleye queries are run in.
--
-- To execute a query you can do one of the following:
--
--   1. Use the 'liftQuery' function.
--      This executes a query without wrapping it into a transaction
--      or performing any retries.
--
--   2. Use the 'transaction' function.
--      This executes the query inside of a transaction and
--      automatically retries the query based on the values inside the
--      'Config' type.
newtype Query a = Query
  { unQ :: ReaderT (Opaleye, Connection) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           )

--------------------------------------------------------------------------------
-- | Instances of this class can execute Opaleye queries.
--
-- As long as your transformer stack includes 'MonadIO',
-- 'MonadReader', and 'HasOpaleye', it should automatically be an
-- instance of this class.
class CanOpaleye m where
  -- | Execute a query outside of a transaction.  To run a query
  -- inside a transaction use the 'transaction' function instead.
  --
  -- 'PostgreSQL.SqlError' exceptions are caught and returned via 'MonadError'.
  liftQuery :: (MonadError e m, AsOpaleyeError e) => Query a -> m a

instance CanOpaleye Query where
  liftQuery = id

-- Default implementation:
instance (MonadIO m, MonadReader r m, HasOpaleye r) => CanOpaleye m where
  liftQuery q = do
    env <- view opaleye
    (result :: Either PostgreSQL.SqlError a) <-
      unsafeRunPg (\c -> try $ runReaderT (unQ q) (env, c))
    either (throwing _SqlError) pure result

--------------------------------------------------------------------------------
-- | Make an 'Opaleye' value to stick into your 'MonadReader'.
initOpaleye :: (MonadIO m) => Config -> Maybe Metrics.Store -> m Opaleye
initOpaleye c store =
  Opaleye <$> mkPool c
          <*> liftIO (counter "num_db_queries")
          <*> liftIO (counter "num_db_retries")
          <*> pure c
  where
    prefix :: Text
    prefix =
      case metricsPrefix c of
        Just t -> Text.strip $ Text.dropWhileEnd (== '.') t
        Nothing -> "iolaus.opaleye"

    counter :: Text -> IO (Maybe Counter)
    counter name =
      case store of
        Nothing -> pure Nothing
        Just s  -> Just <$> Metrics.createCounter (prefix <> "." <> name) s

--------------------------------------------------------------------------------
-- | Given a configuration object, create a database handle.
mkPool :: (MonadIO m) => Config -> m (Pool PostgreSQL.Connection)
mkPool Config{connectionString, poolSize, poolTimeoutSec} = do
    liftIO (Pool.createPool open close 1 timeout size)
  where
    constr  = Text.encodeUtf8 connectionString
    open    = PostgreSQL.connectPostgreSQL constr
    close   = PostgreSQL.close
    timeout = maybe 120 fromIntegral poolTimeoutSec
    size    = maybe 5   fromIntegral poolSize

--------------------------------------------------------------------------------
-- | Yields a database connection to a function.  Useful when you need
-- direct access to the database or to cover a case this library
-- doesn't accommodate.
--
-- Considered unsafe since it gives you direct access to IO, no
-- exceptions are caught, and no query retries are performed.
unsafeRunPg
  :: ( MonadReader r m
     , MonadIO m
     , HasOpaleye r
     )
  => (PostgreSQL.Connection -> IO a) -- ^ A function that receives the connection.
  -> m a                             -- ^ The function's result.
unsafeRunPg f = view (opaleye.pool) >>= liftIO . flip Pool.withResource f

--------------------------------------------------------------------------------
-- | Run any necessary database migrations.
migrate
  :: ( MonadError e m
     , CanOpaleye m
     , AsOpaleyeError e
     )
  => FilePath
     -- ^ Path to a directory containing SQL migration files.
     --
     -- Typically this will be a subdirectory of your application's
     -- data directory which you can get from Cabal by using the
     -- generated @getDataDir@ function from the @Paths_*@ module.
  -> Bool
     -- ^ Produce verbose messages on stdout during the migration.

  -> m ()
migrate dir verbose = do
  result <- liftQuery $ Query (ask >>= lift . go . snd)

  case result of
    Migrate.MigrationSuccess -> pure ()
    Migrate.MigrationError e -> throwing _MigrationError (Text.pack e)

  where
    go :: PostgreSQL.Connection -> IO (Migrate.MigrationResult String)
    go conn = do
      initialized <- existsTable conn "schema_migrations"

      let mi  = if initialized then [] else [Migrate.MigrationInitialization]
          ms  = mi ++ [Migrate.MigrationDirectory dir]

      PostgreSQL.withTransaction conn $
        Migrate.runMigrations verbose conn ms

--------------------------------------------------------------------------------
-- | Internal function to wrap a query function inside a 'Query'.
wrap :: (Connection -> IO a) -> Query a
wrap action = Query $ do
  (env, c) <- ask

  case _queryCounter env of
    Nothing  -> pure ()
    Just cnt -> lift (Counter.inc cnt)

  lift (action c)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runSelect'.
select
  :: ( Default FromFields a b )
  => Select a  -- ^ The Opaleye 'Select' to execute.
  -> Query [b] -- ^ The result.
select s = wrap (`O.runSelect` s)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runInsert_'.
insert
  :: Insert a -- ^ The Opaleye 'Insert' to execute.
  -> Query a  -- ^ The result.
insert i = wrap (`O.runInsert_` i)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runUpdate_'.
update
  :: Update a -- ^ The Opaleye 'Update' to execute.
  -> Query a  -- ^ The result.
update u = wrap (`O.runUpdate_` u)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runDelete_'.
delete
  :: Delete Int64 -- ^ The Opaleye 'Delete' to execute.
  -> Query Int64  -- ^ The result.
delete d = wrap (`O.runDelete_` d)

--------------------------------------------------------------------------------
-- | Execute a query inside a transaction with the default isolation
-- mode and default read-write mode.
--
-- For complete details, please read the documentation for 'transaction''.
transaction
  :: ( MonadError e m
     , CanOpaleye m
     , AsOpaleyeError e
     )
  => Query a
  -> m a
transaction = transaction' PostgreSQL.defaultTransactionMode

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
--   * If all retries are exhausted 'throwError' will be used to
--     return an error via the 'MonadError' constraint.
transaction'
  :: forall e m a.
     ( MonadError e m
     , CanOpaleye m
     , AsOpaleyeError e
     )
  => TransactionMode
  -> Query a
  -> m a
transaction' mode q =
    liftQuery $ Query (ask >>= lift . uncurry go)
  where
    go :: Opaleye -> Connection -> IO a
    go e c = recovering (policy $ _config e)
                        (map const handlers)
                        (\_ -> action e c)
      where
        handlers :: [ Handler IO Bool ]
        handlers  = [ Handler $ \(_ :: PostgreSQL.SqlError) -> handle e c
                    , Handler $ \(_ :: SomeException)       -> abort c
                    ]

    -- The database action wrapped in a transaction.
    action :: Opaleye -> Connection -> IO a
    action e c = do PostgreSQL.beginMode mode c
                    x <- runReaderT (unQ q) (e,c)
                    PostgreSQL.commit c
                    pure x

    -- Handle the exception by rolling back and requesting a retry.
    handle :: Opaleye -> Connection -> IO Bool
    handle e c = do
      case _retryCounter e of
        Nothing  -> pure ()
        Just cnt -> Counter.inc cnt
      PostgreSQL.rollback c
      pure True

    -- Rollback and don't do any retries.
    abort :: Connection -> IO Bool
    abort c = PostgreSQL.rollback c >> pure False

    policy :: Config -> RetryPolicyM IO
    policy Config{retries, backoff} =
      exponentialBackoff (maybe 50_000 {- 50ms -} fromIntegral backoff) <>
      limitRetries (maybe 3 fromIntegral retries)
