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

-}
module Iolaus.Opaleye
  ( Opaleye
  , CanOpaleye(..)
  , HasOpaleye(..)
  , initOpaleye
  , select
  , insert
  , update
  , delete
  , Config(..)
  , defaultConfig
  , Error(..)
  , AsError(..)
  , migrate
  , unsafeRunPg
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Exception (catch)
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Data.Int (Int64)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Profunctor.Product.Default (Default)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as PostgreSQL
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
  , RetryStatus(..)
  , retrying
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
  { _pool         :: Pool PostgreSQL.Connection
  , _queryCounter :: Maybe Counter
  , _retryCounter :: Maybe Counter
  , _config       :: Config
  }

makeClassy ''Opaleye

--------------------------------------------------------------------------------
-- | The monad that Opaleye operations run in.
newtype OpaleyeM a = OpaleyeM
  { unOpaleye :: ReaderT Opaleye IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Opaleye
           , MonadIO
           )

--------------------------------------------------------------------------------
-- | Instances of this class can execute Opaleye operations.
class CanOpaleye m where
  liftOpaleye :: OpaleyeM a -> m a

instance CanOpaleye OpaleyeM where
  liftOpaleye = id

instance (MonadIO m, MonadReader r m, HasOpaleye r) => CanOpaleye m where
  liftOpaleye k = view opaleye >>= liftIO . runReaderT (unOpaleye k)

--------------------------------------------------------------------------------
-- | Make an 'Opaleye' value to stick into your 'MonadReader'.
initOpaleye :: (MonadIO m) => Config -> Maybe Metrics.Store -> m Opaleye
initOpaleye c store =
  Opaleye <$> mkPool c
          <*> (liftIO $ counter "num_db_queries")
          <*> (liftIO $ counter "num_db_retries")
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
  :: (CanOpaleye m)
  => (PostgreSQL.Connection -> IO a) -- ^ A function that receives the connection.
  -> m a                             -- ^ The function's result.
unsafeRunPg f =
  liftOpaleye (view (opaleye.pool) >>= liftIO . flip Pool.withResource f)

--------------------------------------------------------------------------------
-- | Internal function for running Opaleye queries.
run
  :: forall m e a.
     ( CanOpaleye m
     , MonadError e m
     , AsError e
     )
  => (PostgreSQL.Connection -> IO a)
  -> m a
run f = do
  result <- liftOpaleye run'

  case result of
    Right x -> pure x
    Left  y -> throwing _SqlError y

  where
    run' :: OpaleyeM (Either PostgreSQL.SqlError a)
    run' = do
      env <- view opaleye
      liftIO $ retrying (policy $ _config env) shouldRetry $ \rs -> do
        let counter = if rsIterNumber rs == 0 then _queryCounter else _retryCounter
        maybe (pure ()) Counter.inc (counter env)
        go env

    go :: Opaleye -> IO (Either PostgreSQL.SqlError a)
    go env = Pool.withResource (view pool env) $ \conn ->
               catch (Right <$> f conn) handleSqlError

    handleSqlError :: PostgreSQL.SqlError -> IO (Either PostgreSQL.SqlError a)
    handleSqlError = pure . Left

    policy :: Config -> RetryPolicyM IO
    policy Config{retries, backoff} =
      exponentialBackoff (maybe 50_000 {- 50ms -} fromIntegral backoff) <>
      limitRetries (maybe 3 fromIntegral retries)

    shouldRetry :: RetryStatus -> Either PostgreSQL.SqlError a -> IO Bool
    shouldRetry _ (Left _) = pure True
    shouldRetry _ _        = pure False

--------------------------------------------------------------------------------
-- | Run any necessary database migrations.
migrate
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
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
  result <- unsafeRunPg go

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
-- | Execute a database @SELECT@.  This is a wrapper around 'O.runSelect'.
select
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
     , Default FromFields a b
     )
  => Select a -- ^ The Opaleye 'Select' to execute.
  -> m [b]    -- ^ The result.
select s = run (flip O.runSelect s)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runInsert_'.
insert
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
     )
  => Insert a -- ^ The Opaleye 'Insert' to execute.
  -> m a      -- ^ The result.
insert i = run (flip O.runInsert_ i)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runUpdate_'.
update
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
     )
  => Update a -- ^ The Opaleye 'Update' to execute.
  -> m a      -- ^ The result.
update u = run (flip O.runUpdate_ u)

--------------------------------------------------------------------------------
-- | Wrapper around 'O.runDelete_'.
delete
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
     )
  => Delete Int64 -- ^ The Opaleye 'Delete' to execute.
  -> m Int64      -- ^ The result.
delete d = run (flip O.runDelete_ d)
