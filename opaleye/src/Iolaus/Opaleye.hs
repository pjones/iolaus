{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
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
  , Config(..)
  , defaultConfig
  , Error(..)
  , AsError(..)
  , migrate
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
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Opaleye (FromFields, Select)
import qualified Opaleye as O

--------------------------------------------------------------------------------
-- For database migrations:
import Database.PostgreSQL.Simple.Util (existsTable)
import qualified Database.PostgreSQL.Simple.Migration as Migrate

--------------------------------------------------------------------------------
-- Retry failed queries:
import Control.Retry
  ( RetryPolicy
  , RetryStatus
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
  { _pool   :: Pool PostgreSQL.Connection
  , _config :: Config
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
-- | Instances of this class and execute Opaleye operations.
class CanOpaleye m where
  liftOpaleye :: OpaleyeM a -> m a

instance CanOpaleye OpaleyeM where
  liftOpaleye = id

instance (MonadIO m, MonadReader r m, HasOpaleye r) => CanOpaleye m where
  liftOpaleye k = view opaleye >>= liftIO . runReaderT (unOpaleye k)

--------------------------------------------------------------------------------
-- | Make an 'Opaleye' value to stick into your 'MonadReader'.
initOpaleye :: (MonadIO m) => Config -> m Opaleye
initOpaleye c =
  Opaleye <$> mkPool c
          <*> pure c

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
      liftIO $ retrying policy shouldRetry (\_ -> go env)

    go :: Opaleye -> IO (Either PostgreSQL.SqlError a)
    go env = Pool.withResource (view pool env) $ \conn ->
               catch (Right <$> f conn) handleSqlError

    handleSqlError :: PostgreSQL.SqlError -> IO (Either PostgreSQL.SqlError a)
    handleSqlError = pure . Left

    policy :: RetryPolicy
    policy = exponentialBackoff 250000 <> -- 0.25s
             limitRetries 2

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
     -- ^ Throws an error if there is a migration failure.
migrate dir verbose = do
  result <- liftOpaleye (view (opaleye.pool) >>= liftIO . flip Pool.withResource go)

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
-- | Execute a database @SELECT@.
select
  :: ( CanOpaleye m
     , MonadError e m
     , AsError e
     , Default FromFields a b
     )
  => Select a -- ^ The Opaleye 'Select' to execute.
  -> m [b]    -- ^ The result.
select s = run (flip O.runSelect s)
