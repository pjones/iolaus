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
module Iolaus.Database.Migrate
  ( MigrationResult(..)
  , MigrationVerbosity(..)
  , initialized
  , migrate
  ) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.Migration as Migrate
import Database.PostgreSQL.Simple.Util (existsTable)
import Database.PostgreSQL.Simple.Migration (MigrationResult(..))

--------------------------------------------------------------------------------
import Iolaus.Database.Runtime

--------------------------------------------------------------------------------
-- | Verbosity level while migrating the database.
data MigrationVerbosity
  = MigrateQuietly
  | MigrateVerbosely
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | 'True' if the database has been initialized (i.e. at least one
-- migration has run).
initialized :: MonadIO m => Runtime -> m Bool
initialized = liftIO . (`unsafeRunPg` go)
  where
    go :: PostgreSQL.Connection -> IO Bool
    go conn = existsTable conn "schema_migrations"

--------------------------------------------------------------------------------
-- | Run any necessary database migrations.
--
-- Migrations are run inside a database transaction.
migrate
  :: MonadIO m
  => Runtime
  -> FilePath
     -- ^ Path to a directory containing SQL migration files.
     --
     -- Typically this will be a subdirectory of your application's
     -- data directory which you can get from Cabal by using the
     -- generated @getDataDir@ function from the @Paths_*@ module.
  -> MigrationVerbosity
     -- ^ Produce verbose messages on stdout during the migration.
  -> m (MigrationResult String)
migrate rt dir mv = liftIO (unsafeRunPg rt go)
  where
    go :: PostgreSQL.Connection -> IO (Migrate.MigrationResult String)
    go conn = do
      exists <- existsTable conn "schema_migrations"

      let mi  = [Migrate.MigrationInitialization | not exists]
          ms  = mi ++ [Migrate.MigrationDirectory dir]

      PostgreSQL.withTransaction conn $
        Migrate.runMigrations (verbose mv) conn ms

    verbose :: MigrationVerbosity -> Bool
    verbose MigrateQuietly   = False
    verbose MigrateVerbosely = True
