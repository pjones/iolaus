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
module Iolaus.Opaleye.Migrate
  ( migrate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PostgreSQL

--------------------------------------------------------------------------------
-- For database migrations:
import Database.PostgreSQL.Simple.Util (existsTable)

import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand(..)
  , MigrationResult(..)
  , runMigrations
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Opaleye.Error (AsError(..))

--------------------------------------------------------------------------------
-- | Run any necessary database migrations.
migrate
  :: ( MonadError e m
     , MonadIO m
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

  -> PostgreSQL.Connection
     -- ^ A database connection.

  -> m ()
     -- ^ Throws an error if there is a migration failure.
migrate dir verbose conn = do
  initialized  <- liftIO $ existsTable conn "schema_migrations"

  let mi  = if initialized then [] else [MigrationInitialization]
      ms  = mi ++ [MigrationDirectory dir]

  result <- liftIO $ PostgreSQL.withTransaction conn $
                       runMigrations verbose conn ms

  case result of
    MigrationSuccess -> pure ()
    MigrationError e -> throwing _MigrationError (Text.pack e)
