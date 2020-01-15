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
  , module Iolaus.Database.Query
  ) where

--------------------------------------------------------------------------------
import Control.Algebra
import Control.Effect.Database.Internal
import Database.PostgreSQL.Simple.Transaction
import Iolaus.Database.Migrate (MigrationVerbosity(..), MigrationResult(..))
import Iolaus.Database.Query
