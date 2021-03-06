{-# LANGUAGE DeriveAnyClass #-}

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

Database errors.

-}
module Iolaus.Database.Error
  (
    -- * Database Errors
    DbError(..)

    -- * Prisms
  , AsDbError(..)

    -- * Exceptions
  , Rollback(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Exception (Exception)
import Control.Lens.TH (makeClassyPrisms)
import qualified Database.PostgreSQL.Simple as PostgreSQL

--------------------------------------------------------------------------------
-- An exception that triggers a database transaction rollback.
--
-- @since 0.1.0.0
newtype Rollback = Rollback String
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

--------------------------------------------------------------------------------
-- | Database errors.
--
-- @since 0.1.0.0
data DbError
  = SqlError PostgreSQL.SqlError
    -- ^ A possibly recoverable error.  (Note: this /does/ not
    -- represent a SQL syntax error, but rather a problem running the
    -- SQL statement.)

  | RollbackError String
    -- ^ A 'Rollback' exception was thrown.

  deriving (Eq, Show)

makeClassyPrisms ''DbError
