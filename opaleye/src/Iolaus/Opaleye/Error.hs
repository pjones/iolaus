{-# LANGUAGE TemplateHaskell       #-}

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
module Iolaus.Opaleye.Error
  ( OpaleyeError(..)
  , AsOpaleyeError(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as PostgreSQL

--------------------------------------------------------------------------------
-- | Database errors.
data OpaleyeError = SqlError PostgreSQL.SqlError
                    -- ^ A possibly recoverable error.  (Note: this
                    -- does not represent a SQL syntax error, but
                    -- rather a problem running the SQL statement.)

                  | MigrationError Text
                    -- ^ An error occurred during a database migration.

                  deriving (Eq, Show)

makeClassyPrisms ''OpaleyeError
