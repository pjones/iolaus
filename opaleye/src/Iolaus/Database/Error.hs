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
module Iolaus.Database.Error
  ( DbError(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Database.PostgreSQL.Simple as PostgreSQL

-- FIXME: Add a RollbackError constructor.

--------------------------------------------------------------------------------
-- | Database errors.
--
-- @since 0.1.0.0
newtype DbError
  = SqlError PostgreSQL.SqlError
    -- ^ A possibly recoverable error.  (Note: this /does/ not
    -- represent a SQL syntax error, but rather a problem running the
    -- SQL statement.)

  deriving (Eq, Show)
