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
  ) where

--------------------------------------------------------------------------------
-- Imports:
import Control.Algebra
import Database.PostgreSQL.Simple.Transaction (TransactionMode)
import Iolaus.Database.Error
import Iolaus.Database.Migrate (MigrationVerbosity, MigrationResult)
import Iolaus.Database.Query.Internal (Query)

--------------------------------------------------------------------------------
data Database m k
  = forall a. RunQuery (Query a) (Either DbError a -> m k)
  | forall a. Transaction TransactionMode (Query a) (Either DbError a -> m k)
  | Migrate FilePath MigrationVerbosity (MigrationResult String -> m k)
  | MigrationTableExists (Bool -> m k)

--------------------------------------------------------------------------------
deriving instance Functor m => Functor (Database m)

instance HFunctor Database where
  hmap f = \case
    RunQuery q k -> RunQuery q (f . k)
    Transaction m q k -> Transaction m q (f . k)
    Migrate p v k -> Migrate p v (f . k)
    MigrationTableExists k -> MigrationTableExists (f . k)

instance Effect Database where
  thread ctx handler = \case
    RunQuery q k -> RunQuery q (handler . (<$ ctx) . k)
    Transaction m q k -> Transaction m q (handler . (<$ ctx) . k)
    Migrate p v k -> Migrate p v (handler . (<$ ctx) . k)
    MigrationTableExists k -> MigrationTableExists (handler . (<$ ctx) . k)
