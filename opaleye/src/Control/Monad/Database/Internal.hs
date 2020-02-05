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
module Control.Monad.Database.Internal
  ( MonadDatabase(..)
  , DatabaseOptF(..)
  , DatabaseOpt
  , runQuery
  , transaction
  , migrate
  , migrationTableExists
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Free.Church (MonadFree(..), F, liftF)
import Control.Monad.Free.TH (makeFree)
import Database.PostgreSQL.Simple.Transaction (TransactionMode)

--------------------------------------------------------------------------------
-- For MTL Instances:
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Strict as SState

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Database.Error
import Iolaus.Database.Migrate (MigrationVerbosity, MigrationResult)
import Iolaus.Database.Query.Internal (Query)

--------------------------------------------------------------------------------
-- | A class for monads that can run database operations.
--
-- @since 0.1.0.0
class (Monad m) => MonadDatabase m where
  liftDatabaseOp :: DatabaseOpt a -> m a

  default liftDatabaseOp :: (MonadTrans t, MonadDatabase m1, m ~ t m1) => DatabaseOpt a -> m a
  liftDatabaseOp = lift . liftDatabaseOp

--------------------------------------------------------------------------------
instance MonadDatabase m => MonadDatabase (ExceptT e m)
instance MonadDatabase m => MonadDatabase (StateT s m)
instance MonadDatabase m => MonadDatabase (SState.StateT s m)
instance MonadDatabase m => MonadDatabase (ReaderT r m)
instance MonadDatabase m => MonadDatabase (IdentityT m)
instance MonadDatabase m => MonadDatabase (ContT r m)

--------------------------------------------------------------------------------
-- | Actions that can be taken with a database connection.
--
-- @since 0.1.0.0
data DatabaseOptF f
  = forall a. RunQuery (Query a) (Either DbError a -> f)
  | forall a. Transaction TransactionMode (Query a) (Either DbError a -> f)
  | Migrate FilePath MigrationVerbosity (MigrationResult String -> f)
  | MigrationTableExists (Bool -> f)

deriving instance Functor DatabaseOptF
type DatabaseOpt = F DatabaseOptF
makeFree ''DatabaseOptF
