{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
module Control.Carrier.Database.Internal
  ( DatabaseC(..)
  , runDatabase
  ) where

--------------------------------------------------------------------------------
import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class

--------------------------------------------------------------------------------
import Control.Effect.Database.Internal (Database(..))
import Iolaus.Database.Migrate (migrate, initialized)
import Iolaus.Database.Query.Internal (Query(..))
import Iolaus.Database.Runtime (Runtime, catchQueryErrors, unsafeRunPg)
import Iolaus.Database.Transaction (transaction)

--------------------------------------------------------------------------------
-- | A carrier for database effects.
--
-- @since 0.1.0.0
newtype DatabaseC m a = DatabaseC
  { runDatabaseC :: ReaderC Runtime m a }
  deriving (Applicative, Functor, Monad, MonadIO)

--------------------------------------------------------------------------------
instance (MonadIO m, Algebra sig m) => Algebra (Database :+: sig) (DatabaseC m) where
  alg (R other) = DatabaseC (alg (R (handleCoercible other)))
  alg (L opt) = case opt of
    RunQuery q k -> DatabaseC (runQ q) >>= k
    Transaction t q k -> DatabaseC (runT t q) >>= k
    Migrate p v k -> DatabaseC (ask >>= \rt -> migrate rt p v) >>= k
    MigrationTableExists k -> DatabaseC (ask >>= initialized) >>= k
    where
      runQ (Query q) = do
        rt <- ask
        liftIO $ catchQueryErrors (unsafeRunPg rt $ \c -> runReader (rt, c) q)
      runT t (Query q) = do
        rt <- ask
        liftIO $ transaction rt t (\c -> runReader (rt, c) q)

--------------------------------------------------------------------------------
-- | Given a 'Runtime' value, discharge the 'Database' effect.
--
-- @since 0.1.0.0
runDatabase :: Runtime -> DatabaseC m a -> m a
runDatabase rt = runReader rt . runDatabaseC
