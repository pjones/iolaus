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
module Iolaus.Database.Transaction
  ( transaction
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Catch (Handler(..), SomeException)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.Transaction (TransactionMode)
import qualified Database.PostgreSQL.Simple.Transaction as PostgreSQL
import Lens.Micro
import qualified System.Metrics.Counter as Counter

--------------------------------------------------------------------------------
import Control.Retry
  ( RetryPolicyM
  , recovering
  , exponentialBackoff
  , limitRetries
  )

--------------------------------------------------------------------------------
import Iolaus.Database.Config
import Iolaus.Database.Error
import Iolaus.Database.Runtime

--------------------------------------------------------------------------------
-- | Run a query inside a transaction.
transaction
  :: forall a . Runtime
  -> TransactionMode
  -> (Connection -> IO a)
  -> IO (Either DbError a)
transaction rt mode f = catchQueryErrors go
  where
    go :: IO a
    go =
      unsafeRunPg rt $ \c ->
        recovering
           (policy (rt ^. config))
           (map const (handlers c))
           (\_ -> action c)
      where
        handlers :: Connection -> [ Handler IO Bool ]
        handlers c = [ Handler $ \(_ :: PostgreSQL.SqlError) -> handle c
                     , Handler $ \(_ :: SomeException)       -> abort c
                     ]

    -- The database action wrapped in a transaction.
    action :: Connection -> IO a
    action c = PostgreSQL.beginMode mode c *> f c <* PostgreSQL.commit c

    -- Handle the exception by rolling back and requesting a retry.
    handle :: Connection -> IO Bool
    handle c = do
      case rt ^. retryCounter of
        Nothing  -> pure ()
        Just cnt -> Counter.inc cnt
      True <$ PostgreSQL.rollback c

    -- Rollback and don't do any retries.
    abort :: Connection -> IO Bool
    abort c = PostgreSQL.rollback c >> pure False

    policy :: Config -> RetryPolicyM IO
    policy cfg =
      exponentialBackoff (fromIntegral (cfg ^. backoff)) <>
      limitRetries (fromIntegral (cfg ^. retries))
