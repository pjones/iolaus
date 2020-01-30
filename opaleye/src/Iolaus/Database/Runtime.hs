{-# LANGUAGE TemplateHaskell #-}

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
module Iolaus.Database.Runtime
  ( Runtime
  , HasRuntime(..)
  , initRuntime
  , unsafeRunPg
  , catchQueryErrors
  ) where

--------------------------------------------------------------------------------
import Control.Exception (Handler(..), catches)
import Control.Lens ((^.), (#))
import Control.Lens.TH (makeClassy)
import Control.Monad.IO.Class
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified System.Metrics as Metrics
import System.Metrics.Counter (Counter)

--------------------------------------------------------------------------------
import Iolaus.Database.Config
import Iolaus.Database.Error

--------------------------------------------------------------------------------
-- | Run-time environment for database connections.
--
-- @since 0.1.0.0
data Runtime = Runtime
  { _pool         :: Pool Connection
  , _queryCounter :: Maybe Counter
  , _retryCounter :: Maybe Counter
  , __config     :: Config
  }

makeClassy ''Runtime

instance HasConfig Runtime where
  config = _config

--------------------------------------------------------------------------------
-- | Make an 'Runtime' value that is needed to execute queries.
--
-- @since 0.1.0.0
initRuntime :: (MonadIO m) => Config -> Maybe Metrics.Store -> m Runtime
initRuntime c store =
  Runtime <$> mkPool c
          <*> liftIO (counter "num_db_queries")
          <*> liftIO (counter "num_db_retries")
          <*> pure c
  where
    prefix :: Text
    prefix = c ^. metricsPrefix

    counter :: Text -> IO (Maybe Counter)
    counter name =
      case store of
        Nothing -> pure Nothing
        Just s  -> Just <$> Metrics.createCounter (prefix <> "." <> name) s

--------------------------------------------------------------------------------
-- | Given a configuration object, create a database handle.
mkPool :: (MonadIO m) => Config -> m (Pool Connection)
mkPool cfg =
    liftIO (Pool.createPool open close 1 timeout size)
  where
    constr  = Text.encodeUtf8 (cfg ^. connectionString)
    open    = PostgreSQL.connectPostgreSQL constr
    close   = PostgreSQL.close
    timeout = fromIntegral (cfg ^. poolTimeoutSec)
    size    = fromIntegral (cfg ^. poolSize)

--------------------------------------------------------------------------------
-- | Yields a database connection to a function.  Useful when you need
-- direct access to the database or to cover a case this library
-- doesn't accommodate.
--
-- Considered unsafe since it gives you direct access to IO, no
-- exceptions are caught, and no query retries are performed.
unsafeRunPg
  :: Runtime              -- ^ The run-time environment.
  -> (Connection -> IO a) -- ^ A function that receives the connection.
  -> IO a                 -- ^ The function's result.
unsafeRunPg e = Pool.withResource (e ^. pool)

--------------------------------------------------------------------------------
catchQueryErrors :: IO a -> IO (Either DbError a)
catchQueryErrors m =
  catches (Right <$> m)
    [ Handler $ \(e :: PostgreSQL.SqlError) -> pure (Left (_SqlError # e))
    , Handler $ \(_ :: Rollback) -> pure (Left (_RollbackError # ()))
    ]
