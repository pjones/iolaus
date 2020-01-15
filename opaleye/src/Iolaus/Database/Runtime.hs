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
  , pool
  , queryCounter
  , retryCounter
  , config
  , initRuntime
  , unsafeRunPg
  , catchQueryErrors
  ) where

--------------------------------------------------------------------------------
import Control.Exception (Handler(..), catches)
import Control.Monad.IO.Class
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Lens.Micro
import Lens.Micro.TH (makeLenses)
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
  , _config       :: Config
  }

makeLenses ''Runtime

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
    prefix =
      case metricsPrefix c of
        Just t -> Text.strip $ Text.dropWhileEnd (== '.') t
        Nothing -> "iolaus.opaleye"

    counter :: Text -> IO (Maybe Counter)
    counter name =
      case store of
        Nothing -> pure Nothing
        Just s  -> Just <$> Metrics.createCounter (prefix <> "." <> name) s

--------------------------------------------------------------------------------
-- | Given a configuration object, create a database handle.
mkPool :: (MonadIO m) => Config -> m (Pool Connection)
mkPool Config{connectionString, poolSize, poolTimeoutSec} =
    liftIO (Pool.createPool open close 1 timeout size)
  where
    constr  = Text.encodeUtf8 connectionString
    open    = PostgreSQL.connectPostgreSQL constr
    close   = PostgreSQL.close
    timeout = maybe 120 fromIntegral poolTimeoutSec
    size    = maybe 5   fromIntegral poolSize

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
    [ Handler $ \(e :: PostgreSQL.SqlError) -> pure . Left $ SqlError e ]
