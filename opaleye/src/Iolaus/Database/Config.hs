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

Database configuration.

-}
module Iolaus.Database.Config
  ( -- * Database Configuration
    DbConfig(..)

    -- * Lenses
  , HasDbConfig(..)

    -- * Default Configuration
  , defaultDbConfig
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makeClassy)
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- | Database configuration.
--
-- There are several ways to parse a configuration file that contains
-- the fields from this record:
--
--   * Decode from JSON via the @aeson@ package.
--   * Decode from YAML via the @yaml@ package.
--
-- @since 0.1.0.0
data DbConfig = DbConfig
  { _databaseConnectionString :: Text
    -- ^ libpq connection string.

  , _databasePoolSize :: Natural
    -- ^ Size of the database connection pool.

  , _databasePoolTimeoutSec :: Natural
    -- ^ Number of seconds to leave an unused connection open.

  , _databaseTransactionRetries :: Natural
    -- ^ Number of times to retry a failed transaction.  Set to @0@ to
    -- disable.

  , _databaseTransactionBackoff :: Natural
    -- ^ Number of microseconds (\(10^{-6}\)) to wait before retrying
    -- a failed transaction.  This value is increased exponentially
    -- after each subsequent failure.

  , _databaseMetricsPrefix :: Text
    -- ^ The prefix for collected metrics.  Example: @iolaus.opaleye@

  } deriving (Show, Eq)

makeClassy ''DbConfig

--------------------------------------------------------------------------------
instance FromJSON DbConfig where
  parseJSON = Aeson.withObject "Database Config" $ \v ->
    DbConfig
      <$> v .:  "connection_string"
      <*> v .:? "pool_size"        .!= _databasePoolSize
      <*> v .:? "pool_timeout_sec" .!= _databasePoolTimeoutSec
      <*> v .:? "retries"          .!= _databaseTransactionRetries
      <*> v .:? "backoff"          .!= _databaseTransactionBackoff
      <*> fmap fixPrefix (v .:? "metrics_prefix" .!= _databaseMetricsPrefix)

    where
      DbConfig{..} = defaultDbConfig (error "impossible")
      fixPrefix = Text.strip . Text.dropWhileEnd (== '.')

--------------------------------------------------------------------------------
instance ToJSON DbConfig where
  toJSON DbConfig{..} = Aeson.object
    [ "connection_string" .= _databaseConnectionString
    , "pool_size"         .= _databasePoolSize
    , "pool_timeout_sec"  .= _databasePoolTimeoutSec
    , "retries"           .= _databaseTransactionRetries
    , "backoff"           .= _databaseTransactionBackoff
    , "metrics_prefix"    .= _databaseMetricsPrefix
    ]

--------------------------------------------------------------------------------
-- | Build a default configuration by supplying a connection string.
--
-- @since 0.1.0.0
defaultDbConfig :: Text -> DbConfig
defaultDbConfig t =
  DbConfig
    { _databaseConnectionString   = t
    , _databasePoolSize           = 5
    , _databasePoolTimeoutSec     = 120
    , _databaseTransactionRetries = 3
    , _databaseTransactionBackoff = 50000 {- 50ms -}
    , _databaseMetricsPrefix      = "iolaus.opaleye"
    }
