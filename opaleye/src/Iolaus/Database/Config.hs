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
module Iolaus.Database.Config
  ( Config
  , connectionString
  , poolSize
  , poolTimeoutSec
  , retries
  , backoff
  , metricsPrefix
  , defaultConfig
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makeLenses)
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
data Config = Config
  { _connectionString :: Text
    -- ^ libpq connection string.

  , _poolSize :: Natural
    -- ^ Size of the database connection pool.  A value of 'Nothing'
    -- means to use the default value.

  , _poolTimeoutSec :: Natural
    -- ^ Number of seconds to leave an unused connection open.  A
    -- value of 'Nothing' means to use the default value.

  , _retries :: Natural
    -- ^ Number of times to retry a failed query.  Set to @0@ to
    -- disable.  (A value of 'Nothing' means to use the default.)

  , _backoff :: Natural
    -- ^ Number of microseconds (\(10^{-6}\)) to wait before retrying a
    -- failed query.  This value is increased exponentially after each
    -- subsequent failure.  (A value of 'Nothing' means use the
    -- default.)

  , _metricsPrefix :: Text
    -- ^ The prefix for collected metrics.  Example: @iolaus.opaleye@

  } deriving (Show, Eq)

makeLenses ''Config

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = Aeson.withObject "Database Config" $ \v ->
    Config <$> v .:  "connection_string"
           <*> v .:? "pool_size"        .!= _poolSize
           <*> v .:? "pool_timeout_sec" .!= _poolTimeoutSec
           <*> v .:? "retries"          .!= _retries
           <*> v .:? "backoff"          .!= _backoff
           <*> fmap fixPrefix (v .:? "metrics_prefix" .!= _metricsPrefix)

    where
      Config{..} = defaultConfig (error "impossible")
      fixPrefix = Text.strip . Text.dropWhileEnd (== '.')

--------------------------------------------------------------------------------
instance ToJSON Config where
  toJSON Config{..} = Aeson.object
    [ "connection_string" .= _connectionString
    , "pool_size"         .= _poolSize
    , "pool_timeout_sec"  .= _poolTimeoutSec
    , "retries"           .= _retries
    , "backoff"           .= _backoff
    , "metrics_prefix"    .= _metricsPrefix
    ]

--------------------------------------------------------------------------------
-- | Build a default configuration by supplying a connection string.
--
-- @since 0.1.0.0
defaultConfig :: Text -> Config
defaultConfig t =
  Config { _connectionString = t
         , _poolSize         = 5
         , _poolTimeoutSec   = 120
         , _retries          = 3
         , _backoff          = 50000 {- 50ms -}
         , _metricsPrefix    = "iolaus.opaleye"
         }
