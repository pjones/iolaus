{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

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
module Iolaus.Opaleye.Config
  ( Config(..)
  , defaultConfig
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Dhall (Interpret)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- | Database configuration.
--
-- There are several ways to parse a configuration file that contains
-- the fields from this record:
--
--   * Use the 'Dhall.inputFile' function from the @dhall@ package.
--   * Decode from JSON via the @aeson@ package.
--   * Decode from YAML via the @yaml@ package.
--
data Config = Config
  { connectionString :: Text
    -- ^ libpq connection string.

  , poolSize :: Maybe Natural
    -- ^ Size of the database connection pool.  A value of 'Nothing'
    -- means to use the default value.

  , poolTimeoutSec :: Maybe Natural
    -- ^ Number of seconds to leave an unused connection open.  A
    -- value of 'Nothing' means to use the default value.

  , retries :: Maybe Natural
    -- ^ Number of times to retry a failed query.  Set to @0@ to
    -- disable.  (A value of 'Nothing' means to use the default.)

  , backoff :: Maybe Natural
    -- ^ Number of microseconds (\(10^{-6}\)) to wait before retrying a
    -- failed query.  This value is increased exponentially after each
    -- subsequent failure.  (A value of 'Nothing' means use the
    -- default.)

  } deriving (Generic, Show, Eq, Interpret, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | Build a default configuration by supplying a connection string.
defaultConfig :: Text -> Config
defaultConfig t = Config { connectionString = t
                         , poolSize         = Nothing
                         , poolTimeoutSec   = Nothing
                         , retries          = Nothing
                         , backoff          = Nothing
                         }
