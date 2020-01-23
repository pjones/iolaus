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

For more details, including a tutorial, please see the @example.hs@
file that is part of this distribution.

-}
module Iolaus.Database
  (

    -- * Queries
    Query
  , select
  , select1
  , selectToStream
  , count
  , insert
  , insert1
  , update
  , delete

    -- * Configuration
  , Config
  , connectionString
  , poolSize
  , poolTimeoutSec
  , retries
  , backoff
  , metricsPrefix
  , defaultConfig
  , Runtime
  , initRuntime

    -- * Errors
  , DbError(..)

    -- * Re-exports
  , module Opaleye
  ) where

--------------------------------------------------------------------------------
import Iolaus.Database.Error
import Iolaus.Database.Config
import Iolaus.Database.Query
import Iolaus.Database.Runtime (Runtime, initRuntime)

--------------------------------------------------------------------------------
import Opaleye
  ( Select
  , SelectArr
  , Insert(..)
  , Update(..)
  , Delete(..)
  , rCount
  , rReturning
  )
