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

Types and functions for querying a database with "Opaleye".

-}
module Iolaus.Database.Query
  ( Query
  , select
  , select1
  , selectToStream
  , count
  , insert
  , insert1
  , update
  , delete
  , module Opaleye
  , module Opaleye.Operators
  , module Opaleye.Order
  ) where

--------------------------------------------------------------------------------
import Iolaus.Database.Query.Internal
import Opaleye.Operators
import Opaleye.Order

import Opaleye
  ( Select
  , SelectArr
  , Insert(..)
  , Update(..)
  , Delete(..)
  , rCount
  , rReturning
  , FromFields
  , toFields
  , selectTable
  )
