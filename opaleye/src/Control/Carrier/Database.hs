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
module Control.Carrier.Database
  ( DatabaseC
  , runDatabase
  , module Control.Effect.Database
  , module Iolaus.Database
  ) where

--------------------------------------------------------------------------------
import Control.Carrier.Database.Internal
import Control.Effect.Database
import Iolaus.Database
