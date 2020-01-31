{-|

Copyright:
  This file is part of the package iolaus. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

Error that might occur while performing cryptograpic operations.

-}
module Iolaus.Crypto.Error
  ( -- * Crypto Error Type
    CryptoError(..)

    -- * Prisms
  , AsCryptoError(..)
  ) where

--------------------------------------------------------------------------------
import Iolaus.Crypto.Internal.Error
