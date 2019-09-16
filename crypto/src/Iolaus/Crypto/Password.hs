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

Secure storage of secrets while they are at rest (stored in a database).

-}
module Iolaus.Crypto.Password
  ( -- * Creating Passwords
    Password
  , Clear
  , Strong
  , Hashed
  , password
  , strength
  , hash
  , hash'

    -- * Verifying Passwords
  , VerifyStatus(..)
  , verify

    -- * Controlling Password Generation.
  , Settings(..)
  , defaultSettings
  ) where

--------------------------------------------------------------------------------
import Iolaus.Crypto.Internal.Password
import Iolaus.Crypto.Password.Settings
