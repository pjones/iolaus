{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    git://code.devalot.com/sthenauth.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Secure storage of secrets while they are at rest (stored in a database).

-}
module Sthenauth.Crypto.Password
  ( -- * Creating Passwords
    Password
  , Clear
  , Hashed
  , password
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
import Sthenauth.Crypto.Internal.Password
import Sthenauth.Crypto.Password.Settings
