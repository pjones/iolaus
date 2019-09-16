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

-}
module Main (main) where

--------------------------------------------------------------------------------
import Test.Tasty

--------------------------------------------------------------------------------
import qualified Iolaus.Test.Crypto.Password as Password
import qualified Iolaus.Test.Crypto.Symmetric as Symmetric

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Password.run
  , Symmetric.run
  ]
