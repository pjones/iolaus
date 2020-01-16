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
module Main (main) where

--------------------------------------------------------------------------------
import Test.Tasty

--------------------------------------------------------------------------------
import qualified Iolaus.Test.Newtype as Newtype
import qualified Iolaus.Test.JSON as JSON

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Newtype.test
  , JSON.test
  ]
