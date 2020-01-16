{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell  #-}

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

Test the JSON deriving code.

-}
module Iolaus.Test.JSON
  ( test
  ) where

--------------------------------------------------------------------------------
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Iolaus.Database.JSON
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
liftJSON ''Person

--------------------------------------------------------------------------------
-- | This exists just so we can make sure the deriving code compiles.
test :: TestTree
test = testCase "JSON" (assertBool "fail" True)
