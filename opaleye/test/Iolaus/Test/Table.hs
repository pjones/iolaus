{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Iolaus.Test.Table
  ( test
  , Person(..)
  , people
  ) where

--------------------------------------------------------------------------------
import Data.Int (Int64)
import Data.Text (Text)
import Iolaus.Database.Table
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
type PersonId = Key Int64 Person

data Person f = Person
  { pk     :: Col f "id"     PersonId SqlInt8 ReadOnly
  , name   :: Col f "name"   Text     SqlText Required
  , age    :: Col f "age"    Int      SqlInt4 Required
  , height :: Col f "height" Int      SqlInt4 Nullable
  , evil   :: Col f "evil"   Bool     SqlBool Optional
  }

deriving instance Eq (Person ForHask)
deriving instance Show (Person ForHask)

makeTable ''Person "people"

--------------------------------------------------------------------------------
test :: TestTree
test = testCase "Table" (person @=? person)
  where
    person :: Person ForHask
    person = Person (Key 1) "Alice" 45 Nothing False
