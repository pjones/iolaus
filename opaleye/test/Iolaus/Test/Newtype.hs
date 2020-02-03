{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

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

Test the Template Haskell in @Iolaus.Opaleye.Newtype@.

-}
module Iolaus.Test.Newtype
  ( Name(..)
  , testInsert
  , testSelect
  , test
  ) where

--------------------------------------------------------------------------------
import Data.Int (Int64)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import GHC.Generics (Generic)
import Iolaus.Database.Newtype (makeNewtypeInstances)
import Iolaus.Database.Query
import Opaleye (Table, Field, table, tableField)
import qualified Opaleye as O
import qualified Opaleye.Constant as C
import Opaleye.SqlTypes (SqlText)

--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- | Example newtype wrapper we want to use as a column:
newtype Name = Name { unName :: Text }

--------------------------------------------------------------------------------
-- Generate the Opaleye instances for translating Name <-> SqlText.
makeNewtypeInstances ''Name ''SqlText

--------------------------------------------------------------------------------
-- | Example table:
data Person' name = Person
  { fname :: name
  , lname :: name
  } deriving (Generic, Show)

type Person = Person' Name

$(makeAdaptorAndInstance "pPerson" ''Person')

--------------------------------------------------------------------------------
people :: Table (Person' (Field SqlText)) (Person' (Field SqlText))
people = table "people" (pPerson
  Person { fname = tableField "first_name"
         , lname = tableField "last_name"
         })

--------------------------------------------------------------------------------
-- | Test inserts.
testInsert :: Query Int64
testInsert =
  let p = Person (C.constant $ Name "J") (C.constant $ Name "Doe")
  in insert $ O.Insert people [p] O.rCount Nothing

--------------------------------------------------------------------------------
-- | Test selects.
testSelect :: Query [Person]
testSelect = select (O.selectTable people)

--------------------------------------------------------------------------------
-- | This exists so that all of the Template Haskell runs through the compiler.
test :: TestTree
test = testCase "Newtype" (assertBool "fail" True)
