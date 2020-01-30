{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE PolyKinds      #-}

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

This module provides an alternative way of defining a record type that
can be serialized to/from a database.

Instead of defining a record that is polymorphic in all of its fields,
we can use a single type variable along with a type family.  This will
create a record that can change its field types for different
operations (e.g., writing to the database, reading from the database,
etc.)

The examples given in this module assume the following imports:

>>> import Data.Int (Int64)
>>> import Data.Text (Text)
>>> import Iolaus.Database.Table
>>> import Opaleye.Field
>>> import Opaleye.SqlTypes

and the following extensions:

>>> :set -XTypeApplications
>>> :set -XTemplateHaskell
>>> :set -XUndecidableInstances

and the following types:

@
type PersonId = 'Key' Int64 Person

data Person f = Person
 { pkey   :: 'Col' f "id"     PersonId SqlInt8 'ReadOnly'
 , name   :: 'Col' f "name"   Text     SqlText 'Required'
 , age    :: 'Col' f "age"    Int      SqlInt4 'Nullable'
 , height :: 'Col' f "height" Int      SqlInt4 'Optional'
 }

makeTable ''Person "people"
@

The @Person@ type above uses a single type variable (@f@).  Then, each
field is defined using the 'Col' type family.  Finally, the
'makeTable' function is used to generate all the necessary
boilerplate, including a @people@ function that returns the Opaleye
@Table@ type.

NOTE: Currently the code in this module requires that our types
contain one, and only one type variable.  This wouldn't be too hard to
change, but no one has done the work yet.

-}
module Iolaus.Database.Table
  (
    -- * Columns and Keys
    Col
  , Key(..)

    -- * Column Type Modes
    --
    -- | Types to modify the final concrete field type based on
    -- whether or not values are available for reading from or writing
    -- to the database.
  , Required
  , Nullable
  , Optional
  , ReadOnly

    -- * Types to Make Records Concrete
    --
    -- | These types are meant to fill the polymorphic type variable in
    -- records, and thus the 'Col' type family.
    --
    -- See examples above under the type modes such as 'Required',
    -- 'Optional', etc.
  , SqlWrite
  , SqlRead
  , ForHask
  , ForUI
  , ForceNullable

  -- * User Interface Helpers
  , NotAllowed(..)

    -- * Template Haskell Functions
  , makeTable

    -- * Re-exports
  , module Opaleye.SqlTypes
  , module Data.Int
  , module Data.UUID
  ) where

--------------------------------------------------------------------------------
import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int64)
import Data.Profunctor.Product.Default (Default(def))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField(..))
import GHC.Generics (Generic)
import Iolaus.Database.Table.Internal
import Iolaus.Database.Table.TH
import Opaleye.SqlTypes

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , fieldQueryRunnerColumn
  , toFields
  )

--------------------------------------------------------------------------------
-- | Type safety for primary and foreign keys.
--
-- The 'Key' type is a simple wrapper around the type used for primary
-- and foreign keys in the database.  It helps prevent accidents where
-- a key from one table is substituted for another.
--
-- To use this as a field type, provide a Haskell type for the @t@
-- variable and the table type for @a@.
--
-- The following types are supported for the @t@ variable:
--
--   * 'Int64'
--   * 'UUID'
newtype Key t a = Key { getKey :: t }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON)

instance FromField t => FromField (Key t a) where
  fromField f b = Key <$> fromField f b

instance QueryRunnerColumnDefault SqlUuid (Key UUID a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault SqlInt8 (Key Int64 a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (Key UUID a) (Column SqlUuid) where
  def = Constant (toFields . getKey)

instance Default Constant (Key Int64 a) (Column SqlInt8) where
  def = Constant (toFields . getKey)

--------------------------------------------------------------------------------
{- | Marks the record field as mandatory (cannot be null, must be
   present for writes and reads).

   >>> :t name @SqlWrite
   name @SqlWrite :: Person SqlWrite -> Field SqlText

   >>> :t name @SqlRead
   name @SqlRead :: Person SqlRead -> Field SqlText

   >>> :t name @ForHask
   name @ForHask :: Person ForHask -> Text

   >>> :t name @ForUI
   name @ForUI :: Person ForUI -> Text

-}
type Required = 'Required

--------------------------------------------------------------------------------
{- | Marks the record field as possibly containing a null value.

   >>> :t age @SqlWrite
   age @SqlWrite :: Person SqlWrite -> FieldNullable SqlInt4

   >>> :t age @SqlRead
   age @SqlRead :: Person SqlRead -> FieldNullable SqlInt4

   >>> :t age @ForHask
   age @ForHask :: Person ForHask -> Maybe Int

   >>> :t age @ForUI
   age @ForUI :: Person ForUI -> Maybe Int

-}
type Nullable = 'Nullable

--------------------------------------------------------------------------------
{- | Marks a record field as optional when writing to the database.

   Differs from 'Nullable' since the value is still required to be set
   in the database (cannot be null).

   Useful for table columns that have default values.

   >>> :t height @SqlWrite
   height @SqlWrite :: Person SqlWrite -> Maybe (Field SqlInt4)

   >>> :t height @SqlRead
   height @SqlRead :: Person SqlRead -> Field SqlInt4

  >>> :t height @ForHask
  height @ForHask :: Person ForHask -> Int

   >>> :t height @ForUI
   height @ForUI :: Person ForUI -> Maybe Int

-}
type Optional = 'Optional

--------------------------------------------------------------------------------
{- | Marks a record field as read-only (for columns with default values).

   >>> :t pkey @SqlWrite
   pkey @SqlWrite :: Person SqlWrite -> Maybe (Field SqlInt8)

   >>> :t pkey @SqlRead
   pkey @SqlRead :: Person SqlRead -> Field SqlInt8

   >>> :t pkey @ForHask
   pkey @ForHask :: Person ForHask -> PersonId

   >>> :t pkey @ForUI
   pkey @ForUI :: Person ForUI -> NotAllowed

-}
type ReadOnly = 'ReadOnly
