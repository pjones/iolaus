{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
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
module Iolaus.Database.Table.Internal
  ( Col
  , WriteMode(..)
  , NotAllowed(..)
  , ForHask
  , ForUI
  , SqlRead
  , SqlWrite
  , ForceNullable
  , ForceOptional
  ) where

--------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import GHC.TypeLits (Symbol)
import qualified Opaleye.Internal.TypeFamilies as T

--------------------------------------------------------------------------------
data WriteMode
  = ReadOnly
  | Required
  | ForeignKey
  | Optional
  | Nullable
  deriving Eq

--------------------------------------------------------------------------------
newtype NotAllowed = NotAllowed { notAllowed :: () }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Semigroup NotAllowed where
  (<>) x _ = x

instance Monoid NotAllowed where
  mempty = NotAllowed ()

--------------------------------------------------------------------------------
instance Aeson.ToJSON NotAllowed where
  toJSON _ = Aeson.Null
  toEncoding _ = Aeson.toEncoding Aeson.Null

--------------------------------------------------------------------------------
instance Aeson.FromJSON NotAllowed where
  parseJSON _ = pure (NotAllowed ())

--------------------------------------------------------------------------------
-- Type-level function to map 'WriteMode' constructors to Opaleye's
-- not-nullable (@NN@) and nullable (@N@) types.
type family OpaleyeNullable f where
  OpaleyeNullable 'ReadOnly   = T.NN
  OpaleyeNullable 'Required   = T.NN
  OpaleyeNullable 'ForeignKey = T.NN
  OpaleyeNullable 'Optional   = T.NN
  OpaleyeNullable 'Nullable   = T.N

--------------------------------------------------------------------------------
 -- Type-level function to map 'WriteMode' constructors to Opaleye's
 -- optional (@Opt@) and required (@Req@) types.
type family OpaleyeOptional f where
  OpaleyeOptional 'ReadOnly   = T.Opt
  OpaleyeOptional 'Required   = T.Req
  OpaleyeOptional 'ForeignKey = T.Req
  OpaleyeOptional 'Optional   = T.Opt
  OpaleyeOptional 'Nullable   = T.Req

--------------------------------------------------------------------------------
type family UIMap m h where
  UIMap 'ReadOnly h   = NotAllowed
  UIMap 'Required h   = h
  UIMap 'ForeignKey h = NotAllowed
  UIMap 'Optional h   = Maybe h
  UIMap 'Nullable h   = Maybe h

--------------------------------------------------------------------------------
data ForHask_
data ForUI_
data SqlWrite_
data SqlRead_
data ForceNullable_
data ForceOptional_

--------------------------------------------------------------------------------
-- | Fields take on their Haskell types.
type ForHask = 'T.H ForHask_

--------------------------------------------------------------------------------
-- | Fields take on their Haskell types with the exception of
-- 'ReadOnly' types which become 'NotAllowed'.
--
-- Useful for the 'FromJSON' instance to prevent the user interface
-- from setting fields such as primary and foreign keys.
type ForUI = 'T.H ForUI_

--------------------------------------------------------------------------------
-- | Fields take on their SQL write types.
type SqlWrite = 'T.H SqlWrite_

--------------------------------------------------------------------------------
-- | Fields take on their SQL read types.
type SqlRead = 'T.H SqlRead_

--------------------------------------------------------------------------------
-- | Overwrite all fields to 'Nullable'.  Used in place of 'SqlRead'.
--
-- Treats all fields as if they were originally defined as
-- 'Nullable'.  Mainly useful for Opaleye joins.
type ForceNullable = 'T.H ForceNullable_

--------------------------------------------------------------------------------
-- | Similar to 'ForceNullable' but used in place of 'ForHask'.
type ForceOptional = 'T.H ForceOptional_

--------------------------------------------------------------------------------
-- | Internal column encoding for the Opaleye @A@ type family.
newtype ColEnc a = ColEnc (a, a, WriteMode, Symbol)

--------------------------------------------------------------------------------
-- | Turn the record fields into plain Haskell types.
type instance T.A ('T.H ForHask_) ('ColEnc '(h, d, m, n))
  = T.TableRecordField ('T.H T.HT) h d (OpaleyeNullable m) (OpaleyeOptional m)

--------------------------------------------------------------------------------
-- | Turn the record fields into SQL types for writing.
type instance T.A ('T.H SqlWrite_) ('ColEnc '(h, d, m, n))
  = T.TableRecordField ('T.H T.WT) h d (OpaleyeNullable m) (OpaleyeOptional m)

--------------------------------------------------------------------------------
-- | Turn the record fields into SQL types for reading.
type instance T.A ('T.H SqlRead_) ('ColEnc '(h, d, m, n))
  = T.TableRecordField ('T.H T.OT) h d (OpaleyeNullable m) (OpaleyeOptional m)

--------------------------------------------------------------------------------
-- | Turn the record fields into nullable fields:
type instance T.A ('T.H ForceNullable_) ('ColEnc '(h, d, m, n))
  = T.TableRecordField ('T.H T.OT) h d T.N T.Req

--------------------------------------------------------------------------------
-- | Turn the record fields into optional fields:
type instance T.A ('T.H ForceOptional_) ('ColEnc '(h, d, m, n))
  = T.TableRecordField ('T.H T.HT) h d T.N T.Req

--------------------------------------------------------------------------------
-- | Turns record fields into Haskell types for user interfaces.
type instance T.A ('T.H ForUI_) ('ColEnc '(h, d, m, n)) = UIMap m h

--------------------------------------------------------------------------------
{- | A type family for record fields that allows the concrete type of
   the field to change as needed (writing to SQL, reading from SQL, etc.)

   It looks scary, but it's very straight forward:

   @
     Col f n h d m
         | | | | |
         | | | | `-> The write mode ('Required', 'Optional', etc.)
         | | | |
         | | | `---> The database type (SqlInt4, SqlText, etc.)
         | | |
         | | `-----> The Haskell type (Int, Text, etc.)
         | |
         | `-------> The column name in the database schema.
         |
         `---------> A type variable from the containing record.
                     Supplying a concrete type for f replaces Col
                     with a variant of the SQL or Haskell type.
   @

   An example:

   >>> type Color f = Col f "color" Text SqlText Required
   >>> :kind! Color SqlWrite
   Color SqlWrite :: *
   = Field SqlText
   >>> :kind! Color ForHask
   Color ForHask :: *
   = Text

-}
type Col f n h d m = T.A f ('ColEnc '(h, d, m, n))
