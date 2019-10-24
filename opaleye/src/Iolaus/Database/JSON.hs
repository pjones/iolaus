{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

Use PostgreSQL JSON columns to hold Haskell types.

-}
module Iolaus.Database.JSON
  (
    -- * How to use this module
    -- $use

    -- * Functions
    liftJSON,

    -- * Types
    LiftJSON(..)

  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Profunctor.Product.Default (Default(def))
import Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH
import Opaleye hiding (FromField)

-- PostgreSQL
import Database.PostgreSQL.Simple.FromField
  ( FromField(..)
  , Conversion
  , ResultError(..)
  , returnError
  )

-- $use
--
-- If your type has 'ToJSON' and 'FromJSON' instances you can use the
-- 'liftJSON' function to generate the instances necessary to use it
-- with Opaleye:
--
-- > {-# LANGUAGE DeriveAnyClass  #-}
-- > {-# LANGUAGE DeriveGeneric   #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import Data.Aeson (ToJSON, FromJSON)
-- > import Data.Text (Text)
-- > import GHC.Generics (Generic)
-- > import Iolaus.Database.JSON (liftJSON)
-- >
-- > data Person = Person
-- >   { name :: Text
-- >   , age  :: Int
-- >   } deriving (Generic, ToJSON, FromJSON)
-- >
-- > liftJSON ''Person
--
-- Your type will be serialized to/from JSON and stored in a @SqlJsonb@ column.

--------------------------------------------------------------------------------
-- | A type wrapper to lift another type into PostgreSQL via @SqlJsonb@.
--
-- You shouldn't need to use this type wrapper but it's exported just
-- in case.
newtype LiftJSON a = LiftJSON { unliftJSON :: a }

--------------------------------------------------------------------------------
instance (FromJSON a, Typeable a) => FromField (LiftJSON a) where
  fromField f b = go =<< fromField f b
    where
      go :: (FromJSON a, Typeable a) => Aeson.Value -> Conversion (LiftJSON a)
      go v = case Aeson.fromJSON v of
               Aeson.Success x -> return (LiftJSON x)
               Aeson.Error e   -> returnError ConversionFailed f e

--------------------------------------------------------------------------------
instance (FromJSON a, Typeable a) => QueryRunnerColumnDefault SqlJsonb (LiftJSON a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

--------------------------------------------------------------------------------
instance (ToJSON a) => Default Constant (LiftJSON a) (Column SqlJsonb) where
  def = Constant (sqlValueJSONB . Aeson.toJSON . unliftJSON)

--------------------------------------------------------------------------------
-- | Use Template Haskell to generate database instances for @SqlJsonb@.
liftJSON :: TH.Name -> TH.Q [TH.Dec]
liftJSON name =
  [d|
    instance FromField $(TH.conT name) where
      fromField f b = unliftJSON <$> fromField f b

    instance QueryRunnerColumnDefault SqlJsonb $(TH.conT name) where
      queryRunnerColumnDefault = unliftJSON <$> queryRunnerColumnDefault

    instance Default Constant $(TH.conT name) (Column SqlJsonb) where
      def = Constant (sqlValueJSONB . Aeson.toJSON)
  |]
