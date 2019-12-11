{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
module Iolaus.Crypto.Secret
  ( Secret(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as CBS
import Data.Profunctor.Product.Default (Default(def))

import Database.PostgreSQL.Simple.FromField
  ( FromField(..)
  , Conversion
  )

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , SqlJsonb
  , fieldQueryRunnerColumn
  , sqlValueJSONB
  )

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Key (Label(..))

--------------------------------------------------------------------------------
-- | A value of type @a@ that has been encrypted.
data Secret a = Secret
  { secretBytes :: ByteString  -- ^ The encrypted data.
  , secretLabel :: Label       -- ^ The label for the key that was used.
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance ToJSON (Secret a) where
  toJSON s = Aeson.object
    [ "data" .= Encoding.encode (Encoding (secretBytes s))
    , "key"  .= CBS.unpack (getLabel (secretLabel s))
    ]

instance FromJSON (Secret a) where
  parseJSON = Aeson.withObject "Secret" $ \v ->
    Secret <$> fmap (Encoding.getBytes . Encoding.decode) (v .: "data")
           <*> fmap (Label . CBS.pack) (v .: "key")

--------------------------------------------------------------------------------
instance FromField (Secret a) where
  fromField f b = go =<< fromField f b
    where
      go :: Aeson.Value -> Conversion (Secret a)
      go v = case Aeson.fromJSON v of
               Aeson.Success x -> pure x
               Aeson.Error e   -> fail e

instance QueryRunnerColumnDefault SqlJsonb (Secret a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (Secret a) (Column SqlJsonb) where
  def = Constant sqlValueJSONB
