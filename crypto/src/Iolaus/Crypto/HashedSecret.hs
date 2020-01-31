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

Encrypted values that are paired with their hashed digest.

-}
module Iolaus.Crypto.HashedSecret
  ( -- * Encrypting and Hashing
    HashedSecret(..)
  , toHashedSecret

    -- * Re-exports
  , module Iolaus.Crypto.SaltedHash
  , module Iolaus.Crypto.Secret
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary)
import Data.Profunctor.Product.Default (Default(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import GHC.Generics (Generic)

import Opaleye
  ( QueryRunnerColumnDefault(..)
  , Constant(..)
  , Column
  , SqlJsonb
  , fieldQueryRunnerColumn
  , sqlValueJSONB
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Control.Monad.Crypto.Class
import Iolaus.Crypto.Salt
import Iolaus.Crypto.SaltedHash
import Iolaus.Crypto.Secret

--------------------------------------------------------------------------------
-- | A convenience type for both encrypting and hashing a single
-- value.  Useful for when you need to protect a value with
-- encryption, but also need to use the hash for deterministic lookup,
-- i.e. fetching a record from a database.
data HashedSecret a = HashedSecret
  { hashedSecret    :: SaltedHash a
  , encryptedSecret :: Secret a
  } deriving (Generic)

--------------------------------------------------------------------------------
instance ToJSON (HashedSecret a) where
  toJSON HashedSecret{..} = Aeson.object
    [ "hashed"    .= hashedSecret
    , "encrypted" .= encryptedSecret
    ]

--------------------------------------------------------------------------------
instance FromJSON (HashedSecret a) where
  parseJSON = Aeson.withObject "HashedSecret" $ \v ->
    HashedSecret <$> v .: "hashed"
                 <*> v .: "encrypted"

--------------------------------------------------------------------------------
instance FromField (HashedSecret a) where
  fromField f b = go =<< fromField f b
    where
      go v = case Aeson.fromJSON v of
               Aeson.Success x -> pure x
               Aeson.Error e   -> fail e

instance QueryRunnerColumnDefault SqlJsonb (HashedSecret a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (HashedSecret a) (Column SqlJsonb) where
    def = Constant (sqlValueJSONB . Aeson.toJSON)

--------------------------------------------------------------------------------
-- | Encrypt and hash the given value.
toHashedSecret
  :: ( MonadCrypto k m
     , Binary a
     , ForSaltedHash a
     )
  => Key k       -- ^ The encryption key to use.
  -> SharedSalt  -- ^ The salt to use for hashing.
  -> a           -- ^ The value to protect.
  -> m (HashedSecret a)
toHashedSecret k s a = HashedSecret (saltedHash s a) <$> encrypt' k a
