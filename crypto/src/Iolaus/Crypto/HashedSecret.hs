{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
module Iolaus.Crypto.HashedSecret
  ( HashedSecret(..)
  , hashedSecret
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Cipher.Types as Cryptonite
import Crypto.Random (MonadRandom(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
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
import Iolaus.Crypto.SaltedHash
import Iolaus.Crypto.Key
import Iolaus.Crypto.Salt
import Iolaus.Crypto.Error (CryptoError)
import Iolaus.Crypto.Symmetric

--------------------------------------------------------------------------------
-- | A convenience type for encrypting and hashing for when you need
-- to both encrypt some data, but also need a deterministic look up
-- key.
data HashedSecret c a = HashedSecret
  { hashed    :: SaltedHash a
  , encrypted :: Secret c a
  } deriving (Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------
instance (Cryptonite.Cipher c) => FromField (HashedSecret c a) where
  fromField f b = go =<< fromField f b
    where
      go v = case Aeson.fromJSON v of
               Aeson.Success x -> pure x
               Aeson.Error e   -> fail e

instance (Cryptonite.Cipher c) =>
  QueryRunnerColumnDefault SqlJsonb (HashedSecret c a) where
    queryRunnerColumnDefault = fieldQueryRunnerColumn

instance (Cryptonite.Cipher c) =>
  Default Constant (HashedSecret c a) (Column SqlJsonb) where
    def = Constant (sqlValueJSONB . Aeson.toJSON)

--------------------------------------------------------------------------------
-- | Encrypt and hash the given value.
hashedSecret
  :: ( MonadRandom m
     , Cryptonite.BlockCipher c
     , Binary a
     , ForSaltedHash a
     )
  => Key c       -- ^ The encryption key to use.
  -> SharedSalt  -- ^ The salt to use for hashing.
  -> a           -- ^ The value to protect.
  -> m (Either CryptoError (HashedSecret c a))
hashedSecret k s a = do
  e <- encrypt k a

  pure $ HashedSecret
    <$> pure (saltedHash s a)
    <*> e
