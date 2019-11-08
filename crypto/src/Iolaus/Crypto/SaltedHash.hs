{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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

Salted hashes are a way of generating deterministic lookup keys.  For
example, storing an email address in the database as a SHA3 hash
instead of the raw text.  But in this case the hash is salted with the
system-wide salt to mitigate brute-force attacks against the database.

-}
module Iolaus.Crypto.SaltedHash
  ( SaltedHash
  , saltedHash
  , ForSaltedHash(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Hash as Hash
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Profunctor.Product.Default (Default(def))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , SqlBytea
  , fieldQueryRunnerColumn
  , toFields
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Salt (Salt(..), SharedSalt(..))

--------------------------------------------------------------------------------
-- | A type that represents a salted and hashed value.
newtype SaltedHash a = SaltedHash { getHash :: ByteString }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance ToJSON (SaltedHash a) where
  toJSON = toJSON . Encoding.encode . Encoding . getHash

instance FromJSON (SaltedHash a) where
  parseJSON = fmap (SaltedHash . getBytes . Encoding.decode) . Aeson.parseJSON

--------------------------------------------------------------------------------
instance FromField (SaltedHash a) where
  fromField f b = SaltedHash <$> fromField f b

instance QueryRunnerColumnDefault SqlBytea (SaltedHash a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (SaltedHash a) (Column SqlBytea) where
  def = Constant (toFields . getHash)

--------------------------------------------------------------------------------
-- | Types that can be salted and hashed.
class ForSaltedHash a where
  -- | Convert to a 'ByteString' prior to hashing.
  forSaltedHash :: a -> ByteString

instance ForSaltedHash ByteString where
  forSaltedHash = id

instance ForSaltedHash Text where
  forSaltedHash = Encoding.normalize

--------------------------------------------------------------------------------
-- | Generate a cryptographic hash of the input type after running it
-- through a Unicode normalization process (NFKC) and salting it.
saltedHash :: (ForSaltedHash a) => SharedSalt -> a -> SaltedHash a
saltedHash (SharedSalt salt) =
  SaltedHash
    . ByteString.pack
    . ByteArray.unpack
    . hash
    . forSaltedHash
  where
    hash :: ByteString -> Hash.Digest Hash.SHA3_512
    hash = Hash.hash . (getSalt salt <>)
