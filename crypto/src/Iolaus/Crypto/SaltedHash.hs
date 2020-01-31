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
import Control.Monad ((<=<))
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
import Iolaus.Crypto.Internal.Encoding (Encoding(..))
import qualified Iolaus.Crypto.Internal.Encoding as Encoding
import Iolaus.Crypto.Salt (Salt(..), SharedSalt(..))

--------------------------------------------------------------------------------
-- | A type that represents a salted and hashed value.
--
-- This is a simple wrapper around the SHA3 512 cryptographic hashing
-- function with the addition of a salt to make brute force guessing
-- more difficult.
newtype SaltedHash a = SaltedHash { getHash :: ByteString }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance ToJSON (SaltedHash a) where
  toJSON = toJSON . Encoding.encode . Encoding . getHash

instance FromJSON (SaltedHash a) where
  parseJSON = fmap (SaltedHash . getBytes) .
    (Encoding.decodeM <=< Aeson.parseJSON)

--------------------------------------------------------------------------------
instance FromField (SaltedHash a) where
  fromField f b = SaltedHash <$> fromField f b

instance QueryRunnerColumnDefault SqlBytea (SaltedHash a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (SaltedHash a) (Column SqlBytea) where
  def = Constant (toFields . getHash)

--------------------------------------------------------------------------------
-- | Types that can be salted and hashed.
--
-- While it might seem that this class could be replaced with
-- 'Binary', there's a very important difference when it comes to
-- hashing 'Text' values.
--
-- In order for the process to be deterministic, 'Text' values must go
-- through a normalization process (NFKC) before being hashed.  That's
-- precisely what the 'Text' instance does.
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
