{-# LANGUAGE MultiParamTypeClasses #-}

{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    git://code.devalot.com/sthenauth.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Salted hashes are a way of generating deterministic lookup keys.  For
example, storing an email address in the database as a SHA3 hash
instead of the raw text.  But in this case the hash is salted with the
system-wide salt to mitigate brute-force attacks against the database.

-}
module Sthenauth.Crypto.SaltedHash
  ( SaltedHash
  , saltedHash
  , saltedHash'
  , ForSaltedHash(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.ICU.Normalize (NormalizationMode(NFKC), normalize)
import qualified Database.Beam as Beam
import qualified Database.Beam.Backend.SQL as Beam
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgValueSyntax)

--------------------------------------------------------------------------------
-- Project Imports:
import Sthenauth.Crypto.Encoding (Encoding(..))
import qualified Sthenauth.Crypto.Encoding as Encoding
import Sthenauth.Crypto.Salt (Salt(..), SharedSalt(..))

--------------------------------------------------------------------------------
-- | A type that represents a salted and hashed value.
newtype SaltedHash a = SaltedHash { getHash :: Text }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance Beam.FromBackendRow Postgres (SaltedHash a) where
  fromBackendRow = SaltedHash <$> Beam.fromBackendRow

--------------------------------------------------------------------------------
instance Beam.HasSqlValueSyntax PgValueSyntax (SaltedHash a) where
  sqlValueSyntax = Beam.sqlValueSyntax . getHash

--------------------------------------------------------------------------------
-- | Types that can be salted and hashed.
class ForSaltedHash a where
  -- | Unicode text representing a value of type @a@.
  forSaltedHash :: a -> Text

instance ForSaltedHash Text where
  forSaltedHash = id

--------------------------------------------------------------------------------
-- | Generate a cryptographic hash of the input type after running it
-- through a Unicode normalization process (NFKC) and salting it.
saltedHash :: (ForSaltedHash a) => SharedSalt -> a -> SaltedHash a
saltedHash salt =
  SaltedHash
    . getHash
    . saltedHash' salt
    . encodeUtf8
    . normalize NFKC
    . forSaltedHash

--------------------------------------------------------------------------------
-- | Specialized version of 'saltedHash' for 'ByteString' values.
saltedHash' :: SharedSalt -> ByteString -> SaltedHash ByteString
saltedHash' (SharedSalt salt) =
  SaltedHash
    . Encoding.encode
    . Encoding
    . ByteString.pack
    . ByteArray.unpack
    . hash
  where
    hash :: ByteString -> Hash.Digest Hash.SHA3_512
    hash = Hash.hash . (getSalt salt <>)
