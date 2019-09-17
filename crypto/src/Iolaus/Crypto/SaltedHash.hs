{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}

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
  , saltedHash'
  , ForSaltedHash(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Profunctor.Product.Default (Default(def))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.ICU.Normalize (NormalizationMode(NFKC), normalize)
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , SqlText
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
newtype SaltedHash a = SaltedHash { getHash :: Text }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
instance FromField (SaltedHash a) where
  fromField f b = SaltedHash <$> fromField f b

instance QueryRunnerColumnDefault SqlText (SaltedHash a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (SaltedHash a) (Column SqlText) where
  def = Constant (toFields . getHash)

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