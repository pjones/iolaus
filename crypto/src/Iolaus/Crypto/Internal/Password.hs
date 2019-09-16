{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}

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

Core password management types an functions.

-}
module Iolaus.Crypto.Internal.Password
  ( Password
  , Clear
  , Hashed
  , password
  , hash
  , hash'
  , VerifyStatus(..)
  , verify
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Hash.Algorithms (SHA3_512(..))
import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.Random (MonadRandom)
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Profunctor.Product.Default (Default(..))
import Data.String (IsString(..))
import Data.Text (Text, strip)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.ICU.Normalize (NormalizationMode(NFKC), normalize)
import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import GHC.Generics (Generic)

import Opaleye
  ( QueryRunnerColumnDefault(..)
  , Constant(..)
  , Column
  , PGJson
  , fieldQueryRunnerColumn
  , pgValueJSON
  )

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import Iolaus.Crypto.Salt (Salt(..), SharedSalt(..))
import qualified Iolaus.Crypto.Salt as Salt
import Iolaus.Crypto.Password.Settings (Settings(..))
import qualified Iolaus.Crypto.Password.Settings as Settings

--------------------------------------------------------------------------------
-- | Information about what hashing algorithm was used and how it was
--   configured.
data HashType = PBKDF2_HS3512 Settings Salt
              | LegacyBCrypt
              deriving (Generic, Eq, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- | Type representing insecure (plain) passwords.
newtype Clear = Clear ()

--------------------------------------------------------------------------------
-- | Type representing secure (hashed) passwords.
newtype Hashed = Hashed HashType deriving Show

--------------------------------------------------------------------------------
-- | A password is any shared secret that will take on two forms:
--
--     * Clear: A plain-text secret entered by a user for
--       confirmation.  This type of password should never be written
--       to disk or included in logging output.
--
--     * Hashed: A cryptographicly hashed secret that is safe to store.
--
--   Clear passwords can be created with the 'password' function or
--   using the 'IsString' instance and the @OverloadedStrings@
--   extension to GHC.
--
--   Hashed passwords are created from clear passwords using a 'Salt'
--   and the 'hash' function.  Once a password is hashed it can be
--   safely converted to JSON and stored in a file or database.
data Password a = Password a ByteString
deriving instance Show (Password Hashed)

--------------------------------------------------------------------------------
instance IsString (Password Clear) where
  fromString = password . fromString

--------------------------------------------------------------------------------
instance Eq (Password Clear) where
  (==) (Password (Clear ()) x) (Password (Clear ()) y) = x == y

-- | Allow hashed passwords to be compared for equality.
instance Eq (Password Hashed) where
  (==) (Password (Hashed x) z) (Password (Hashed y) w) = x == y && z == w

--------------------------------------------------------------------------------
instance ToJSON (Password Hashed) where
  toJSON (Password (Hashed ht) bs) =
    Aeson.object [ "type" .= ht
                 , "hash" .= Encoding bs
                 ]

--------------------------------------------------------------------------------
instance FromJSON (Password Hashed) where
  parseJSON (Aeson.Object v) =
    Password <$> (Hashed   <$> v .: "type")
             <*> (getBytes <$> v .: "hash")
  parseJSON invalid = Aeson.typeMismatch "Password" invalid

--------------------------------------------------------------------------------
instance FromField (Password Hashed) where
  fromField = fromJSONField

instance QueryRunnerColumnDefault PGJson (Password Hashed) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (Password Hashed) (Column PGJson) where
  def = Constant (pgValueJSON . Aeson.toJSON)

--------------------------------------------------------------------------------
-- | Construct an insecure password.  This type of password has no
-- @Show@ instance or any other way to store it to intentionally keep
-- it from being accidentally exposed.
--
-- This function can safely work with Unicode text.  To ensure that a
-- password can be verifyied in the future the text is normalize using
-- [NFKC](https://en.wikipedia.org/wiki/Unicode_equivalence) before
-- being converted into UTF-8.
--
-- Note: leading and trailing white space is removed from the password
-- to avoid unnecessary errors caused by human input mistakes.  The
-- content of the password is not modified or truncated in any other
-- way.
password :: Text -> Password Clear
password = Password clear . encodeUtf8 . normalize NFKC . strip
  where clear = Clear ()

--------------------------------------------------------------------------------
-- | Secure a password for storage using PBKDF2-HMAC-SHA3-512.
--
-- To create a secure password we send it through two rounds of
-- hashing:
--
--   1. Hash the password using a generated salt.  The salt will be
--      stored with the password so this process can be repeated for
--      verification.
--
--   2. Hash the result of the last step, this time using a /shared/
--      salt.  The shared salt should /not/ be stored with the
--      password and should be kept out of the database and preferably
--      in a hardward device.
--
-- If your data store of passwords is ever compromised the second
-- round of hashing makes it more difficult for an attacker to match
-- plain text guesses to the stored password hashes.
--
-- For more information see section 5.1.1.2 of the NIST Special
-- Publication 800-63B.
hash ::
  ( MonadRandom m
  ) => SharedSalt      -- ^ A salt shared across an entire application.
    -> Settings        -- ^ Hashing settings.
    -> Password Clear  -- ^ The clear password to hash.
    -> m (Password Hashed)
hash ss settings clear = do
  salt <- Salt.generate
  pure (hash' ss salt settings clear)

--------------------------------------------------------------------------------
-- | A pure hashing function that requires you to provide the salt.
--   See 'hash' for documentation.
hash' :: SharedSalt -> Salt -> Settings -> Password Clear -> Password Hashed
hash' (SharedSalt ss) salt settings (Password _ clear) =
  Password (Hashed ht) round2

  where
    -- Round 1 of hashing, clear password with salt:
    round1 = go salt clear

    -- Round 2 of hashing, hashed password with shared salt:
    round2 = go ss round1

    -- The PRF we are using:
    prf = PBKDF2.prfHMAC SHA3_512

    -- Record the type of hash that was applied.
    ht = PBKDF2_HS3512 settings salt

    -- A single round of hashing.
    go :: Salt -> ByteString -> ByteString
    go s p = PBKDF2.generate prf (Settings.forPBKDF2 settings) p (getSalt s)

--------------------------------------------------------------------------------
-- | When verifying that a clear password entered from a user matches
--   a stored (hashed) password, this data type tells you if there is
--   a match and whether or not the hashed version needs to be
--   upgraded (older algorithm or lower iteration count).
data VerifyStatus = Mismatch
                    -- ^ Clear and hashed passwords don't match.  The
                    -- user has entered the wrong password and
                    -- authentication should fail.

                  | NeedsUpgrade
                    -- ^ The hashed password needs to be upgraded.
                    -- The hashed password may be using weaker
                    -- settings than what the system is currently
                    -- configured to use or the salt stored in the
                    -- password may be shorter than the recommened
                    -- value.  To upgrade the hashed password run the
                    -- clear password through the 'hash' function and
                    -- store the result.

                  | Match
                    -- ^ Passwords match and all is good.
                    -- Authentication should succeed.
                  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Verify that a clear password entered from a user matches a stored
--   (hashed) password.
verify :: SharedSalt       -- ^ Salt shared by all passwords.
       -> Settings         -- ^ Settings for password hashing.
       -> Password Clear   -- ^ The clear password from the user.
       -> Password Hashed  -- ^ The hashed password stored in the database.
       -> VerifyStatus     -- ^ Status.
verify ss settings clear hashed@(Password (Hashed ht) bs) =
  case ht of
    PBKDF2_HS3512 settings' salt ->
      let settingsOkay = settings' >= settings
          saltOkay     = ByteString.length (getSalt salt) >= Salt.recommended
      in if hash' ss salt settings clear == hashed
         then bool NeedsUpgrade Match (settingsOkay && saltOkay)
         else Mismatch

    LegacyBCrypt ->
      let (Password _ c) = clear
      in if BCrypt.validatePassword c bs
            then NeedsUpgrade
            else Mismatch
