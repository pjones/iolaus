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
module Iolaus.Crypto.Password
  ( Password
  , Clear
  , Strong
  , Hashed
  , Settings(..)
  , defaultSettings
  , toPassword
  , toStrongPassword
  , toHashedPassword
  , toHashedPassword'
  , VerifyStatus(..)
  , verifyPassword
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Hash.Algorithms (SHA3_512(..))
import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Bool (bool)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Profunctor.Product.Default (Default(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.FromField (FromField(..), fromJSONField)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import qualified Text.Password.Strength as Zxcvbn
import qualified Text.Password.Strength.Config as Zxcvbn

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
import Control.Monad.Crypto
import Iolaus.Crypto.Encoding (Encoding(..), normalize)
import Iolaus.Crypto.Salt

--------------------------------------------------------------------------------
-- | Settings to control the password hashing process.
data Settings = Settings
  { iterations :: Natural -- ^ Number of iterations for the algorithm.
  , bytes      :: Natural -- ^ Size of the output in bytes.
  }
  deriving stock    (Generic, Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- | Default settings.
--
-- These are based on the recommendations in the NIST Special
-- Publication 800-63B.  Attempting to use values weaker than the
-- defaults will cause the hashing function to silently upgrade to the
-- values given by this function.
defaultSettings :: Settings
defaultSettings =
  Settings { iterations = 10000 -- As per NIST 800-63b
           , bytes      = 32    -- 256 bits
           }

--------------------------------------------------------------------------------
-- | Convert the settings into PBKDF2 parameters.
forPBKDF2 :: Settings -> PBKDF2.Parameters
forPBKDF2 s =
  PBKDF2.Parameters { PBKDF2.iterCounts   = fromIntegral iterations'
                    , PBKDF2.outputLength = fromIntegral bytes'
                    }

  where
    -- Disallow going lower than the default values.
    iterations' = max (iterations s) (iterations defaultSettings)
    bytes'      = max (bytes s)      (bytes defaultSettings)

--------------------------------------------------------------------------------
-- | Information about what hashing algorithm was used and how it was
--   configured.
data HashType
  = PBKDF2_HS3512 Settings Salt
  | LegacyBCrypt
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- | Type representing insecure (plain) passwords.
newtype Clear = Clear Text

--------------------------------------------------------------------------------
-- | Type representing a clear password that has been verified to not be weak.
newtype Strong = Strong ()

--------------------------------------------------------------------------------
-- | Type representing secure (hashed) passwords.
newtype Hashed = Hashed HashType deriving Show

--------------------------------------------------------------------------------
-- | A password is any shared secret that can be in one of three states:
--
--     * Clear: A plain-text secret entered by a user for
--       confirmation.  This type of password should never be written
--       to disk or included in logging output.
--
--     * Strong: Also a plain-text secret, but verified to not be weak.
--
--     * Hashed: A cryptographicly hashed secret that is safe to store.
--
--   Clear passwords can be created with the 'toPassword' function or
--   using the 'IsString' instance and the @OverloadedStrings@
--   extension to GHC.
--
--   Strong passwords are created from clear passwords using the
--   'toStrongPassword' function.
--
--   Hashed passwords are created from strong passwords using a 'Salt'
--   and the 'toHashedPassword' function.  Once a password is hashed
--   it can be safely converted to JSON and stored in a file or
--   database.
data Password a = Password a ByteString
deriving instance Show (Password Hashed)

--------------------------------------------------------------------------------
instance IsString (Password Clear) where
  fromString = toPassword . fromString

--------------------------------------------------------------------------------
instance Eq (Password Clear) where
  (==) (Password (Clear _) x) (Password (Clear _) y) = x == y

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
  parseJSON = Aeson.withObject "Password" $ \v ->
    Password <$> (Hashed   <$> v .: "type")
             <*> (getBytes <$> v .: "hash")

--------------------------------------------------------------------------------
instance FromField (Password Hashed) where
  fromField = fromJSONField

instance QueryRunnerColumnDefault SqlJsonb (Password Hashed) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant (Password Hashed) (Column SqlJsonb) where
  def = Constant (sqlValueJSONB . Aeson.toJSON)

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
toPassword :: Text -> Password Clear
toPassword t = Password (Clear t) (normalize t)

--------------------------------------------------------------------------------
-- | Determine the strength of a password using the zxcvbn algorithm
-- and possibly return a strong password.
toStrongPassword
  :: Zxcvbn.Config
     -- ^ Configuration for the zxcvbn algorithm.

  -> Day
     -- ^ A reference day for detecting dates in passwords.  This
     -- should be the current day.

  -> Password Clear
     -- ^ The password to verify.

  -> Either Zxcvbn.Score (Password Strong)
     -- ^ If the password is weak, return its score.  Otherwise return
     -- the password marked as being 'Strong'.

toStrongPassword cfg day (Password (Clear t) bs) =
  let score = Zxcvbn.score cfg day t
  in if Zxcvbn.strength score >= Zxcvbn.Moderate
       then Right (Password (Strong ()) bs)
       else Left score

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
toHashedPassword
  :: (MonadCrypto k m)
  => SharedSalt      -- ^ A salt shared across an entire application.
  -> Settings        -- ^ Hashing settings.
  -> Password Strong -- ^ The strong password to hash.
  -> m (Password Hashed)
toHashedPassword ss settings clear = do
  salt <- generateSalt
  pure (toHashedPassword' ss salt settings clear)

--------------------------------------------------------------------------------
-- | A pure hashing function that requires you to provide the salt.
--   See 'toHashedPassword' for documentation.
toHashedPassword' :: SharedSalt -> Salt -> Settings -> Password Strong -> Password Hashed
toHashedPassword' (SharedSalt ss) salt settings (Password _ clear) =
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
    go s p = PBKDF2.generate prf (forPBKDF2 settings) p (getSalt s)

--------------------------------------------------------------------------------
-- | When verifying that a clear password entered from a user matches
--   a stored (hashed) password, this data type tells you if there is
--   a match and whether or not the hashed version needs to be
--   upgraded (older algorithm or lower iteration count).
data VerifyStatus
  = PasswordMismatch
    -- ^ Clear and hashed passwords don't match.  The user has entered
    -- the wrong password and authentication should fail.

  | PasswordNeedsUpgrade
    -- ^ The hashed password needs to be upgraded.  The hashed
    -- password may be using weaker settings than what the system is
    -- currently configured to use or the salt stored in the password
    -- may be shorter than the recommend value.  To upgrade the hashed
    -- password run the clear password through the 'toHashedPassword'
    -- function and store the result.

  | PasswordsMatch
    -- ^ Passwords match and all is good.  Authentication should
    -- succeed.

  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Verify that a clear password entered from a user matches a stored
--   (hashed) password.
verifyPassword
  :: SharedSalt       -- ^ Salt shared by all passwords.
  -> Settings         -- ^ Settings for password hashing.
  -> Password Clear   -- ^ The clear password from the user.
  -> Password Hashed  -- ^ The hashed password stored in the database.
  -> VerifyStatus     -- ^ Status.
verifyPassword ss settings clear hashed@(Password (Hashed ht) bs) =
  case ht of
    PBKDF2_HS3512 settings' salt ->
      let settingsOkay = settings' >= settings
          saltOkay     = ByteString.length (getSalt salt) >= recommendedSaltLen
      in if toHashedPassword' ss salt settings strong == hashed
         then bool PasswordNeedsUpgrade PasswordsMatch (settingsOkay && saltOkay)
         else PasswordMismatch

    LegacyBCrypt ->
      let (Password _ c) = clear
      in if BCrypt.validatePassword c bs
            then PasswordNeedsUpgrade
            else PasswordMismatch
  where
    strong :: Password Strong
    strong =
      let (Password _ c) = clear
      in Password (Strong ()) c
