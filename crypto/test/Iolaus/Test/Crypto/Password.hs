{-# LANGUAGE OverloadedStrings #-}

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

Password tests.

-}
module Iolaus.Test.Crypto.Password
  ( run
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.Aeson as Aeson
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Time.Calendar as Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog
import qualified Text.Password.Strength.Config as Zxcvbn

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto

--------------------------------------------------------------------------------
-- | Main entry point.
run :: TestTree
run =
  testGroup "passwords"
    [ testProperty "hash idempotent" prop_hash_idempotent
    , testProperty "hash mismatch"   prop_hash_mismatch
    , testProperty "hash serialize"  prop_hash_serialize
    ]

--------------------------------------------------------------------------------
salt :: (Monad m) => ByteString -> m Salt
salt = either (const $ fail "should not happen") pure . toSalt

--------------------------------------------------------------------------------
-- | Random password generator.
genClearPassword :: Gen Text
genClearPassword = Gen.text (Range.linear 6 255) Gen.unicode

--------------------------------------------------------------------------------
-- | Salt generator.
genSalt :: Gen Salt
genSalt = do
  let minLen = recommendedSaltLen -- Anything less triggers NeedsUpgrade.
  Gen.bytes (Range.linear minLen (minLen * 2)) >>= salt

--------------------------------------------------------------------------------
-- | Hashing multiple times produces the same value.
prop_hash_idempotent :: Property
prop_hash_idempotent =
  property $ do
    password <- forAll genClearPassword
    strong   <- mkStrong password
    saltP    <- forAll genSalt
    saltS    <- forAll (SharedSalt <$> genSalt)

    let settings = defaultSettings
        clear  = toPassword password
        hashed = toHashedPassword' saltS saltP settings strong
        status = verifyPassword saltS settings clear hashed

    status === PasswordsMatch

--------------------------------------------------------------------------------
-- | Give me some peace knowing that mismatching passwords are detected.
prop_hash_mismatch :: Property
prop_hash_mismatch =
  property $ do
    password <- forAll genClearPassword
    strong <- mkStrong password
    saltP <- salt "abcdefghijk"
    saltS <- SharedSalt <$> salt "1234567890"

    let settings = defaultSettings
        clearB   = toPassword (password <> "-extra")
        hashed = toHashedPassword' saltS saltP settings strong
        status = verifyPassword saltS settings clearB hashed

    status === PasswordMismatch

--------------------------------------------------------------------------------
-- | JSON encoding and decoding.
prop_hash_serialize :: Property
prop_hash_serialize =
  property $ do
    password <- forAll genClearPassword
    strong <- mkStrong password
    saltP <- salt "abcdefghijk"
    saltS <- SharedSalt <$> salt "1234567890"

    let settings = defaultSettings
        hashed = toHashedPassword' saltS saltP settings strong

    tripping hashed Aeson.encode Aeson.decode

--------------------------------------------------------------------------------
-- | Converts a clear password to a strong password.  This can fail if
-- the randomly generated password is consider weak.  I think that
-- should be extremely rare so until it becomes a problem I'm keeping
-- things the way they are.
mkStrong :: (Monad m) => Text -> m (Password Strong)
mkStrong t =
  case toStrongPassword cfg day clear of
    Left e  -> fail (show e <> " " <> show t)
    Right p -> pure p

  where
    clear :: Password Clear
    clear = toPassword t

    day :: Time.Day
    day = Time.fromGregorian 2019 1 1

    cfg :: Zxcvbn.Config
    cfg = mempty
