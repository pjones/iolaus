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

Symmetric encryption tests.

-}
module Iolaus.Test.Crypto.Symmetric (run) where

--------------------------------------------------------------------------------
-- Library Imports:
import Crypto.Cipher.AES (AES256)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.IV (IV)
import qualified Iolaus.Crypto.IV as IV
import Iolaus.Crypto.Key (Key)
import qualified Iolaus.Crypto.Key as Key
import qualified Iolaus.Crypto.Symmetric as Symmetric

--------------------------------------------------------------------------------
-- | Main entry point.
run :: TestTree
run =
  testGroup "symmetric ciphers"
    [ testProperty "reversible" prop_reversible
    ]

--------------------------------------------------------------------------------
-- | Generate an IV.
genIV :: Gen (ByteString, IV AES256)
genIV = do
  bs <- Gen.bytes (Range.singleton 16)

  case IV.pack bs of
    Left _  -> fail "should never happen"
    Right x -> pure (bs, x)

--------------------------------------------------------------------------------
genKey :: Gen (ByteString, Key AES256)
genKey = do
  bs <- Gen.bytes (Range.singleton 32)

  case Key.convert $ Key.packBS bs of
    Left e  -> fail ("should never happen: " <> show e)
    Right x -> pure (bs, x)

--------------------------------------------------------------------------------
prop_reversible :: Property
prop_reversible =
  property $ do
    (_, iv)  <- forAllWith (show . fst) genIV
    (_, key) <- forAllWith (show . fst) genKey
    txt <- forAll $ Gen.text (Range.linear 0 255) Gen.unicode

    enc <- case Symmetric.encrypt' iv key txt of
      Left  e -> fail (show e)
      Right s -> pure s

    dec <- case Symmetric.decrypt key enc of
      Left  e -> fail (show e)
      Right s -> pure s

    dec === txt

    -- Also test JSON encoding/decoding.
    case Aeson.eitherDecode (Aeson.encode enc) of
      Left e     -> fail (show e)
      Right enc' -> enc === enc'
