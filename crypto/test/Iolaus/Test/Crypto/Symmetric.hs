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

Symmetric encryption tests.

-}
module Iolaus.Test.Crypto.Symmetric (run) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.Aeson as Aeson
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto
import Iolaus.Crypto.Cryptonite
import Iolaus.Test.Crypto.Keys

--------------------------------------------------------------------------------
-- | Main entry point.
run :: TestTree
run =
  testGroup "symmetric ciphers"
    [ testProperty "reversible" prop_reversible
    ]

--------------------------------------------------------------------------------
prop_reversible :: Property
prop_reversible =
  property $ do
    txt <- forAll $ Gen.text (Range.linear 0 255) Gen.unicode
    mgr <- memoryKeys
    ct  <- initCryptoniteT mgr

    Right (enc, dec) <- runCryptoniteT ct $ do
      key <- generateKey (toLabel "key")
      e <- encrypt key txt
      d <- decrypt key e
      return (e, d)

    dec === txt

    -- Also test JSON encoding/decoding.
    case Aeson.eitherDecode (Aeson.encode enc) of
      Left e     -> fail (show e)
      Right enc' -> assert (enc == enc')
