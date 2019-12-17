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
module Iolaus.Test.Crypto.Cryptonite (run) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.Aeson as Aeson
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto
import qualified Iolaus.Crypto.Monad as M
import Iolaus.Crypto.Cryptonite
import Iolaus.Test.Crypto.Keys

--------------------------------------------------------------------------------
-- | Main entry point.
run :: TestTree
run =
  testGroup "Cryptonite backend"
    [ testCase "symmetric reversible" testSReversible
    , testCase "asymmetric reversible" testAReversible
    , testCase "asymmetric signatures" testSig
    , testCase "asymmetric public keys" testPubKey
    ]

--------------------------------------------------------------------------------
runCrypto :: M.CryptoOpt (CryptoniteT IO) a -> IO a
runCrypto opt = do
  mgr <- memoryKeys
  ct  <- initCryptoniteT mgr
  Right x <- runCryptoniteT ct (M.liftCryptoOpt opt)
  return x

--------------------------------------------------------------------------------
genericReversible :: M.CryptoOpt (CryptoniteT IO) (ByteString, Secret ByteString, ByteString) -> Assertion
genericReversible opt = do
  (msg, enc, dec) <- runCrypto opt
  (secretBytes enc /= msg) @? "encrypted value should not match original"
  dec @?= msg

  -- Also test JSON encoding/decoding.
  case Aeson.eitherDecode (Aeson.encode enc) of
    Left e     -> fail (show e)
    Right enc' -> (enc == enc') @? "JSON should work"

--------------------------------------------------------------------------------
testSReversible :: Assertion
testSReversible = genericReversible $ do
  msg <- M.generateRandom 2048
  key <- M.generateKey AES256 (toLabel "key")
  e <- M.encrypt key msg
  d <- M.decrypt key e
  return (msg, e, d)

--------------------------------------------------------------------------------
testAReversible :: Assertion
testAReversible = genericReversible $ do
  msg <- M.generateRandom 2048
  key <- M.generateKeyPair RSA4096 (toLabel "key")
  p <- M.toPublicKey key
  e <- M.asymmetricEncrypt (toLabel "key") p msg
  d <- M.asymmetricDecrypt key e
  return (msg, e, d)

--------------------------------------------------------------------------------
testPubKey :: Assertion
testPubKey = do
  key <- runCrypto (M.generateKeyPair RSA2048 (toLabel "key") >>= M.toPublicKey)
  (decodePublicKey $ encodePublicKey key) @?= Just key

--------------------------------------------------------------------------------
testSig :: Assertion
testSig = do
  status <- runCrypto $ do
    msg <- M.generateRandom 2048
    key <- M.generateKeyPair RSA2048 (toLabel "key")
    pub <- M.toPublicKey key
    s   <- M.asymmetricSign key SHA2_512 msg
    M.verifySignature pub s msg

  status @?= SignatureVerified
