{-# LANGUAGE FlexibleContexts  #-}
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
import Control.Monad.Except
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Package Imports:
import Control.Monad.Crypto.Cryptonite
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
    , testCase "multiple keys" testMultipleKeys
    ]

--------------------------------------------------------------------------------
type App a = CryptoniteT (ExceptT CryptoError IO) a

--------------------------------------------------------------------------------
runCrypto :: App a -> IO a
runCrypto = check <=< exec
  where
    check (Left e)  = assertFailure (show e)
    check (Right a) = return a

    exec m = do
      mgr <- memoryKeys
      ct  <- initCryptoniteT mgr
      runExceptT (runCryptoniteT' ct m)

--------------------------------------------------------------------------------
genericReversible :: App (ByteString, Secret ByteString, ByteString) -> Assertion
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
  msg <- generateRandomBytes 2048
  key <- generateKey AES256 (toLabel "key")
  e <- encrypt key msg
  d <- decrypt key e
  return (msg, e, d)

--------------------------------------------------------------------------------
testAReversible :: Assertion
testAReversible = genericReversible $ do
  msg <- generateRandomBytes 2048
  key <- generateKeyPair RSA4096 (toLabel "key")
  p <- toPublicKey key
  e <- asymmetricEncrypt p msg
  d <- asymmetricDecrypt key e
  return (msg, e, d)

--------------------------------------------------------------------------------
testPubKey :: Assertion
testPubKey = do
  let label = toLabel "key"
  key <- runCrypto (generateKeyPair RSA2048 label >>= toPublicKey)
  (decodePublicKey label $ encodePublicKey key) @?= Just key

--------------------------------------------------------------------------------
testSig :: Assertion
testSig = do
  status <- runCrypto $ do
    msg <- generateRandomBytes 2048
    key <- generateKeyPair RSA2048 (toLabel "key")
    pub <- toPublicKey key
    s   <- asymmetricSign key SHA2_512 msg
    verifySignature pub s msg

  status @?= SignatureVerified

--------------------------------------------------------------------------------
testMultipleKeys :: Assertion
testMultipleKeys = do
  let value = 42 :: Int

  (a, b) <- runCrypto $ do
    keyA <- generateKey AES256 (toLabel "Key A")
    keyB <- generateKey AES256 (toLabel "Key B")

    encA <- encrypt' keyA value
    decA <- decrypt' keyA encA

    -- Should fail to decode from Binary:
    decB <- catchError
              (decrypt' keyB encA >> return (value + 1))
              (const (return value))

    return (decA, decB)

  a @?= value
  b @?= value
