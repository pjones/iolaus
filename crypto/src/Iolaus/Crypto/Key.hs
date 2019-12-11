{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}

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

Types for selecting ciphers at compile time.

-}
module Iolaus.Crypto.Key
  ( Cipher(..)
  , Algo(..)
  , PublicKey(..)
  , Label(..)
  , toLabel
  , getLabelText
  , KeyManager(..)
  , GetStatus(..)
  , PutStatus(..)
  , fileManager
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Codec.Binary.Base32 as Base32
import Control.Exception.Safe (tryIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import System.Directory (doesPathExist)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Iolaus.Crypto.Encoding as Encoding

--------------------------------------------------------------------------------
newtype PublicKey (c :: Algo) = PublicKey ByteString

--------------------------------------------------------------------------------
-- | A label is a way to identify a key by a name.
--
-- When used with a hardware security module, the label is used to
-- find the hardware slot containing the key.
--
-- For keys stored on the file system, the label translates to a file
-- name.
--
-- To ensure the label is safe to use in hardware devices and as file
-- names the underlying text is normalized and then base32 encoded.
newtype Label = Label
  { getLabel :: CBS.ByteString -- ^ Access the encoded label.
  }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Create a label from a 'Text' value.
toLabel :: Text -> Label
toLabel = Label . Base32.encode . Encoding.normalize

--------------------------------------------------------------------------------
-- | The inverse of 'toLabel'.
getLabelText :: Label -> Text
getLabelText = decodeUtf8 . check . Base32.decode . getLabel
  where
    check :: Either (ByteString, ByteString) ByteString -> ByteString
    check (Left (bs, _)) = bs
    check (Right bs)     = bs

--------------------------------------------------------------------------------
-- | Supported symmetric ciphers.
data Cipher
  = AES256 -- ^ https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

  -- NOTE: If you add another constructor, you *must* update the
  -- following type families, the compiler will not detect
  -- non-exhaustiveness.
  --
  --   * ToBlockCipher (in Iolaus.Crypto.Cryptonite)
  deriving (Generic, Eq, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | Supported asymmetric algorithms.
data Algo
  = RSA4096 -- ^ https://en.wikipedia.org/wiki/RSA_(cryptosystem)
  deriving (Generic, Eq, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | The result of fetching a key with a 'KeyManager'.
data GetStatus
  = GetSucceeded ByteString -- ^ The key was successfully retrieved.
  | GetFailed               -- ^ The key could not be found.

--------------------------------------------------------------------------------
-- | The result of storing a key with a 'KeyManager'.
data PutStatus
  = PutSucceeded  -- ^ The key was stored successfully.
  | PutKeyExists  -- ^ A key with the requested label already exists.
  | PutFailed     -- ^ Failed to store the key.

--------------------------------------------------------------------------------
-- | Used by the 'CryptoniteT' backend to store and retrieve keys.
data KeyManager = KeyManager
  { managerGetKey :: Label -> IO GetStatus
    -- ^ A function that can fetch keys on demand.

  , managerPutKey :: Label -> ByteString -> IO PutStatus
    -- ^ A function that can store keys on demand.
  }

--------------------------------------------------------------------------------
-- | A 'KeyManager' for the 'CryptoniteT' backend that stores keys on
-- the file system.
fileManager
  :: FilePath   -- ^ The directory where keys will be stored.
  -> KeyManager
fileManager dir = KeyManager get put

  where
    get :: Label -> IO GetStatus
    get label = do
      r <- tryIO (ByteString.readFile (path label))
      return (either (const GetFailed) GetSucceeded r)

    put :: Label -> ByteString -> IO PutStatus
    put label bs = do
      let file = path label
      exists <- doesPathExist file
      if exists
        then return PutKeyExists
        else do
          r <- tryIO (ByteString.writeFile (path label) bs)
          return (either (const PutFailed) (const PutSucceeded) r)

    path :: Label -> FilePath
    path (Label file) = dir </> CBS.unpack file
