{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

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
  , Hash(..)
  , toX509HashAlg
  , toX509SigAlg
  , decodeBinaryKey
  , PublicKey(..)
  , toX509PubKey
  , encodePublicKey
  , decodePublicKey
  , Label(..)
  , toLabel
  , KeyManager(..)
  , FileExtension(..)
  , GetStatus(..)
  , PutStatus(..)
  , fileManager
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import Control.Exception.Safe (tryIO)
import Control.Monad ((<=<))
import qualified Crypto.Hash as SHA1
import Crypto.Hash.Algorithms (SHA1)
import qualified Crypto.PubKey.RSA as RSA
import Data.Aeson (ToJSON, FromJSON)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.ByteArray as Memory
import qualified Data.ByteArray.Encoding as Base
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.X509 as X509
import GHC.Generics (Generic)
import System.Directory (doesPathExist)
import System.FilePath ((</>))
import System.PosixCompat.Files (setFileMode)

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error
import Iolaus.Crypto.PEM

--------------------------------------------------------------------------------
-- | The public key portion of a key pair.
--
-- To create a public key from a PEM encoded 'ByteString', use the
-- 'decodePublicKey' function.
--
-- To store a public key in a file or transmit it over the network,
-- use the 'encodePublicKey' function.
newtype PublicKey = RSAPubKey (Algo, RSA.PublicKey)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Convert a 'PublicKey' to one that can be used in X509
-- certificates.
toX509PubKey :: PublicKey -> X509.PubKey
toX509PubKey (RSAPubKey (_, k)) = X509.PubKeyRSA k

--------------------------------------------------------------------------------
-- | Convert a 'X509.PubKey' to a 'PublicKey'.
fromX509PubKey :: X509.PubKey -> Maybe PublicKey
fromX509PubKey = \case
  X509.PubKeyRSA k@(RSA.PublicKey n _ _)
    | n == 256  -> Just (RSAPubKey (RSA2048, k))
    | n == 512  -> Just (RSAPubKey (RSA4096, k))
    | otherwise -> Nothing
  _ -> Nothing

--------------------------------------------------------------------------------
-- | Encode a public key in standard PEM format.
encodePublicKey :: PublicKey -> LBS.ByteString
encodePublicKey = encodePEM . pure . toPEM PublicKeySection . toX509PubKey

--------------------------------------------------------------------------------
-- | Decode a public key that was encoded in PEM.  The first usable
-- key found in the PEM stream is returned.
decodePublicKey :: LBS.ByteString -> Maybe PublicKey
decodePublicKey = (fromX509PubKey <=< listToMaybe) . concatMap fromPEM . decodePEM

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
-- names the underlying text is normalized, hashed, and then base32
-- encoded.
data Label = Label
  { getLabel     :: CBS.ByteString -- ^ Access the encoded label.
  , getLabelText :: Text           -- ^ The inverse of 'toLabel'.
  } deriving (Generic, Eq, Ord, Binary, Show)

--------------------------------------------------------------------------------
-- | Create a label from a 'Text' value.
toLabel :: Text -> Label
toLabel t = Label (encode t) t
  where
    encode =
      Base.convertToBase Base.Base32 .
      (Memory.convert :: SHA1.Digest SHA1 -> ByteString) .
      (SHA1.hash :: ByteString -> SHA1.Digest SHA1) .
      Encoding.normalize

--------------------------------------------------------------------------------
-- | Supported symmetric ciphers.
--
-- Note: AES ciphers are used in GCM mode.
data Cipher
  = AES128 -- ^ https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
  | AES192 -- ^ https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
  | AES256 -- ^ https://en.wikipedia.org/wiki/Advanced_Encryption_Standard
  deriving (Generic, Eq, Ord, Show, Binary, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | Supported asymmetric algorithms.
data Algo
  = RSA2048 -- ^ https://en.wikipedia.org/wiki/RSA_(cryptosystem)
  | RSA4096 -- ^ https://en.wikipedia.org/wiki/RSA_(cryptosystem)
  deriving (Generic, Eq, Ord, Show, Binary, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | Supported hashing algorithms.
data Hash
  = SHA2_256 -- ^ https://en.wikipedia.org/wiki/Secure_Hash_Algorithms
  | SHA2_384 -- ^ https://en.wikipedia.org/wiki/Secure_Hash_Algorithms
  | SHA2_512 -- ^ https://en.wikipedia.org/wiki/Secure_Hash_Algorithms
  deriving (Generic, Eq, Ord, Show, Binary, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- | Convert a 'Hash' value to a X509 'X509.HashALG'.
toX509HashAlg :: Hash -> X509.HashALG
toX509HashAlg = \case
  SHA2_256 -> X509.HashSHA256
  SHA2_384 -> X509.HashSHA384
  SHA2_512 -> X509.HashSHA512

--------------------------------------------------------------------------------
-- | Convert a 'Hash' and 'Algo' to a X509 'X509.SignatureALG'.
toX509SigAlg :: Hash -> Algo -> X509.SignatureALG
toX509SigAlg hash = \case
    RSA2048 -> rsa
    RSA4096 -> rsa
  where
    rsa = X509.SignatureALG (toX509HashAlg hash) X509.PubKeyALG_RSA

--------------------------------------------------------------------------------
decodeBinaryKey :: (Binary a) => Label -> ByteString -> Either CryptoError a
decodeBinaryKey label bs =
  case Binary.decodeOrFail (LBS.fromStrict bs) of
    Left _ -> Left (KeyReadFailure (getLabelText label))
    Right (bs', _, k)
      | LBS.null bs' -> Right k
      | otherwise -> Left (KeyReadFailure (getLabelText label))

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
-- | Allow for duplicate keys in a 'KeyManager' by using file
-- extensions.  For example, this allows private and public keys with
-- the same 'Label' to be stored next to one another in the file
-- system.
data FileExtension
  = KeyExt                      -- ^ Symmetric key.
  | PrivateExt                  -- ^ Asymmetric private key.
  | PublicExt                   -- ^ Asymmetric public key.
  | CertExt                     -- ^ Certificate.
  | OtherExt Text               -- ^ Some other extension (without the dot)
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Convert a 'FileExtension' into a 'FilePath' (adds the leading dot).
fileExtension :: FileExtension -> FilePath
fileExtension = \case
  KeyExt     -> ".key"
  PrivateExt -> ".prv"
  PublicExt  -> ".pub"
  CertExt    -> ".crt"
  OtherExt x -> "." <> Text.unpack x

--------------------------------------------------------------------------------
-- | Used by the 'CryptoniteT' backend to store and retrieve keys.
data KeyManager = KeyManager
  { managerGetKey :: Label -> FileExtension -> IO GetStatus
    -- ^ A function that can fetch keys on demand.

  , managerPutKey :: Label -> FileExtension -> ByteString -> IO PutStatus
    -- ^ A function that can store keys on demand.
  }

--------------------------------------------------------------------------------
-- | A 'KeyManager' for the 'CryptoniteT' backend that stores keys on
-- the file system.
fileManager
  :: FilePath   -- ^ The directory where keys will be stored.
  -> IO KeyManager
fileManager dir = do
    sem <- newQSem 1

    return KeyManager
      { managerGetKey =  (limit sem .)    . get
      , managerPutKey = ((limit sem .) .) . put
      }

  where
    limit :: QSem -> IO a -> IO a
    limit sem = bracket_ (waitQSem sem) (signalQSem sem)

    get :: Label -> FileExtension -> IO GetStatus
    get label ext = do
      r <- tryIO (ByteString.readFile (path label ext))
      return (either (const GetFailed) GetSucceeded r)

    put :: Label -> FileExtension -> ByteString -> IO PutStatus
    put label ext bs = do
      let file  = path label ext
          write = do ByteString.writeFile file "\n"
                     setFileMode file 0o600
                     ByteString.writeFile file bs
      exists <- doesPathExist file
      if exists
        then return PutKeyExists
        else either (const PutFailed) (const PutSucceeded) <$> tryIO write

    path :: Label -> FileExtension -> FilePath
    path (Label file _) ext = dir </> CBS.unpack file <> fileExtension ext
