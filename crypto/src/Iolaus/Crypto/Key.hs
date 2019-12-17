{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

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
  , decodeKey
  , PublicKey(..)
  , encodePublicKey
  , decodePublicKey
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
import qualified Data.ByteArray.Encoding as Base
import Control.Applicative ((<|>), empty)
import Control.Exception.Safe (tryIO)
import qualified Crypto.PubKey.RSA as RSA
import Data.ASN1.BinaryEncoding (DER(..))
import qualified Data.ASN1.Encoding as ASN1
import Data.ASN1.Types (ASN1)
import qualified Data.ASN1.Types as ASN1
import Data.Aeson (ToJSON, FromJSON)
import Data.Bifunctor (first)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.PEM as PEM
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.X509 as X509
import GHC.Generics (Generic)
import System.Directory (doesPathExist)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Package Imports:
import qualified Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Error

--------------------------------------------------------------------------------
-- | The public key portion of a key pair.
--
-- To create a public key from a PEM encoded file, use the
-- 'decodePublicKey' function.
--
-- To store a public key in a file or transmit it over the network,
-- use the 'encodePublicKey' function.
newtype PublicKey = RSAPubKey (Algo, RSA.PublicKey)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Encode a public key in standard PEM-encoded ASN1 DER format.
encodePublicKey :: PublicKey -> ByteString
encodePublicKey = PEM.pemWriteBS . mkPEM . forPEM . toX509
  where
    toX509 :: PublicKey -> X509.PubKey
    toX509 (RSAPubKey (_, k)) = X509.PubKeyRSA k

    forPEM :: X509.PubKey -> ByteString
    forPEM = ASN1.encodeASN1' DER . ($ []) . ASN1.toASN1

    mkPEM :: ByteString -> PEM.PEM
    mkPEM = PEM.PEM "PUBLIC KEY" []

--------------------------------------------------------------------------------
-- | Decode a public key that was encoded in PEM ASN1 DER.  The first
-- usable key found in the PEM stream is returned.
decodePublicKey :: ByteString -> Maybe PublicKey
decodePublicKey bs = do
    pems <- toM (PEM.pemParseBS bs)
    foldl (\a b -> a <|> fromPEM b) empty pems

  where
    fromPEM :: PEM.PEM -> Maybe PublicKey
    fromPEM pem = do
      as <- toM (ASN1.decodeASN1' DER (PEM.pemContent pem))
      foldl (\a b -> a <|> fromX509 b) empty (toX509 as)

    toX509 :: [ASN1] -> [X509.PubKey]
    toX509 as = go ([], as)
      where
        un :: Either String (a, [ASN1]) -> ([a], [ASN1])
        un (Left _) = ([], [])
        un (Right (x, xs)) = ([x], xs)

        go :: ([X509.PubKey], [ASN1]) -> [X509.PubKey]
        go (xs, []) = xs
        go (xs, ys) = go (first (xs ++) (un (ASN1.fromASN1 ys)))

    fromX509 :: X509.PubKey -> Maybe PublicKey
    fromX509 = \case
      X509.PubKeyRSA k@(RSA.PublicKey n _ _)
        | n == 256  -> Just (RSAPubKey (RSA2048, k))
        | n == 512  -> Just (RSAPubKey (RSA4096, k))
        | otherwise -> Nothing
      _ -> Nothing

    toM :: Either l r -> Maybe r
    toM (Left _)  = Nothing
    toM (Right x) = Just x

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
  { getLabel  :: CBS.ByteString -- ^ Access the encoded label.
  } deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Create a label from a 'Text' value.
toLabel :: Text -> Label
toLabel = Label . Base.convertToBase Base.Base32 . Encoding.normalize

--------------------------------------------------------------------------------
-- | The inverse of 'toLabel'.
getLabelText :: Label -> Text
getLabelText Label{..} =
    decodeUtf8 . check . Base.convertFromBase Base.Base32 $ getLabel
  where
    check :: Either l ByteString -> ByteString
    check (Left _)   = getLabel -- Should never happen.
    check (Right bs) = bs

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
decodeKey :: (Binary a) => Label -> ByteString -> Either CryptoError a
decodeKey label bs =
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
