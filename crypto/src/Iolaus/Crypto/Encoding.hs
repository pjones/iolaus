{-# LANGUAGE MultiParamTypeClasses #-}

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

Helper functions for encoding and decoding binary data to text.

-}
module Iolaus.Crypto.Encoding
  ( Encoding(..)
  , encode
  , decode
  , normalize
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Codec.Binary.Base64Url as Base64
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.ICU.Normalize as ICU

--------------------------------------------------------------------------------
-- | Store binary data as Base64 encoded text.
newtype Encoding = Encoding { getBytes :: ByteString }

--------------------------------------------------------------------------------
instance ToJSON Encoding where
  toJSON = toJSON . encode
  toEncoding = toEncoding . encode

 -------------------------------------------------------------------------------
instance FromJSON Encoding where
  parseJSON = fmap decode . parseJSON

--------------------------------------------------------------------------------
instance Show Encoding where
  show = Text.unpack . encode

--------------------------------------------------------------------------------
-- | Encode a binary value as text (Base64).
encode :: Encoding -> Text
encode (Encoding bs) = decodeUtf8 (Base64.encode bs)

--------------------------------------------------------------------------------
-- | Decode Base64 text back into binary format.
decode :: Text -> Encoding
decode = Encoding . check . Base64.decode . encodeUtf8
  where
    check :: Either (ByteString, ByteString) ByteString -> ByteString
    check (Left (bs, _)) = bs
    check (Right bs)     = bs

--------------------------------------------------------------------------------
-- | Normalize text so that it will hash the same way given different
-- representations.
normalize :: Text -> ByteString
normalize = encodeUtf8 . ICU.normalize ICU.NFKC . Text.strip
