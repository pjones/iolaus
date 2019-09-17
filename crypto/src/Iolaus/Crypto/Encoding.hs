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
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

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
decode = Encoding . Base64.decodeLenient . encodeUtf8