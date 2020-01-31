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
module Iolaus.Crypto.Internal.Encoding
  ( Encoding(..)
  , encode
  , decode
  , decodeM
  , normalize
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (MonadPlus, mzero, (<=<))
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.ByteArray.Encoding as Base
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
  parseJSON = decodeM <=< parseJSON

--------------------------------------------------------------------------------
instance Show Encoding where
  show = Text.unpack . encode

--------------------------------------------------------------------------------
-- | Encode a binary value as text (Base64).
encode :: Encoding -> Text
encode (Encoding bs) = decodeUtf8 (Base.convertToBase Base.Base64URLUnpadded bs)

--------------------------------------------------------------------------------
-- | Decode Base64 text back into binary format.
decode :: Text -> Maybe Encoding
decode = fmap Encoding .
         check . Base.convertFromBase Base.Base64URLUnpadded . encodeUtf8
  where
    check :: Either l ByteString -> Maybe ByteString
    check (Left _)   = Nothing
    check (Right bs) = Just bs

--------------------------------------------------------------------------------
-- | Decode with failure.
decodeM :: (MonadPlus m) => Text -> m Encoding
decodeM = maybe mzero return . decode

--------------------------------------------------------------------------------
-- | Normalize text so that it will hash the same way given different
-- representations.
normalize :: Text -> ByteString
normalize = encodeUtf8 . ICU.normalize ICU.NFKC . Text.strip
