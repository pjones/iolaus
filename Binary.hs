{-|

Copyright:
  This file is part of the package sthenauth. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    git://code.devalot.com/sthenauth.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Helper functions for encoding and decoding binary data to text.

-}
module Sthenauth.Crypto.Binary
  ( Binary(..)
  , encode
  , decode
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.ByteString.Base64.URL as Base64
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

--------------------------------------------------------------------------------
-- | Store binary data as Base64 encoded text.
newtype Binary = Binary { getBytes :: ByteString }

--------------------------------------------------------------------------------
instance ToJSON Binary where
  toJSON = toJSON . encode
  toEncoding = toEncoding . encode

 -------------------------------------------------------------------------------
instance FromJSON Binary where
  parseJSON = fmap decode . parseJSON

--------------------------------------------------------------------------------
-- | Encode a binary value as text (Base64).
encode :: Binary -> Text
encode (Binary bs) = decodeUtf8 (Base64.encode bs)

--------------------------------------------------------------------------------
-- | Decode Base64 text back into binary format.
decode :: Text -> Binary
decode = Binary . Base64.decodeLenient . encodeUtf8
