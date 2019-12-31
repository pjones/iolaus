{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

-}
module Iolaus.Crypto.Signature
  ( Signature(..)
  , SigStatus(..)
  , sigToX509
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.X509 as X509

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Encoding (Encoding(..))
import Iolaus.Crypto.Encoding as Encoding
import Iolaus.Crypto.Key

--------------------------------------------------------------------------------
-- | A signature created from asymmetric cryptography.
data Signature a = Signature
  { sigBytes :: ByteString
  , sigAlgo  :: Algo
  , sigHash  :: Hash
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | The result of verifying a signature.
data SigStatus
  = SignatureVerified
    -- ^ The signature has been verified to be correct.

  | SignatureMismatch
    -- ^ The signature given does not match the accompanying message.

  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
instance ToJSON (Signature a) where
  toJSON Signature{..} = Aeson.object
    [ "data" .= Encoding.encode (Encoding sigBytes)
    , "algo" .= sigAlgo
    , "hash" .= sigHash
    ]

--------------------------------------------------------------------------------
instance FromJSON (Signature a) where
  parseJSON = Aeson.withObject "Signature" $ \v ->
    Signature <$> fmap Encoding.getBytes (Encoding.decodeM =<< (v .: "data"))
              <*> v .: "algo"
              <*> v .: "hash"

--------------------------------------------------------------------------------
-- | Convert a 'Signature' to the tuple expected by the X509 package.
sigToX509 :: Signature a -> (ByteString, X509.SignatureALG)
sigToX509 Signature{..} = (sigBytes, toX509SigAlg sigHash sigAlgo)
