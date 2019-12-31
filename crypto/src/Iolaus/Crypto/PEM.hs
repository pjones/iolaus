{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

PEM encoding and decoding.

-}
module Iolaus.Crypto.PEM
  ( SectionLabel(..)
  , toPEM
  , toPEM'
  , fromPEM
  , fromPEM'
  , encodePEM
  , decodePEM
  , decodePEM'
  ) where


--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError, runExcept)
import Data.ASN1.BinaryEncoding (DER(..))
import qualified Data.ASN1.Encoding as ASN1
import Data.ASN1.Error (ASN1Error)
import Data.ASN1.Types (ASN1, ASN1Object)
import qualified Data.ASN1.Types as ASN1
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.List (unfoldr)
import Data.PEM (PEM(..), pemWriteLBS, pemParseLBS)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- Package Imports:
import Iolaus.Crypto.Error

--------------------------------------------------------------------------------
-- | Some of the section labels used in PEM files.
--
-- https://github.com/openssl/openssl/blob/master/include/openssl/pem.h
data SectionLabel
  = PublicKeySection
  | PrivateKeySection
  | CertificateRequestSection
  | CertificateSection
  | OtherSection Text
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Encode an 'ASN1Object' as 'PEM'.
toPEM :: (ASN1Object a) => SectionLabel -> a -> PEM
toPEM label = toPEM' label . ASN1.encodeASN1' DER . ($ []) . ASN1.toASN1

--------------------------------------------------------------------------------
-- | Encode a 'ByteString' into 'PEM'.
toPEM' :: SectionLabel -> ByteString -> PEM
toPEM' label = PEM (pemSectionLabel label) []

--------------------------------------------------------------------------------
-- | Decode an 'ASN1Object' from 'PEM'.  If an error occurs an empty
-- list is returned.  If you want to know about errors use 'fromPEM''
-- instead.
fromPEM :: (ASN1Object a) => PEM -> [a]
fromPEM = either (const [] :: CryptoError -> [a]) id . runExcept . fromPEM'

--------------------------------------------------------------------------------
-- | Decode an 'ASN1Object' from 'PEM'.
fromPEM' :: forall m e a. (MonadError e m, AsCryptoError e, ASN1Object a) => PEM -> m [a]
fromPEM' = either liftE (return . unfoldr step) .
    ASN1.decodeASN1' DER . pemContent
  where
    step :: [ASN1] -> Maybe (a, [ASN1])
    step [] = Nothing
    step xs = case ASN1.fromASN1 xs of
                Left _        -> Nothing
                Right (x, ys) -> Just (x, ys)

    liftE :: ASN1Error -> m [a]
    liftE = throwing _WrappedASN1Error

--------------------------------------------------------------------------------
-- | Encode a list of 'PEM' values into a lazy 'ByteString'.
encodePEM :: [PEM] -> LBS.ByteString
encodePEM = Builder.toLazyByteString .
  foldMap (Builder.lazyByteString . pemWriteLBS)

--------------------------------------------------------------------------------
-- | Decode a stream of bytes into a list of PEM structures.  Decoding
-- errors are ignored.  If you want to know if an error occurred then
-- use 'decodePEM'' or the 'pemParseLBS' function directly.
decodePEM :: LBS.ByteString -> [PEM]
decodePEM = either (const mempty) id . pemParseLBS

--------------------------------------------------------------------------------
-- | Variant of 'decodePEM' that throws an error if there is a parse failure.
decodePEM' :: (MonadError e m, AsCryptoError e) => LBS.ByteString -> m [PEM]
decodePEM' bs =
  case pemParseLBS bs of
    Left e   -> throwing _PemDecodingError (Text.pack e)
    Right as -> return as

--------------------------------------------------------------------------------
pemSectionLabel :: SectionLabel -> String
pemSectionLabel = \case
  PublicKeySection          -> "PUBLIC KEY"
  PrivateKeySection         -> "PRIVATE KEY"
  CertificateRequestSection -> "CERTIFICATE REQUEST"
  CertificateSection        -> "CERTIFICATE"
  OtherSection t            -> Text.unpack t
