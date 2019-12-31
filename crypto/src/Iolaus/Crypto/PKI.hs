{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

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

Public Key Infrastructure and Certificate Authorities.

-}
module Iolaus.Crypto.PKI
  ( -- * A Monad for Certificate Authorities
    MonadCertAuth(..)

    -- * Functions for Implementing a Certificate Authority
  , newCert
  , signCert
  , encodeSignedCert
  , PathLenConstraint(..)
  , certForCA

    -- * Functions for Using a Certificate Authority
  , Endpoint(..)
  , certForTLS
  , newLeafCert
  , certChain

    -- * A Free Monad for Certificate Authorities
  , CaOptF(..)
  , CaOpt
  , nextSerialNumber
  , fetchHashAndAlgo
  , fetchRootCert
  , fetchIntermediateCert
  , signWithIntermediateCert
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.ASN1.OID as ASN1
import qualified Data.ASN1.Types as ASN1
import qualified Data.ByteString.Lazy as LBS
import Data.Hourglass (DateTime(..), timeFromElapsedP)
import Data.PEM (PEM)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 (Certificate(..), SignedCertificate, Signed(..))
import qualified Data.X509 as X509

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Signature
import Iolaus.Crypto.API
import Iolaus.Crypto.PEM
import Iolaus.Crypto.Monad (MonadCrypto(..), KeyPair, HasKeyAccess(..))
import qualified Iolaus.Crypto.Monad as M
import Iolaus.Crypto.PKI.Monad

--------------------------------------------------------------------------------
-- | Generate a new (unsigned) certificate.
newCert
  :: ( MonadCrypto k m
     , MonadCertAuth m
     )
  => Integer
     -- ^ The serial number to use.
  -> Text
     -- ^ Common name of the subject.
  -> Maybe SignedCertificate
     -- ^ Issuer ('Nothing' means to self-issue).
  -> (UTCTime, UTCTime)
     -- ^ Time range for validity.
  -> m (KeyPair k, Certificate)
     -- ^ Generated 'KeyPair' and 'Certificate'.
newCert sn cn issuer (start, end) = do
  (hash, algo) <- liftCaOpt fetchHashAndAlgo
  key <- generateKeyPair algo (toLabel (cn <> " " <> Text.pack (show sn)))
  pub <- toPublicKey key

  let subjectDN    = makeCN cn
      dnFromIssuer = certSubjectDN . signedObject . X509.getSigned

      cert = Certificate
        { certVersion      = 3
        , certSerial       = sn
        , certSignatureAlg = toX509SigAlg hash algo
        , certIssuerDN     = maybe subjectDN dnFromIssuer issuer
        , certValidity     = (toDateTime start, toDateTime end)
        , certSubjectDN    = subjectDN
        , certPubKey       = toX509PubKey pub
        , certExtensions   = X509.Extensions (Just [makeDnsName cn])
        }

  return (key, cert)

--------------------------------------------------------------------------------
-- | Sign a certificate.
signCert
  :: ( MonadCrypto k m )
  => KeyPair k
     -- ^ The keys to use for signing.
  -> Hash
     -- ^ The hashing algorithm to use.
  -> Certificate
     -- ^ The certificate to sign.
  -> m SignedCertificate
     -- ^ The signed certificate.
signCert key hash = X509.objectToSignedExactF
 (fmap sigToX509 . liftCryptoOpt . M.asymmetricSign key hash)

--------------------------------------------------------------------------------
appendExtension :: (X509.Extension a) => a -> Bool -> Certificate -> Certificate
appendExtension ext crit cert = cert { certExtensions = updated }
  where
    updated :: X509.Extensions
    updated =
      let (X509.Extensions old) = certExtensions cert
          new = X509.extensionEncode crit ext
      in X509.Extensions (old <> Just [new])

--------------------------------------------------------------------------------
-- | Identify which end of a connection should be represented.
data Endpoint = Client | Server deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Select the appropriate extended key usage X509 extension.
endpointKeyUsage :: Endpoint -> X509.ExtKeyUsagePurpose
endpointKeyUsage = \case
  Client -> X509.KeyUsagePurpose_ClientAuth
  Server -> X509.KeyUsagePurpose_ServerAuth

--------------------------------------------------------------------------------
-- | Mark a certificate as being used for TLS/SSL.
certForTLS :: Endpoint -> Certificate -> Certificate
certForTLS ep = appendExtension keyUsage False
  where
    keyUsage = X509.ExtExtendedKeyUsage [endpointKeyUsage ep]

--------------------------------------------------------------------------------
-- | The maximum number of non-self-issued certificates that are
-- allowed to follow the certificate you are tagging with this type.
--
-- For more details see RFC 5280 section 4.2.1.9:
--
-- https://www.rfc-editor.org/rfc/rfc5280.html#section-4.2.1.9
newtype PathLenConstraint =
  PathLenConstraint { pathLenConstraint :: Maybe Int }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Mark a certificate as being used for Certificate Authority operations.
certForCA :: PathLenConstraint -> Certificate -> Certificate
certForCA plen =
    appendExtension keyUsage False .
    appendExtension basic    True
  where
    keyUsage = X509.ExtKeyUsage [X509.KeyUsage_keyCertSign, X509.KeyUsage_cRLSign]
    basic = X509.ExtBasicConstraints True (fromIntegral <$> pathLenConstraint plen)

--------------------------------------------------------------------------------
newLeafCert
  :: ( MonadCrypto k m
     , MonadCertAuth m
     , HasKeyAccess k m
     )
  => Text
     -- ^ Common name for the subject field.
  -> (UTCTime, UTCTime)
     -- ^ Time range for validity.
  -> (Certificate -> Certificate)
     -- ^ Function to fine-tune a certificate (e.g. 'certForTLS').
  -> m (LBS.ByteString, SignedCertificate)
     -- ^ PEM-encoded private key and signed certificate.
newLeafCert name validity f = do
  sn <- liftCaOpt nextSerialNumber
  issuer <- liftCaOpt fetchIntermediateCert
  (key, cert) <- newCert sn name (Just issuer) validity
  signed <- liftCaOpt (signWithIntermediateCert (f cert))
  bs <- encodePrivateKey key
  return (bs, signed)

--------------------------------------------------------------------------------
-- | Encode a signed certificate in PEM format.
encodeSignedCert :: SignedCertificate -> PEM
encodeSignedCert = toPEM' CertificateSection . X509.encodeSignedObject

--------------------------------------------------------------------------------
-- | An entire certificate chain encoded as PEM.
certChain
  :: ( MonadCertAuth m )
  => Maybe SignedCertificate
     -- ^ Include this certificate at the end of the chain
  -> m LBS.ByteString
certChain mcert = do
  root <- liftCaOpt fetchRootCert
  int  <- liftCaOpt fetchIntermediateCert

  return $ case mcert of
    Nothing -> go [root, int]
    Just c  -> go [root, int, c]

  where
    go :: [SignedCertificate] -> LBS.ByteString
    go = encodePEM . map encodeSignedCert

--------------------------------------------------------------------------------
-- | Create a common name field.
makeCN :: Text -> X509.DistinguishedName
makeCN = X509.DistinguishedName . pure .
  (ASN1.getObjectID X509.DnCommonName,) .
  ASN1.ASN1CharacterString ASN1.UTF8 . Text.encodeUtf8

--------------------------------------------------------------------------------
-- | Create a Subject Alternate Name extension.
makeDnsName :: Text -> X509.ExtensionRaw
makeDnsName = X509.extensionEncode False . X509.ExtSubjectAltName .
  pure . X509.AltNameDNS . Text.unpack

--------------------------------------------------------------------------------
-- | Helper function to create a 'DateTime' value from the hourglass
-- package that the x509 package uses.
toDateTime :: UTCTime -> DateTime
toDateTime = timeFromElapsedP . fromInteger . truncate . utcTimeToPOSIXSeconds
