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

Helper functions and types for the X509 package.

-}
module Iolaus.Crypto.X509
  ( -- * Creating and Signing Certificates
    makeCert
  , signCert
  , encodeSignedCert

    -- * Certificate Authority Extensions
  , PathLenConstraint(..)
  , certForCA

    -- * Leaf Certificate Extensions
  , Endpoint(..)
  , certForTLS
  , certForDomain

  -- * General Certificate Extensions
  , CriticalExt(..)
  , appendExtension
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.ASN1.OID as ASN1
import qualified Data.ASN1.Types as ASN1
import Data.Bits (shiftL, (.|.))
import Data.Hourglass (DateTime(..), timeFromElapsedP)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Data.X509 (Certificate(..), SignedCertificate, Signed(..))
import qualified Data.X509 as X509

--------------------------------------------------------------------------------
-- Project Imports:
import Control.Monad.Crypto.Class
import Iolaus.Crypto.Internal.Key
import Iolaus.Crypto.PEM
import Iolaus.Crypto.Signature

--------------------------------------------------------------------------------
-- | Generate a new (unsigned) certificate.
--
-- The generated certificate will need to have any necessary
-- extensions added to it before passing it to the 'signCert'
-- function.
makeCert
  :: UUID
     -- ^ The serial number to use.
  -> Text
     -- ^ Common name of the subject.
  -> Algo
     -- ^ The asymmetric algorithm to use.
  -> Hash
     -- ^ The hashing algorithm to use.
  -> Maybe SignedCertificate
     -- ^ Issuer ('Nothing' means to self-issue).
  -> (UTCTime, UTCTime)
     -- ^ Time range for validity.
  -> PublicKey
     -- ^ The public key to put into the certificate.
  -> Certificate
     -- ^ The new, unsigned certificate.
makeCert sn cn algo hash issuer (start, end) =
  let subjectDN    = makeCN cn
      dnFromIssuer = certSubjectDN . signedObject . X509.getSigned

      cert pub = Certificate
        { certVersion      = 3
        , certSerial       = toSerialNumber sn
        , certSignatureAlg = toX509SigAlg hash algo
        , certIssuerDN     = maybe subjectDN dnFromIssuer issuer
        , certValidity     = (toDateTime start, toDateTime end)
        , certSubjectDN    = subjectDN
        , certPubKey       = toX509PubKey pub
        , certExtensions   = X509.Extensions Nothing
        }

  in cert

--------------------------------------------------------------------------------
-- | Sign a certificate.
signCert
  :: ( MonadCrypto k m )
  => KeyPair k
     -- ^ The keys to use for signing.
  -> Certificate
     -- ^ The certificate to sign.
  -> m SignedCertificate
     -- ^ The signed certificate.
signCert key cert =
    X509.objectToSignedExactF
      (fmap sigToX509 . asymmetricSign key hash) cert
  where
    hash :: Hash
    hash = case fromX509SigAlg (certSignatureAlg cert) of
      Nothing -> SHA2_512
      Just (h, _) -> h

--------------------------------------------------------------------------------
-- | Certain X509 certificate extensions need to be marked as critical.
data CriticalExt
  = Critical    -- ^ This extension /must/ be used by all implementations.
  | NonCritical -- ^ Non-conforming implementations can ignore this extension.
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Append an extension to a 'Certificate' with the option to mark it
-- critical or not.
appendExtension
  :: ( X509.Extension a )
  => a                          -- ^ The extension to append.
  -> CriticalExt                -- ^ Critical extension flag.
  -> Certificate                -- ^ The certificate to modify.
  -> Certificate                -- ^ The updated certificate
appendExtension ext crit cert = cert { certExtensions = updated }
  where
    updated :: X509.Extensions
    updated =
      let (X509.Extensions old) = certExtensions cert
          new = X509.extensionEncode isCrit ext
      in X509.Extensions (old <> Just [new])

    isCrit :: Bool
    isCrit = case crit of
      Critical    -> True
      NonCritical -> False

--------------------------------------------------------------------------------
-- | Identify which end of a connection should be represented.
data Endpoint
  = Client       -- ^ Client.
  | Server Text  -- ^ Server with the given domain name.
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Select the appropriate extended key usage X509 extension.
endpointKeyUsage :: Endpoint -> X509.ExtKeyUsagePurpose
endpointKeyUsage = \case
  Client   -> X509.KeyUsagePurpose_ClientAuth
  Server _ -> X509.KeyUsagePurpose_ServerAuth

--------------------------------------------------------------------------------
-- | Mark a certificate as being used for TLS/SSL.
--
-- For server certificates the domain name of the server is added as
-- the Common Name and as a DNS name via 'certForDomain'.
certForTLS :: Endpoint -> Certificate -> Certificate
certForTLS ep = appendExtension keyUsage NonCritical . addDomain
  where
    keyUsage = X509.ExtExtendedKeyUsage [endpointKeyUsage ep]
    addDomain = case ep of
      Client -> id
      Server name -> certForDomain name

--------------------------------------------------------------------------------
-- | Alter the certificate's Common Name and add a Subject Alternative
-- Name extension with the DNS name set to the given domain name.
certForDomain :: Text -> Certificate -> Certificate
certForDomain domain cert =
  let cert' = cert { certSubjectDN = makeCN domain }
  in appendExtension (makeDnsName domain) NonCritical cert'

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
    appendExtension keyUsage NonCritical .
    appendExtension basic    Critical
  where
    keyUsage = X509.ExtKeyUsage [X509.KeyUsage_keyCertSign, X509.KeyUsage_cRLSign]
    basic = X509.ExtBasicConstraints True (fromIntegral <$> pathLenConstraint plen)

--------------------------------------------------------------------------------
-- | Encode a signed certificate in PEM format.
encodeSignedCert :: SignedCertificate -> PEM
encodeSignedCert = toPEM' CertificateSection . X509.encodeSignedObject

--------------------------------------------------------------------------------
-- | Create a common name field.
makeCN :: Text -> X509.DistinguishedName
makeCN = X509.DistinguishedName . pure .
  (ASN1.getObjectID X509.DnCommonName,) .
  ASN1.ASN1CharacterString ASN1.UTF8 . Text.encodeUtf8

--------------------------------------------------------------------------------
-- | Create a Subject Alternate Name extension.
makeDnsName :: Text -> X509.ExtSubjectAltName
makeDnsName = X509.ExtSubjectAltName . pure . X509.AltNameDNS . Text.unpack

--------------------------------------------------------------------------------
-- | Helper function to create a 'DateTime' value from the hourglass
-- package that the x509 package uses.
toDateTime :: UTCTime -> DateTime
toDateTime = timeFromElapsedP . fromInteger . truncate . utcTimeToPOSIXSeconds

--------------------------------------------------------------------------------
-- | Helper function to convert a 'UUID' to a X509 serial number.
toSerialNumber :: UUID -> Integer
toSerialNumber uuid =
  let (w1, w2, w3, w4) = UUID.toWords uuid
  in fromIntegral w1 `shiftL` 96 .|.
     fromIntegral w2 `shiftL` 64 .|.
     fromIntegral w3 `shiftL` 32 .|.
     fromIntegral w4
