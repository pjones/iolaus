{-# LANGUAGE TemplateHaskell            #-}

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
module Iolaus.Crypto.Error
  ( CryptoError(..)
  , AsCryptoError(..)
  , wrappedCryptoError
  , fromFailable
  , assert
  , liftCryptoError
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens.TH (makeClassyPrisms)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import qualified Crypto.Error as CE
import Data.Bifunctor (first)
import qualified Data.ASN1.Error as ASN1
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Errors that might occur during cryptographic operations.
data CryptoError
  = InvalidKeyLength
  | InvalidSaltLength
  | MalformedCipherTextError
  | MalformedSignatureTextError
  | MissingAuthTagError
  | AuthTagMismatchError
  | CipherMismatchError Text
  | AlgoMismatchError Text
  | KeyExistsError Text
  | KeyDoesNotExistError Text
  | KeyWriteFailure Text
  | KeyReadFailure Text
  | PemDecodingError Text
  | WrappedCryptoError CE.CryptoError
  | WrappedASN1Error ASN1.ASN1Error
  deriving Show

makeClassyPrisms ''CryptoError

--------------------------------------------------------------------------------
-- | Helper function to create errors from Cryptonite errors.
wrappedCryptoError :: CE.CryptoError -> CryptoError
wrappedCryptoError CE.CryptoError_KeySizeInvalid = InvalidKeyLength
wrappedCryptoError CE.CryptoError_SaltTooSmall   = InvalidSaltLength
wrappedCryptoError e                             = WrappedCryptoError e

--------------------------------------------------------------------------------
-- | Helper function for dealing with the cryptonite 'CryptoFailable'.
fromFailable :: CE.CryptoFailable b -> Either CryptoError b
fromFailable = first wrappedCryptoError . CE.eitherCryptoError

--------------------------------------------------------------------------------
assert :: Bool -> CryptoError -> Either CryptoError ()
assert b e = if b then Right () else Left e

--------------------------------------------------------------------------------
-- | Lift an @Either CryptoError a@ value into a @MonadError@ context.
liftCryptoError
  :: ( MonadError e m
     , AsCryptoError e
     )
  => Either CryptoError a
  -> m a
liftCryptoError (Right x) = pure x
liftCryptoError (Left  x) = throwing _CryptoError x
