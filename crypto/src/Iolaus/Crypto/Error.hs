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

License: Apache-2.0

-}
module Iolaus.Crypto.Error
  ( CryptoError(..)
  , AsCryptoError(..)
  , wrappedCryptoError
  , liftCryptoError
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Except (MonadError)
import Control.Lens.TH (makeClassyPrisms)
import Control.Monad.Error.Lens (throwing)
import qualified Crypto.Error as CE

--------------------------------------------------------------------------------
-- | Errors that might occur while constructing an environment.
data CryptoError = InvalidKeyLength
                 | InvalidSaltLength
                 | WrappedCryptoError CE.CryptoError
                 deriving Show

makeClassyPrisms ''CryptoError

--------------------------------------------------------------------------------
-- | Helper function to create errors from Cryptonite errors.
wrappedCryptoError :: CE.CryptoError -> CryptoError
wrappedCryptoError CE.CryptoError_KeySizeInvalid = InvalidKeyLength
wrappedCryptoError CE.CryptoError_SaltTooSmall   = InvalidSaltLength
wrappedCryptoError e                             = WrappedCryptoError e

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
