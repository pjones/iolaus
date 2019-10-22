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
module Iolaus.Validation (
  validate,
  Valid,
  ValidT,
  Validation,
  Result,
  (.:),
  (~:),
  (.?),
  (~?),
  (.?=),
  (<?>),

  module Iolaus.Validation.Assertions,
  assert,
  assert',
  passthru,

  Errors,
  errors,
  ValidationError,
  field,
  message,
  exception,

  module Iolaus.Validation.Error,

  ValidationT,
  assertM,
  runValidation,
  runValidationEither,


  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens ((^.))
import Data.Functor.Identity
import qualified Data.Validation as V

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Validation.Assertions
import Iolaus.Validation.Error
import Iolaus.Validation.Prim

--------------------------------------------------------------------------------
-- | Restrict 'ValidationT' to the 'Identity' functor.
type Validation r a = ValidationT Identity r a

--------------------------------------------------------------------------------
-- | Like 'Validation' except the focus and result are the same type.
type Valid a = ValidationT Identity a a

--------------------------------------------------------------------------------
-- | Like 'ValidationT' except the focus and result are the same type.
type ValidT m a = ValidationT m a a

--------------------------------------------------------------------------------
-- | Run a validation function in an arbitrary context and convert the
-- result to an 'Either' value.
runValidationEither :: (Functor m) => ValidationT m r a -> r -> m (Either Errors a)
runValidationEither v = fmap (^. V._Either) . runValidation v

--------------------------------------------------------------------------------
-- | Simple validation that results in an 'Either' value.
validate :: Validation r a -> r -> Either Errors a
validate v = runIdentity . runValidationEither v
