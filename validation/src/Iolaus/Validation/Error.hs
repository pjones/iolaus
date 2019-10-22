{-# LANGUAGE LambdaCase #-}

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
module Iolaus.Validation.Error
  ( ValidationException(..)
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Exception (Exception(..))
import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | The error type used by the included assertions.
data ValidationException
  = Invalid Text
  | InputEmptyError
  | InputBlankError
  | MinLenError Int64
  | MaxLenError Int64
  | MinIntError Int64
  | MaxIntError Int64
  | MinDecError Scientific
  | MaxDecError Scientific
  | ShouldBeJustError
  | ShouldBeNothingError
  | ShouldBeTrueError
  | ShouldBeFalseError
  deriving (Show)

--------------------------------------------------------------------------------
-- FIXME: How to localize this text?
instance Exception ValidationException where
  displayException = \case
    Invalid t            -> "invalid: " <> Text.unpack t
    InputEmptyError      -> "cannot be empty"
    InputBlankError      -> "cannot be blank"
    MinLenError n        -> "length cannot be shorter than " <> show n
    MaxLenError n        -> "length cannot be longer than " <> show n
    MinIntError n        -> "cannot be less than " <> show n
    MaxIntError n        -> "cannot be greater than " <> show n
    MinDecError n        -> "cannot be less than " <> show n
    MaxDecError n        -> "cannot be greater than " <> show n
    ShouldBeJustError    -> "must be present"
    ShouldBeNothingError -> "must not be present"
    ShouldBeTrueError    -> "must be true"
    ShouldBeFalseError   -> "must be false"
