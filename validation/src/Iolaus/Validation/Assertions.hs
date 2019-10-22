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
module Iolaus.Validation.Assertions
  ( HasValidLength(..)
  , minInt
  , maxInt
  , intRange
  , minDec
  , maxDec
  , decRange
  , assertJust
  , assertNothing
  , assertTrue
  , assertFalse
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Validation.Prim
import Iolaus.Validation.Error

--------------------------------------------------------------------------------
-- | Validation for types that can have a length.
class HasValidLength a where
  -- | Must have at least one element.
  notEmpty :: (Applicative m) => ValidationT m a a
  notEmpty = assert (\x -> compareLength x 0 == GT) InputEmptyError

  -- | Must have at least one element that isn't considered to be blank.
  notBlank :: (Applicative m) => ValidationT m a a
  notBlank = notEmpty

  -- | Length cannot be less than @n@.
  minLen :: (Applicative m) => Int64 -> ValidationT m a a
  minLen n = assert (\x -> compareLength x (n - 1) == GT) (MinLenError n)

  -- | Length cannot be greater than @n@.
  maxLen :: (Applicative m) => Int64 -> ValidationT m a a
  maxLen n = assert (\x -> compareLength x (n + 1) == LT) (MaxLenError n)

  -- | Length must be between two values (inclusive).
  lengthWithin :: (Applicative m) => Int64 -> Int64 -> ValidationT m a a
  lengthWithin x y = minLen x *> maxLen y

  -- | Compare length to given value.
  compareLength :: a -> Int64 -> Ordering

--------------------------------------------------------------------------------
instance HasValidLength Text where
  compareLength t n = Text.compareLength t (fromIntegral n)
  notBlank = notEmpty *> assert (not . Text.all isSpace) InputBlankError

--------------------------------------------------------------------------------
instance HasValidLength LText.Text where
  compareLength = LText.compareLength
  notBlank = notEmpty *> assert (not . LText.all isSpace) InputBlankError

--------------------------------------------------------------------------------
instance HasValidLength [a] where
  compareLength [] n = compare 0 n
  compareLength xs n = compare (fromIntegral $ length xs) n

--------------------------------------------------------------------------------
instance HasValidLength (NonEmpty a) where
  compareLength ne = compare (fromIntegral $ NE.length ne)

--------------------------------------------------------------------------------
-- | Cannot be less than @n@.
minInt :: (Applicative m, Integral a) => a -> ValidationT m a a
minInt n = assert (>= n) (MinIntError $ fromIntegral n)

--------------------------------------------------------------------------------
-- | Cannot be greater than @n@.
maxInt ::  (Applicative m, Integral a) => a -> ValidationT m a a
maxInt n = assert (<= n) (MaxIntError $ fromIntegral n)

--------------------------------------------------------------------------------
-- | Must be between two values (inclusive).
intRange :: (Applicative m, Integral a) => a -> a -> ValidationT m a a
intRange x y = minInt x *> maxInt y

--------------------------------------------------------------------------------
-- | Cannot be less than @n@.
minDec :: (Applicative m, RealFloat a) => a -> ValidationT m a a
minDec n = assert (>= n) (MinDecError $ fromFloatDigits n)

--------------------------------------------------------------------------------
-- | Cannot be greater than @n@.
maxDec :: (Applicative m, RealFloat a) => a -> ValidationT m a a
maxDec n = assert (<= n) (MaxDecError $ fromFloatDigits n)

--------------------------------------------------------------------------------
-- | Must be between two values (inclusive).
decRange :: (Applicative m, RealFloat a) => a -> a -> ValidationT m a a
decRange x y = minDec x *> maxDec y

--------------------------------------------------------------------------------
-- | Must be a 'Just'.
assertJust ::  (Applicative m) => ValidationT m (Maybe a) (Maybe a)
assertJust = assert isJust ShouldBeJustError

--------------------------------------------------------------------------------
-- | Must be a 'Nothing'.
assertNothing :: (Applicative m) => ValidationT m (Maybe a) (Maybe a)
assertNothing = assert isNothing ShouldBeNothingError

--------------------------------------------------------------------------------
-- | Must be 'True'.
assertTrue :: (Applicative m) => ValidationT m Bool Bool
assertTrue = assert id ShouldBeTrueError

--------------------------------------------------------------------------------
-- | Must be 'False'.
assertFalse :: (Applicative m) => ValidationT m Bool Bool
assertFalse = assert not ShouldBeFalseError
