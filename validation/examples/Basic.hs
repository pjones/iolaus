{-# LANGUAGE OverloadedStrings #-}

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
module Main (main) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import Iolaus.Validation

--------------------------------------------------------------------------------
-- | The data type we want to validate.
data Person = Person
  { name :: Text
    -- ^ Name is required.

  , age  :: Maybe Int
    -- ^ Optional age.  If given, must be valid.

  } deriving Show

--------------------------------------------------------------------------------
-- | Our data validation function.
validatePerson :: Valid Person
validatePerson =
  Person <$> name .: notBlank       <?> "name"
         <*> age  .? intRange 1 150 <?> "age"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  print (check $ Person "Alice" (Just 45))   -- Good
  print (check $ Person "Alice" Nothing)     -- Good
  print (check $ Person "Alice" (Just 155))  -- Bad name
  print (check $ Person "     " (Just 155))  -- Bad name and age

  where
    check :: Person -> Either Errors Person
    check = validate validatePerson
