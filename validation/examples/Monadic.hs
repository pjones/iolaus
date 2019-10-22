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
  , age  :: Int
  } deriving Show

--------------------------------------------------------------------------------
-- | A custom assertion that needs to run in IO.
isValidName :: ValidT IO Text
isValidName = assertM (Invalid "bad name") $ \pname -> do
  putStrLn ("Does this name look good (y|n) " <> show pname <> "\n")
  answer <- getChar
  pure (answer == 'y' || answer == 'Y')

--------------------------------------------------------------------------------
-- | Our data validation type signature is a little more complicated now.
validatePerson :: ValidT IO Person
validatePerson =
  Person <$> name .: isValidName    <?> "name"
         <*> age  .: intRange 1 150 <?> "age"

--------------------------------------------------------------------------------
main :: IO ()
main = check (Person "Alice" 45) >>= print

  where
    check :: Person -> IO (Either Errors Person)
    check = runValidationEither validatePerson
