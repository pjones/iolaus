{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import Control.Lens
import Control.Lens.TH (makeLenses)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Validation (_Success, _Failure)
import Iolaus.Validation
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
data Person = Person
  { _name :: Text
  , _age  :: Int
  } deriving Show

makeLenses ''Person

--------------------------------------------------------------------------------
data Window = Window
 { _title :: Text
 , _border :: Maybe Int
 } deriving Show

makeLenses ''Window

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests

--------------------------------------------------------------------------------
tests :: TestTree
tests =
  testGroup "Tests"
    [ testPerson
    , testWindow
    , testChangeType
    , testRemoveMaybe
    , testPassthru
    , testAssertions
    ]

--------------------------------------------------------------------------------
testPerson :: TestTree
testPerson = testCase "Person" $ do
    isn't _Left (go (Person "foo" 1)) @? "should pass"
    isn't _Right (go (Person "   " 1)) @? "should fail"
    isn't _Right (go (Person "foo" 0)) @? "should fail"
    NE.length <$> (^? _Left.errors) (go $ Person "" 0) @?= Just 3

  where
    go :: Person -> Either Errors Person
    go = validate person

    person :: Validation Person Person
    person = Person <$> name ~: notBlank
                    <*> age  ~: intRange 1 150

--------------------------------------------------------------------------------
testWindow :: TestTree
testWindow = testCase "Window" $ do
    isn't _Left (go $ Window "foo" (Just 1)) @? "should pass"
    isn't _Left (go $ Window "foo" Nothing)  @? "should pass"
    isn't _Right (go $ Window "foo" (Just 0)) @? "should fail"

  where
    go :: Window -> Either Errors Window
    go = validate window

    window :: Validation Window Window
    window = Window <$> title  ~: notBlank
                    <*> border ~? intRange 1 10

--------------------------------------------------------------------------------
testChangeType :: TestTree
testChangeType = testCase "Change Type" $ do
    (go (Nothing, 'A') ^? _Right) @?= Just 2
    (go (Just 5,  'A') ^? _Right) @?= Just 5
    (go (Just 6,  'A') ^? _Right) @?= Nothing

  where
    go :: (Maybe Int, Char) -> Either Errors Int
    go = validate (_1 ~? intRange 1 5 .?= 2)

--------------------------------------------------------------------------------
testRemoveMaybe :: TestTree
testRemoveMaybe = testCase "Remove Maybe" $ do
    (go (Nothing, Nothing) ^? _Right) @?= Just (1, "blank")
    (go (Just 0, Nothing)  ^? _Right) @?= Just (0, "blank")
    (go (Just 2, Nothing)  ^? _Right) @?= Nothing

  where
    go :: (Maybe Int, Maybe Text) -> Either Errors (Int, Text)
    go = validate ((,) <$> _1 ~? intRange 0 1 .?= 1       <?> "_1"
                       <*> _2 ~? notBlank     .?= "blank" <?> "_2")

--------------------------------------------------------------------------------
testPassthru :: TestTree
testPassthru = testCase "Passthru" $ do
  validate passthru 'A'                ^? _Right @?= Just 'A'
  validate (_1 ~: passthru) ('A', 'B') ^? _Right @?= Just 'A'

--------------------------------------------------------------------------------
testAssertions :: TestTree
testAssertions = testCase "Assertions" $ do
    minInt 1 @! (2 :: Int)
    minInt 1 @! (1 :: Int)
    minInt 1 @* (0 :: Int)

    maxInt 2 @* (3 :: Int)
    maxInt 2 @! (2 :: Int)
    maxInt 2 @! (1 :: Int)

    minDec 1.0 @! (1.1 :: Double)
    minDec 1.0 @! (1.0 :: Double)
    minDec 1.0 @* (0.9 :: Double)

    maxDec 1.0 @* (1.1 :: Double)
    maxDec 1.0 @! (1.0 :: Double)
    maxDec 1.0 @! (0.9 :: Double)

    assertJust @! Just 'A'
    assertJust @* (Nothing :: Maybe Char)
    assertNothing @! (Nothing :: Maybe Char)
    assertNothing @* Just 'A'

    assertTrue @! True
    assertTrue @* False
    assertFalse @! False
    assertFalse @* True

    notEmpty @! ("abc" :: Text)
    notEmpty @* ("" :: Text)
    notBlank @! ("abc" :: Text)
    notBlank @* ("" :: Text)
    notBlank @* ("\n" :: Text)
    notBlank @* ("  " :: Text)

    minLen 1 @! ("ab" :: Text)
    minLen 1 @! ("a" :: Text)
    minLen 1 @* ("" :: Text)

    maxLen 2 @* ("abc" :: Text)
    maxLen 2 @! ("ab" :: Text)
    maxLen 2 @! ("a" :: Text)
    maxLen 2 @! ("" :: Text)

  where
    -- PASS:
    (@!) :: Validation a a -> a -> Assertion
    (@!) v x = isn't _Failure (validate v x) @? "should pass"

    -- FAIL:
    (@*) :: Validation a a -> a -> Assertion
    (@*) v x = isn't _Success (validate v x) @? "should fail"
