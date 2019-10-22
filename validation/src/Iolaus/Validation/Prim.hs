{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
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
module Iolaus.Validation.Prim
  ( ValidationT
  , Result
  , (<?>)

  , Errors
  , errors
  , ValidationError
  , field
  , message
  , exception

  , (.:)
  , (~:)
  , (.?)
  , (~?)
  , (.?=)

  , passthru
  , assert
  , assert'
  , assertM

  , runValidation
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Exception (Exception(..), SomeException)
import Control.Lens (Lens', (^.), (?~), view, over)
import Control.Lens.TH (makeLenses)
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Validation as V

--------------------------------------------------------------------------------
-- | Validation errors are wrapped up in this type.
data ValidationError = ValidationError
  { _field :: Maybe Text
    -- ^ The field name on the record being validated.

  , _message :: Text
    -- ^ An error message.

  , _exception :: SomeException
    -- ^ The exception that produced the error message.

  } deriving (Show)

makeLenses ''ValidationError

instance ToJSON ValidationError where
  toJSON e = Aeson.object
    [ "field" .= (e ^. field)
    , "message" .= (e ^. message)
    , "error" .= show (e ^. exception)
    ]

--------------------------------------------------------------------------------
-- | All errors are collected into this structure.
newtype Errors = Errors { _errors :: NonEmpty ValidationError }
  deriving (Show)

makeLenses ''Errors

--------------------------------------------------------------------------------
instance Semigroup Errors where
  (<>) (Errors x) (Errors y) = Errors (x <> y)

instance ToJSON Errors where
  toJSON (Errors es) = Aeson.object [ "errors" .= es ]

--------------------------------------------------------------------------------
-- | Validation result.
type Result a = V.Validation Errors a

--------------------------------------------------------------------------------
-- | A validation type that allows assertions to run in arbitrary
-- monadic contexts.
--
-- The 'ValidatorT' type itself is /not/ a 'Monad'.
newtype ValidationT m r a = Validation
  { unV :: r -> m (Result a) }

instance (Applicative m) => Functor (ValidationT m r) where
  fmap f (Validation g) = Validation (fmap (f <$>) . g)

instance (Applicative m) => Applicative (ValidationT m r) where
  pure x = Validation (pure . pure . const x)
  (<*>) f v = Validation $ \r -> (<*>) <$> unV f r <*> unV v r

--------------------------------------------------------------------------------
setField :: (Functor m) => Text -> ValidationT m r a -> ValidationT m r a
setField name v =
  let set = over (V._Failure . errors) (NE.map (field ?~ name))
  in Validation (fmap set . unV v)

--------------------------------------------------------------------------------
-- | Label the field that is being validated so error messages are a
-- little more helpful.
(<?>) :: (Functor m) => ValidationT m r a -> Text -> ValidationT m r a
(<?>) = flip setField
infixr 5 <?>

--------------------------------------------------------------------------------
-- | Skip the current focus and pass it through without validation.
passthru :: (Applicative m) => ValidationT m r r
passthru = Validation (pure . pure)

--------------------------------------------------------------------------------
-- | Low-level (and pure) assertion.
assert' :: (Applicative m) => (r -> Result a) -> ValidationT m r a
assert' f = Validation (pure . f)

--------------------------------------------------------------------------------
-- | A pure assertion that expects a predicate function.
assert :: (Applicative m, Exception e)
       => (r -> Bool)
          -- ^ A function that will be given a value to validate.

       -> e
          -- ^ If the predicate function returns 'False', this
          -- exception will be used to generate a validation error.

       -> ValidationT m r r
assert f e = assert' $ \r -> if f r then pure r else mkError e

--------------------------------------------------------------------------------
-- | Monadic assertion.
assertM :: (Monad m, Exception e)
        => e
           -- ^ If the predicate function returns 'False', this
           -- exception will be used to generate a validation error.

        -> (r -> m Bool)
           -- ^ A monadic predicate function.  It will be given the
           -- value to validate.

        -> ValidationT m r r
assertM e k =
  Validation $ \r -> k r >>= \case
    True  -> pure (V.Success r)
    False -> pure (mkError e)

--------------------------------------------------------------------------------
-- | Use a function to change to validation focus.
(.:) :: (r -> a) -> ValidationT m a a -> ValidationT m r a
(.:) f v = Validation $ \r -> unV v (f r)
infixr 7 .:

--------------------------------------------------------------------------------
-- | Use a lens to change the validation focus.
(~:) :: Lens' r a -> ValidationT m a a -> ValidationT m r a
(~:) l v = Validation $ \r -> unV v (view l r)
infixr 6 ~:

--------------------------------------------------------------------------------
-- | Make a validation optional.  If the given function returns a
-- 'Nothing', validation is skipped for this field.
(.?) :: (Applicative m) => (r -> Maybe a) -> ValidationT m a a -> ValidationT m r (Maybe a)
(.?) f v = Validation $ \r ->
  case f r of
    Nothing -> pure (V.Success Nothing)
    Just x  -> (Just <$>) <$> unV v x
infixr 7 .?

--------------------------------------------------------------------------------
-- | Make a validation optional.  If the given lens produces a
-- 'Nothing', validation is skipped for this field.
(~?) :: (Applicative m) => Lens' r (Maybe a) -> ValidationT m a a -> ValidationT m r (Maybe a)
(~?) l v = Validation $ \r ->
  case view l r of
    Nothing -> pure (V.Success Nothing)
    Just x  -> (Just <$>) <$> unV v x
infixr 7 ~?

--------------------------------------------------------------------------------
(.?=) :: (Functor m) => ValidationT m r (Maybe a) -> a -> ValidationT m r a
(.?=) v x = Validation (fmap (fromMaybe x <$>) . unV v)
infixr 6 .?=

--------------------------------------------------------------------------------
-- | Low-level function to run validations.
runValidation :: ValidationT m r a
                 -- ^ The validation function to use.
              -> r
                 -- ^ The value to validation (the validation focus).

              -> m (Result a)
                 -- ^ The result of validation.
runValidation = unV

--------------------------------------------------------------------------------
-- | Internal function to create the correct error type.
mkError :: (Exception e) => e -> Result a
mkError e =
  let e' = ValidationError Nothing (Text.pack $ displayException e) (toException e)
  in V.Failure (Errors $ NE.fromList [e'])
