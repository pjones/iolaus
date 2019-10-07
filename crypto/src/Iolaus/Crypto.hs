{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
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

Simplified crypto operations using MTL + classy optics.

-}

module Iolaus.Crypto
  ( Crypto
  , runCrypto
  , initCrypto
  , HasCrypto(crypto)
  , AsCryptoError(..)
  , MonadCrypto(..)

  , password
  , strength
  , hash
  , verify
  , encrypt
  , decrypt
  , saltedHash

  , Password
  , Clear
  , Strong
  , Hashed
  , Secret
  , SaltedHash
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Crypto.Cipher.Types (BlockCipher)
import Crypto.Random (MonadRandom(getRandomBytes))
import Data.Binary (Binary)
import Data.Text (Text)
import qualified Data.Time.Calendar as Time
import qualified Text.Password.Strength as Zxcvbn
import qualified Text.Password.Strength.Config as Zxcvbn

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Error (CryptoError(..), AsCryptoError(..), liftCryptoError)
import Iolaus.Crypto.Key (Key)
import Iolaus.Crypto.Password (Password, Clear, Strong, Hashed, VerifyStatus)
import qualified Iolaus.Crypto.Password as Password
import Iolaus.Crypto.Salt (SharedSalt(..))
import Iolaus.Crypto.SaltedHash (ForSaltedHash, SaltedHash)
import qualified Iolaus.Crypto.SaltedHash as SaltedHash
import Iolaus.Crypto.Symmetric (Secret)
import qualified Iolaus.Crypto.Symmetric as Symmetric

--------------------------------------------------------------------------------
-- | Environment needed for encryption.
data Crypto = Crypto
  { _pset :: Password.Settings
    -- ^ Password settings.
  }

makeClassy ''Crypto

--------------------------------------------------------------------------------
-- | Cryptography operations.
newtype CryptoOp a = CryptoOp
  { unCrypto :: ExceptT CryptoError (ReaderT Crypto IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Crypto
           )

instance MonadRandom CryptoOp where
  getRandomBytes = CryptoOp . lift . lift . getRandomBytes

--------------------------------------------------------------------------------
-- | A class of types that can perform cryptography operations.
class (Monad m) => MonadCrypto m where
  liftCrypto :: CryptoOp a -> m a

instance MonadCrypto CryptoOp where
  liftCrypto = id

--------------------------------------------------------------------------------
-- | Run a crypto operation and return the result.
--
-- As long as your monad meets all of the listed constraints you can
-- use this function to make an instance of 'MonadCrypto'.
--
-- @
--   newtype MyMonad = ...
--
--   instance MonadCrypto MyMonad where
--     liftCrypto = runCrypto
-- @
runCrypto
  :: ( MonadIO m
     , MonadError e m
     , AsCryptoError e
     , MonadReader r m
     , HasCrypto r
     )
  => CryptoOp a -> m a
runCrypto k = do
  env <- view crypto
  result <- liftIO $ flip runReaderT env $ runExceptT (unCrypto k)
  liftCryptoError result

--------------------------------------------------------------------------------
-- | Create the 'Crypto' value necessary to perform cryptography
-- operations.  Place this value in your @Reader@ environment and make
-- it an instance of 'HasCrypto'.
initCrypto
  :: ( Monad m
     )
  => m Crypto
initCrypto =
  Crypto <$> pure Password.defaultSettings -- FIXME: calculate this!

--------------------------------------------------------------------------------
-- | See 'Password.password' in "Iolaus.Crypto.Password".
password :: (Monad m) => Text -> m (Password Clear)
password = pure . Password.password

--------------------------------------------------------------------------------
-- | See 'Password.strength' in "Iolaus.Crypto.Password".
strength
  :: (Monad m)
  => Zxcvbn.Config
  -> Time.Day
  -> Password Clear
  -> m (Either Zxcvbn.Score (Password Strong))
strength c d = pure . Password.strength c d

--------------------------------------------------------------------------------
-- | See 'Password.hash' in "Iolaus.Crypto.Password".
hash
  :: ( MonadCrypto m )
  => SharedSalt
  -> Password Strong
  -> m (Password Hashed)
hash salt pc = liftCrypto $ do
  Crypto{_pset} <- ask
  Password.hash salt _pset pc

--------------------------------------------------------------------------------
-- | See 'Password.verify' in "Iolaus.Crypto.Password".
verify
  :: ( MonadCrypto m )
  => SharedSalt
  -> Password Clear
  -> Password Hashed
  -> m VerifyStatus
verify salt pc ph = liftCrypto $ do
  Crypto{_pset} <- ask
  pure $ Password.verify salt _pset pc ph

--------------------------------------------------------------------------------
-- | See 'Symmetric.encrypt' in "Iolaus.Crypto.Symmetric".
encrypt
  :: ( MonadCrypto m
     , Binary a
     , BlockCipher c
     )
  => Key c
  -> a
  -> m (Secret c a)
encrypt key x = liftCrypto $ (Symmetric.encrypt key x >>= CryptoOp . liftEither)

--------------------------------------------------------------------------------
-- | See 'Symmetric.decrypt' in "Iolaus.Crypto.Symmetric".
decrypt
  :: ( MonadCrypto m
     , Binary a
     , BlockCipher c
     )
  => Key c
  -> Secret c a
  -> m a
decrypt key x = liftCrypto $ CryptoOp $ liftEither (Symmetric.decrypt key x)

--------------------------------------------------------------------------------
-- | See 'SaltedHash.saltedHash' in "Iolaus.Crypto.SaltedHash".
saltedHash
  :: ( MonadCrypto m
     , ForSaltedHash a
     )
  => SharedSalt
  -> a
  -> m (SaltedHash a)
saltedHash salt x = liftCrypto $ pure $ SaltedHash.saltedHash salt x
