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
  , CryptoError(..)
  , AsCryptoError(..)
  , MonadCrypto(..)

  , Password.password
  , passwordM
  , Password.strength
  , strengthM
  , hash
  , verify
  , generateKey
  , encrypt
  , decrypt
  , generateSalt
  , encodeSalt
  , decodeSalt
  , saltedHash
  , hashedSecret

  , Password
  , Clear
  , Strong
  , Hashed
  , VerifyStatus(..)
  , Secret
  , SaltedHash
  , HashedSecret(..)

  , Key
  , Salt
  , SharedSalt(..)

  , Cipher
  , BlockCipher
  , DefaultCipher
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Crypto.Random (MonadRandom(..), DRG(..), ChaChaDRG, drgNew)
import Data.Binary (Binary)
import Data.IORef
import Data.Text (Text)
import qualified Data.Time.Calendar as Time
import qualified Text.Password.Strength as Zxcvbn
import qualified Text.Password.Strength.Config as Zxcvbn

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Cipher
import Iolaus.Crypto.Error (CryptoError(..), AsCryptoError(..), liftCryptoError)
import Iolaus.Crypto.HashedSecret (HashedSecret)
import qualified Iolaus.Crypto.HashedSecret as HashedSecret
import Iolaus.Crypto.Key (Key)
import qualified Iolaus.Crypto.Key as Key
import Iolaus.Crypto.Password (Password, Clear, Strong, Hashed, VerifyStatus)
import qualified Iolaus.Crypto.Password as Password
import Iolaus.Crypto.Salt (Salt, SharedSalt(..))
import qualified Iolaus.Crypto.Salt as Salt
import Iolaus.Crypto.SaltedHash (ForSaltedHash, SaltedHash)
import qualified Iolaus.Crypto.SaltedHash as SaltedHash
import Iolaus.Crypto.Symmetric (Secret)
import qualified Iolaus.Crypto.Symmetric as Symmetric

--------------------------------------------------------------------------------
-- | Environment needed for encryption.
data Crypto = Crypto
  { _pset :: Password.Settings
    -- ^ Password settings.

  , _drg :: IORef ChaChaDRG
    -- ^ Random number generator.
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
  getRandomBytes n = do
      slot <- view drg
      gen  <- io (readIORef slot)

      let (bytes, gen') = randomBytesGenerate n gen

      io (writeIORef slot gen')
      pure bytes
    where io = CryptoOp . lift . lift

--------------------------------------------------------------------------------
-- | A class of types that can perform cryptography operations.
class (Monad m) => MonadCrypto m where
  liftCrypto :: CryptoOp a -> m a

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
  :: ( MonadIO m
     )
  => m Crypto
initCrypto =
  Crypto <$> pure Password.defaultSettings -- FIXME: calculate this!
         <*> liftIO (drgNew >>= newIORef)

--------------------------------------------------------------------------------
-- | Monadic version of 'Password.password'.
passwordM :: (Monad m) => Text -> m (Password Clear)
passwordM = pure . Password.password

--------------------------------------------------------------------------------
-- | Monadic version of 'Password.strength'.
strengthM
  :: (Monad m)
  => Zxcvbn.Config
  -> Time.Day
  -> Password Clear
  -> m (Either Zxcvbn.Score (Password Strong))
strengthM c d = pure . Password.strength c d

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
-- | Generate a new encryption key.
generateKey
  :: ( MonadCrypto m
     , Cipher c
     )
  => m (Key c)
generateKey = liftCrypto Key.generate'

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
encrypt key x = liftCrypto (Symmetric.encrypt key x >>= CryptoOp . liftEither)

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
-- | Generate salt using the recommended length.
generateSalt
  :: ( MonadCrypto m )
  => m Salt
generateSalt = liftCrypto Salt.generate

--------------------------------------------------------------------------------
-- | Encode salt so that it can be stored in a file or database.
encodeSalt :: Salt -> Text
encodeSalt = Salt.encode

--------------------------------------------------------------------------------
-- | Decode salt from 'Text'.
decodeSalt
  :: ( MonadCrypto m )
  => Text
  -> m Salt
decodeSalt = liftCrypto . CryptoOp . liftEither . Salt.pack

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

--------------------------------------------------------------------------------
-- | See 'HashedSecret.hashedSecret' in "Iolaus.Crypto.HashedSecret".
hashedSecret
  :: ( MonadCrypto m
     , Binary a
     , BlockCipher c
     , ForSaltedHash a
     )
  => Key c
  -> SharedSalt
  -> a
  -> m (HashedSecret c a)
hashedSecret key salt x =
  liftCrypto (HashedSecret.hashedSecret key salt x >>= CryptoOp . liftEither)
