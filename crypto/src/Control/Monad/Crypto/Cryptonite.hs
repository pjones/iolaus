{-# LANGUAGE UndecidableInstances #-}

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

This module provides the 'CryptoniteT' monad transformer which
implements the 'MonadCrypto' class.

-}
module Control.Monad.Crypto.Cryptonite
  ( -- * CryptoniteT
    CryptoniteT
  , Cryptonite
  , fileManager
  , initCryptoniteT
  , runCryptoniteT
  , runCryptoniteT'

    -- * Re-exports
  , KeyManager(..)
  , FileExtension(..)
  , GetStatus(..)
  , PutStatus(..)
  , module Control.Monad.Crypto.Class
  , module Iolaus.Crypto.Error
  , module Iolaus.Crypto.Key
  , module Iolaus.Crypto.Secret
  , module Iolaus.Crypto.Signature
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Lens (view)
import Control.Lens.TH (makeClassy)
import Control.Monad.Database.Class (MonadDatabase)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random (MonadRandom(..), DRG(..), ChaChaDRG, drgNew)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (listToMaybe)

--------------------------------------------------------------------------------
-- For MTL Instances:
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Class

--------------------------------------------------------------------------------
-- Project Imports:
import Control.Monad.CertAuth.Internal
import Control.Monad.Crypto.Class
import Control.Monad.Crypto.KeyAccess
import Iolaus.Crypto.Cryptonite.Asymmetric as Asymmetric
import Iolaus.Crypto.Cryptonite.Symmetric as Symmetric
import Iolaus.Crypto.Internal.Error
import Iolaus.Crypto.Internal.Key
import Iolaus.Crypto.PEM

--------------------------------------------------------------------------------
-- For Exporting:
import Iolaus.Crypto.Error
import Iolaus.Crypto.Key
import Iolaus.Crypto.Secret
import Iolaus.Crypto.Signature

import Control.Monad.Crypto.Internal
  ( MonadCrypto(..)
  , CryptoOpt
  , CryptoOptF(..)
  , KeyManager(..)
  , FileExtension(..)
  , GetStatus(..)
  , PutStatus(..)
  )

--------------------------------------------------------------------------------
-- | Save me some typing.
type RNG = IORef ChaChaDRG

--------------------------------------------------------------------------------
data Cryptonite = Cryptonite
  { _envRG  :: RNG
  , _envMgr :: KeyManager
  }

makeClassy ''Cryptonite

--------------------------------------------------------------------------------
-- | An internal transformer for random bytes.
newtype RandomT m a = RandomT
  { unR :: ReaderT RNG m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadTrans
                   , MonadError e
                   , MonadState s
                   , MonadCont
                   , MonadCertAuth
                   , MonadDatabase
                   )

runRandomT :: RNG -> RandomT m a -> m a
runRandomT env = (`runReaderT` env) . unR

--------------------------------------------------------------------------------
instance (MonadReader r m) => MonadReader r (RandomT m) where
  ask   = lift ask
  reader = lift . reader
  local f m = do
    env <- RandomT (ask :: ReaderT RNG m RNG)
    lift (local f (runRandomT env m))

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadRandom (RandomT m) where
  getRandomBytes n = do
    slot <- RandomT ask
    gen  <- liftIO (readIORef slot)
    let (bytes, gen') = randomBytesGenerate n gen
    liftIO (writeIORef slot gen')
    pure bytes

--------------------------------------------------------------------------------
-- | An implementation of 'MonadCrypto' via the Cryptonite library.
newtype CryptoniteT m a = CryptoniteT
  { unC :: RandomT (ExceptT CryptoError (ReaderT Cryptonite m)) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad, MonadIO
                   , MonadRandom
                   , MonadState s
                   , MonadCont
                   , MonadCertAuth
                   , MonadDatabase
                   )

--------------------------------------------------------------------------------
data instance Key Cryptonite = CryptoniteKey Label SymmetricKey
data instance KeyPair Cryptonite = CryptoniteKeyPair Label AsymmetricKey

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadCrypto Cryptonite (CryptoniteT m) where
  liftCryptoOpt = evalCrypto

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadKeyAccess Cryptonite (CryptoniteT m) where
  liftKeyAccessOpt = evalKeyAccess

--------------------------------------------------------------------------------
instance MonadTrans CryptoniteT where
  lift = CryptoniteT . lift . lift . lift

--------------------------------------------------------------------------------
instance (MonadError e m, AsCryptoError e) => MonadError e (CryptoniteT m) where
   throwError = lift . throwError
   catchError m f = do
     env <- CryptoniteT ask
     lift (catchError (runCryptoniteT' env m) (runCryptoniteT' env . f))

--------------------------------------------------------------------------------
instance (MonadReader r m) => MonadReader r (CryptoniteT m) where
  ask   = lift ask
  reader = lift . reader
  local f m = do
    env <- CryptoniteT (ask :: RandomT (ExceptT CryptoError (ReaderT Cryptonite m)) Cryptonite)
    lift (local f (runCryptoniteT env m)) >>= \case
      Left e  -> CryptoniteT (throwError e)
      Right a -> pure a

--------------------------------------------------------------------------------
evalCrypto
  :: ( MonadIO m )
  => CryptoOpt Cryptonite a
  -> CryptoniteT m a
evalCrypto opt = CryptoniteT . runF opt pure $ \case
  GenerateRandomBytes n next ->
    getRandomBytes n >>= next

  GenerateKey cipher label next -> do
    key <- Symmetric.generateKey cipher
    putKey label KeyExt (Symmetric.fromKey key)
    next (CryptoniteKey label key)

  FetchKey label next ->
    getKey label KeyExt >>= \case
      Nothing -> next Nothing
      Just bs -> do
        key <- liftCryptoError (Symmetric.toKey label bs)
        next (Just (CryptoniteKey label key))

  Encrypt (CryptoniteKey label key) value next ->
    Symmetric.encrypt label key value >>= liftCryptoError >>= next

  Decrypt (CryptoniteKey _ key) sec next ->
    liftCryptoError (Symmetric.decrypt key sec) >>= next

  GenerateKeyPair algo label next -> do
    key <- Asymmetric.generateKeyPair algo label
    putKey label PrivateExt (Asymmetric.fromKey key)
    next (CryptoniteKeyPair label key)

  FetchKeyPair label next ->
    getKey label PrivateExt >>= \case
      Nothing -> next Nothing
      Just bs -> do
        key <- liftCryptoError (Asymmetric.toKey label bs)
        next (Just (CryptoniteKeyPair label key))

  ToPublicKey (CryptoniteKeyPair _ key) next ->
    next (Asymmetric.toPublicKey key)

  AsymmetricEncrypt key bs next ->
    Asymmetric.encrypt key bs >>= next

  AsymmetricDecrypt (CryptoniteKeyPair _ key) secret next ->
    Asymmetric.decrypt key secret >>= next

  AsymmetricSign (CryptoniteKeyPair _ key) hash bs next ->
    Asymmetric.sign key hash bs >>= next

  VerifySignature pub sig bs next ->
    Asymmetric.verify pub sig bs >>= next

--------------------------------------------------------------------------------
evalKeyAccess :: MonadIO m => KeyAccessOpt Cryptonite a -> CryptoniteT m a
evalKeyAccess opt = CryptoniteT . runF opt pure $ \case
  EncodeKey (CryptoniteKey l k) next ->
    next (Binary.encode (l, k))

  DecodeKey bs next ->
    case Binary.decodeOrFail bs of
      Left _ -> next Nothing
      Right (_, _, (l, k)) -> next (Just (CryptoniteKey l k))

  EncodePrivateKey (CryptoniteKeyPair _ k) next ->
    next (encodePEM [toPEM PrivateKeySection (Asymmetric.toX509PrivKey k)])

  DecodePrivateKey label bs next ->
    next (fmap (CryptoniteKeyPair (toLabel "None")) .
          (Asymmetric.fromX509PrivKey label <=< listToMaybe) .
            concatMap fromPEM $ decodePEM bs)

--------------------------------------------------------------------------------
putKey
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , AsCryptoError e
     , HasCryptonite r
     )
  => Label
  -> FileExtension
  -> ByteString
  -> m ()
putKey label ext bs = do
  mgr <- view envMgr
  liftIO (managerPutKey mgr label ext bs) >>= \case
    PutSucceeded -> pure ()
    PutKeyExists -> throwing _KeyExistsError (getLabelText label)
    PutFailed    -> throwing _KeyWriteFailure (getLabelText label)

--------------------------------------------------------------------------------
getKey
  :: ( MonadIO m
     , MonadReader r m
     , HasCryptonite r
     )
  => Label
  -> FileExtension
  -> m (Maybe ByteString)
getKey label ext = do
  mgr <- view envMgr
  liftIO (managerGetKey mgr label ext) >>= \case
    GetFailed -> pure Nothing
    GetSucceeded bs -> pure (Just bs)

--------------------------------------------------------------------------------
-- | Create a configuration value needed to run cryptographic operations.
initCryptoniteT
  :: (MonadIO m)
  => KeyManager
  -> m Cryptonite
initCryptoniteT mgr =
  Cryptonite <$> liftIO (drgNew >>= newIORef)
             <*> pure mgr

--------------------------------------------------------------------------------
-- | Run all cryptographic operations and return their result.
runCryptoniteT
  :: Cryptonite
  -> CryptoniteT m a
  -> m (Either CryptoError a)
runCryptoniteT config =
  (`runReaderT` config) . runExceptT . runRandomT (view envRG config) . unC

--------------------------------------------------------------------------------
-- | A variant of 'runCryptoniteT' that uses 'MonadError' instead of 'Either'.
runCryptoniteT'
  :: ( MonadError e m
     , AsCryptoError e
     )
  => Cryptonite
  -> CryptoniteT m a
  -> m a
runCryptoniteT' c = runCryptoniteT c >=> liftCryptoError
