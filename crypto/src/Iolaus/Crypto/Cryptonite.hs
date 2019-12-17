{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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
module Iolaus.Crypto.Cryptonite
  ( CryptoniteT
  , Config
  , fileManager
  , initCryptoniteT
  , runCryptoniteT
  , runCryptoniteT'
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Except
import Control.Monad.Free.Church (runF)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Crypto.Random (MonadRandom(..), DRG(..), ChaChaDRG, drgNew)
import Data.ByteString (ByteString)
import Data.IORef

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
import Iolaus.Crypto.Cryptonite.Asymmetric as Asymmetric
import Iolaus.Crypto.Cryptonite.Symmetric as Symmetric
import Iolaus.Crypto.Error
import Iolaus.Crypto.Monad

--------------------------------------------------------------------------------
data Config = Config
  { envRG  :: IORef ChaChaDRG
  , envMgr :: KeyManager
  }

--------------------------------------------------------------------------------
-- | An implementation of 'MonadCrypto' via the Cryptonite library.
newtype CryptoniteT m a = CryptoniteT
  { unC :: ExceptT CryptoError (ReaderT Config m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError CryptoError
           , MonadReader Config
           , MonadIO
           )

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadRandom (CryptoniteT m) where
  getRandomBytes n = do
    slot <- asks envRG
    gen  <- liftIO (readIORef slot)
    let (bytes, gen') = randomBytesGenerate n gen
    liftIO (writeIORef slot gen')
    return bytes

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadCrypto (CryptoniteT m) where
  data Key (CryptoniteT m) = CryptoniteKey Label SymmetricKey
  data KeyPair (CryptoniteT m) = CryptoniteKeyPair Label AsymmetricKey
  liftCryptoOpt = evalCrypto

--------------------------------------------------------------------------------
evalCrypto
  :: forall m a. (MonadIO m)
  => CryptoOpt (CryptoniteT m) a
  -> CryptoniteT m a
evalCrypto opt = runF opt return $ \case
  GenerateRandom n next ->
    getRandomBytes n >>= next

  GenerateKey cipher label next -> do
    key <- Symmetric.generateKey cipher
    putKey label (Symmetric.fromKey key)
    next (CryptoniteKey label key)

  FetchKey cipher label next ->
    getKey label >>= \case
      Nothing -> next Nothing
      Just bs -> do
        key <- liftCryptoError (Symmetric.toKey cipher label bs)
        next (Just (CryptoniteKey label key))

  Encrypt (CryptoniteKey label key) value next ->
    Symmetric.encrypt label key value >>= liftCryptoError >>= next

  Decrypt (CryptoniteKey _ key) sec next ->
    liftCryptoError (Symmetric.decrypt key sec) >>= next

  GenerateKeyPair algo label next -> do
    key <- Asymmetric.generateKeyPair algo
    putKey label (Asymmetric.fromKey key)
    next (CryptoniteKeyPair label key)

  FetchKeyPair algo label next ->
    getKey label >>= \case
      Nothing -> next Nothing
      Just bs -> do
        key <- liftCryptoError (Asymmetric.toKey algo label bs)
        next (Just (CryptoniteKeyPair label key))

  ToPublicKey (CryptoniteKeyPair _ key) next ->
    next (Asymmetric.toPublicKey key)

  AsymmetricEncrypt label key bs next ->
    Asymmetric.encrypt label key bs >>= next

  AsymmetricDecrypt (CryptoniteKeyPair _ key) secret next ->
    Asymmetric.decrypt key secret >>= next

  AsymmetricSign (CryptoniteKeyPair _ key) hash bs next ->
    Asymmetric.sign key hash bs >>= next

  VerifySignature pub sig bs next ->
    Asymmetric.verify pub sig bs >>= next

  where
    putKey :: Label -> ByteString -> CryptoniteT m ()
    putKey label bs = do
      mgr <- asks envMgr
      liftIO (managerPutKey mgr label bs) >>= \case
        PutSucceeded -> return ()
        PutKeyExists -> throwError (KeyExistsError (getLabelText label))
        PutFailed    -> throwError (KeyWriteFailure (getLabelText label))

    getKey :: Label -> CryptoniteT m (Maybe ByteString)
    getKey label = do
      mgr <- asks envMgr
      liftIO (managerGetKey mgr label) >>= \case
        GetFailed -> return Nothing
        GetSucceeded bs -> return (Just bs)

--------------------------------------------------------------------------------
-- | Create a configuration value needed to run cryptographic operations.
initCryptoniteT
  :: (MonadIO m)
  => KeyManager
  -> m Config
initCryptoniteT mgr =
  Config <$> liftIO (drgNew >>= newIORef)
         <*> pure mgr

--------------------------------------------------------------------------------
-- | Run all cryptographic operations and return their result.
runCryptoniteT
  :: Config
  -> CryptoniteT m a
  -> m (Either CryptoError a)
runCryptoniteT config = (`runReaderT` config) . runExceptT . unC

--------------------------------------------------------------------------------
-- | A variant of 'runCryptoniteT' that uses 'MonadError' instead of 'Either'.
runCryptoniteT'
  :: ( MonadError e m
     , AsCryptoError e
     )
  => Config
  -> CryptoniteT m a
  -> m a
runCryptoniteT' c = runCryptoniteT c >=> liftCryptoError
