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
import Data.IORef
import qualified Crypto.Cipher.AES as C

--------------------------------------------------------------------------------
-- Project Imports:
import Iolaus.Crypto.Key
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
type family ToBlockCipher (t :: Cipher) where
  ToBlockCipher 'AES256 = C.AES256
  ToBlockCipher t       = C.AES256

--------------------------------------------------------------------------------
instance (MonadIO m) => MonadCrypto (CryptoniteT m) where
  data Key (CryptoniteT m) c = CryptoniteKey Label (SymmetricKey (ToBlockCipher c))
  liftCryptoOpt = evalCrypto

--------------------------------------------------------------------------------
evalCrypto :: forall m a. (MonadIO m) => CryptoOpt (CryptoniteT m) a -> CryptoniteT m a
evalCrypto opt = runF opt return $ \case
  GenerateRandom n next ->
    getRandomBytes n >>= next

  GenerateKey label next -> do
    mgr <- asks envMgr
    key@(SymmetricKey bs) <- Symmetric.generateKey
    liftIO (managerPutKey mgr label bs) >>= \case
      PutSucceeded -> next (CryptoniteKey label key)
      PutKeyExists -> throwError (KeyExistsError (getLabelText label))
      PutFailed    -> throwError (KeyWriteFailure (getLabelText label))

  FetchKey label next -> do
    mgr <- asks envMgr
    liftIO (managerGetKey mgr label) >>= \case
      GetFailed -> next Nothing
      GetSucceeded bs -> do
        key <- liftCryptoError (Symmetric.toKey bs)
        next (Just (CryptoniteKey label key))

  Encrypt (CryptoniteKey label key) value next ->
    Symmetric.encrypt label key value >>= liftCryptoError >>= next

  Decrypt (CryptoniteKey _ key) sec next ->
    liftCryptoError (Symmetric.decrypt key sec) >>= next

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
