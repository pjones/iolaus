{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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

This is an example of encrypting a value using a symmetric cipher.

-}
module Main (main) where

--------------------------------------------------------------------------------
-- Load in our dependencies:
import Control.Monad.Crypto.Cryptonite
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString

--------------------------------------------------------------------------------
-- | An example transformer stack.  You can add as many transformers
-- as necessary as long as one of them implements 'MonadCrypto'.  Here
-- we are going to use 'CryptoniteT' for software-based encryption.
newtype App a = App
  { runApp :: CryptoniteT IO a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadCrypto Cryptonite
           )

--------------------------------------------------------------------------------
-- | An example application.
--
-- Notice that our application is polymorphic in its monad.  This
-- limits it to only those effects that are listed as class
-- constraints, and we can easily choose a different cryptography
-- backend as needed.
--
-- NOTE: 'MonadIO' is only needed here for the use of 'putStrLn'.  In
-- a real application you would probably want to avoid using 'MonadIO'
-- since it allows your application to do anything.
app :: (MonadIO m, MonadCrypto k m) => m ()
app = do
  let label  = toLabel "my symmetric key label"
      cipher = AES256
      number = 42 :: Int

  keyM <- fetchKey label
  key  <- maybe (generateKey cipher label) pure keyM

  liftIO (putStrLn ("key file is: " <> ByteString.unpack (getLabel label)))

  secretNumber <- encryptBinary key number
  liftIO (LByteString.putStrLn (Aeson.encode secretNumber))

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- We're going to store keys on the file system in the current directory.
  manager <- fileManager "."
  crypto <- initCryptoniteT manager
  runCryptoniteT crypto (runApp app) >>= print
