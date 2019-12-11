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

In-memory keys.

-}
module Iolaus.Test.Crypto.Keys
  ( memoryKeys
  ) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Iolaus.Crypto
import Iolaus.Crypto.Monad

--------------------------------------------------------------------------------
-- | A 'KeyManager' that keeps keys in a map.  For testing only.
memoryKeys :: (MonadIO m) => m KeyManager
memoryKeys = do
    ref <- liftIO (newIORef Map.empty)
    return (KeyManager (get ref) (put ref))

  where
    get :: IORef (Map Label ByteString) -> Label -> IO GetStatus
    get ref label = do
      table <- readIORef ref
      case Map.lookup label table of
        Nothing -> return GetFailed
        Just bs -> return (GetSucceeded bs)

    put :: IORef (Map Label ByteString) -> Label -> ByteString -> IO PutStatus
    put ref label bs = do
      table <- readIORef ref
      if Map.member label table
        then return PutKeyExists
        else atomicModifyIORef ref (\t -> (Map.insert label bs t,  PutSucceeded))
