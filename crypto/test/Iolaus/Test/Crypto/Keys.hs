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
import Control.Monad.Crypto.Cryptonite
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
type Table = Map (Label, FileExtension) ByteString

--------------------------------------------------------------------------------
-- | A 'KeyManager' that keeps keys in a map.  For testing only.
memoryKeys :: (MonadIO m) => m KeyManager
memoryKeys = do
    ref <- liftIO (newIORef Map.empty)
    return (KeyManager (get ref) (put ref))

  where
    get :: IORef Table -> Label -> FileExtension -> IO GetStatus
    get ref label ext = do
      table <- readIORef ref
      case Map.lookup (label, ext) table of
        Nothing -> return GetFailed
        Just bs -> return (GetSucceeded bs)

    put :: IORef Table -> Label -> FileExtension -> ByteString -> IO PutStatus
    put ref label ext bs = do
      let ent = (label, ext)
      table <- readIORef ref
      if Map.member ent table
        then return PutKeyExists
        else atomicModifyIORef ref (\t -> (Map.insert ent bs t,  PutSucceeded))
