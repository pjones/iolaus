{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

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

Types for selecting ciphers at compile time.

-}
module Iolaus.Crypto.Cipher
  ( Ciphers(..)
  , Cipher
  , DefaultCipher
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.Cipher.AES as Cryptonite

--------------------------------------------------------------------------------
-- | Supported ciphers.
data Ciphers
  = AES256 -- ^ https://en.wikipedia.org/wiki/Advanced_Encryption_Standard

--------------------------------------------------------------------------------
-- | Type function to translate the 'Ciphers' type to a matching type
-- in the Cryptonite library.
type family Cipher a :: * where
  Cipher 'AES256 = Cryptonite.AES256

--------------------------------------------------------------------------------
-- | The default cipher used by this library.
type DefaultCipher = Cipher 'AES256
