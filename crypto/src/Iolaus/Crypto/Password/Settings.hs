{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

{-|

Copyright:
  This file is part of the package iolaus. It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: Apache-2.0

Password generation settings.

-}
module Iolaus.Crypto.Password.Settings
  ( Settings(..)
  , defaultSettings
  , forPBKDF2
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Data.Aeson (FromJSON, ToJSON)
import Dhall (Interpret)
import GHC.Generics
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
-- | Settings to control the password generation process.
data Settings = Settings
  { iterations :: Natural -- ^ Number of iterations for the algorithm.
  , bytes      :: Natural -- ^ Size of the output in bytes.
  } deriving (Generic, Eq, Ord, Show, Interpret, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- | Default settings.
--
-- These are based on the recommendations in the NIST Special
-- Publication 800-63B.  Attempting to use values weaker than the
-- defaults will cause the hashing function to silently upgrade to the
-- values given by this function.
defaultSettings :: Settings
defaultSettings =
  Settings { iterations = 10000 -- As per NIST 800-63b
           , bytes      = 32    -- 256 bits
           }

--------------------------------------------------------------------------------
-- | Convert the settings into PBKDF2 parameters.
forPBKDF2 :: Settings -> PBKDF2.Parameters
forPBKDF2 s =
  PBKDF2.Parameters { PBKDF2.iterCounts   = fromIntegral iterations'
                    , PBKDF2.outputLength = fromIntegral bytes'
                    }

  where
    -- Disallow going lower than the default values.
    iterations' = max (iterations s) (iterations defaultSettings)
    bytes'      = max (bytes s)      (bytes defaultSettings)
