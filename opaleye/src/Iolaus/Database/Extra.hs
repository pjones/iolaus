{-|

Copyright:
  This file is part of the package iolaus.  It is subject to the
  license terms in the LICENSE file found in the top-level directory
  of this distribution and at:

    https://code.devalot.com/open/iolaus.git

  No part of this package, including this file, may be copied,
  modified, propagated, or distributed except according to the terms
  contained in the LICENSE file.

License: BSD-2-Clause

Extra functions.

-}
module Iolaus.Database.Extra
  ( lowerEq
  , transactionTimestamp
  ) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Opaleye as O
import Opaleye.Internal.Column (Column(..))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

--------------------------------------------------------------------------------
-- | Compare text after down casing it.
--
-- @since 0.1.0.0
lowerEq :: O.Field O.SqlText -> Text -> O.Field O.SqlBool
lowerEq a b = O.lower a O..== O.sqlStrictText (Text.toLower b)

--------------------------------------------------------------------------------
-- | Current transaction time.
--
-- @since 0.1.0.0
transactionTimestamp :: O.Field O.SqlTimestamptz
transactionTimestamp = Column (HPQ.FunExpr "transaction_timestamp" [])
