{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-}
module Iolaus.Database.Query.Internal
  ( Query(..)
  , select
  , select1
  , insert
  , update
  , delete
  ) where

--------------------------------------------------------------------------------
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Maybe (listToMaybe)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Lens.Micro
import Opaleye (FromFields, Select, Insert, Update, Delete)
import qualified Opaleye as O
import qualified System.Metrics.Counter as Counter

--------------------------------------------------------------------------------
import Iolaus.Database.Runtime

--------------------------------------------------------------------------------
-- | An opaque type representing one or more database queries.
--
-- Query values are created with functions such as 'select', 'insert',
-- 'update', 'delete', etc.
--
-- To execute a query use one of the following modules:
--
--   * "Control.Effect.Database"
--
-- @since 0.1.0.0
newtype Query a = Query
  { runQuery :: ReaderC (Runtime, Connection) IO a }
  deriving (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- | Internal function for lifting Opaleye query types.
liftOpaleyeOp
  :: ( MonadIO m, Has (Reader (Runtime, Connection)) sig m)
  => (Connection -> IO a)
  -> m a
liftOpaleyeOp action = do
  (rt, conn) <- ask

  case rt ^. queryCounter of
    Nothing  -> pure ()
    Just cnt -> liftIO (Counter.inc cnt)

  liftIO (action conn)

--------------------------------------------------------------------------------
-- | Lift a 'Select' into a 'Query'.
--
-- @since 0.1.0.0
select :: (Default FromFields a b) => Select a -> Query [b]
select = Query . liftOpaleyeOp . flip O.runSelect

--------------------------------------------------------------------------------
-- | Like 'select' but applies a @LIMIT@ clause to the 'Select' and
-- returns the first row.
--
-- @since 0.1.0.0
select1 :: (Default FromFields a b) => Select a -> Query (Maybe b)
select1 = fmap listToMaybe . select . O.limit 1

--------------------------------------------------------------------------------
-- | Lift an 'Insert' into a 'Query'.
--
-- @since 0.1.0.0
insert :: Insert a -> Query a
insert = Query . liftOpaleyeOp . flip O.runInsert_

--------------------------------------------------------------------------------
-- | Lift an 'Update' into a 'Query'.
--
-- @since 0.1.0.0
update :: Update a -> Query a
update = Query . liftOpaleyeOp . flip O.runUpdate_

--------------------------------------------------------------------------------
-- | Lift a 'Delete' into a 'Query'.
--
-- @since 0.1.0.0
delete :: Delete a -> Query a
delete = Query . liftOpaleyeOp . flip O.runDelete_
