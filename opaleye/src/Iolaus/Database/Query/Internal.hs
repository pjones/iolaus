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
  , selectToStream
  , count
  , insert
  , insert1
  , update
  , delete
  ) where

--------------------------------------------------------------------------------
import Control.Exception (onException)
import Control.Lens ((^.))
import Control.Monad (MonadPlus(..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple.Transaction as Pg
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
  { runQuery :: ReaderT (Runtime, Connection) IO a }
  deriving newtype (Functor, Applicative, Monad)

--------------------------------------------------------------------------------
-- | Internal function for lifting Opaleye query types.
liftOpaleyeOp
  :: ( MonadIO m
     , MonadReader (Runtime, Connection) m
     )
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
-- | Like 'select' but applies a @LIMIT 1@ clause to the 'Select' and
-- returns the first row.
--
-- @since 0.1.0.0
select1 :: (Default FromFields a b) => Select a -> Query (Maybe b)
select1 = fmap listToMaybe . select . O.limit 1

--------------------------------------------------------------------------------
-- | Like 'select', except that the results are streamed on demand.
--
-- A @ListT@ implementation is needed to read the results of the
-- stream.  Compatible packages include:
--
--   * <https://hackage.haskell.org/package/List List>
--   * <https://hackage.haskell.org/package/list-t list-t>
--   * <https://hackage.haskell.org/package/logict logict>
--   * <https://hackage.haskell.org/package/list-transformer list-transformer>
--   * <https://hackage.haskell.org/package/pipes pipes>
--
-- Using a @ListT@ implementation ensures that all rows are processed
-- in constant space and that only one row is in memory at a time.
--
-- NOTE: Internally this uses a database cursor.  Therefore it needs
-- to create a transaction that will remain open until all rows have
-- been read.  As a consequence, this function /cannot/ be used from
-- within an existing transaction.  One of the @runQuery@ variants
-- must be used to execute this query.
--
-- @since 0.1.0.0
selectToStream
  :: forall m a b. (Default FromFields a b, MonadPlus m, MonadIO m)
  => Select a
  -> Query (m b)
selectToStream = Query . go
  where
    go :: forall k.
       ( MonadIO k
       , MonadReader (Runtime, Connection) k
       )
       => Select a -> k (m b)
    go query = do
      (_, conn) <- ask
      cursor <- liftIO (Pg.begin conn >> O.declareCursor conn query) :: k (O.Cursor b)

      let closeC   = O.closeCursor cursor >> Pg.commit conn
          foldF    = O.foldForward cursor 1 (const pure) undefined
          foldSafe = onException foldF (Pg.rollback conn)

      pure (loop closeC foldSafe)

    loop :: IO () -> IO (Either b b) -> m b
    loop closeC foldC =
      liftIO foldC >>= \case
        Left  _ -> liftIO closeC >> mzero
        Right x -> pure x `mplus` loop closeC foldC

--------------------------------------------------------------------------------
-- | Count all of the rows returned from the given 'Select'.
--
-- This is equivalent to a @SELECT COUNT(*) ...@ when there is no
-- @GROUP BY@ clause.
--
-- @since 0.1.0.0
count :: Select a -> Query Int64
count = fmap (fromMaybe 0 . listToMaybe) . select . O.countRows

--------------------------------------------------------------------------------
-- | Lift an 'Insert' into a 'Query'.
--
-- @since 0.1.0.0
insert :: Insert a -> Query a
insert = Query . liftOpaleyeOp . flip O.runInsert_

--------------------------------------------------------------------------------
-- | Perform an insert, clamping the output to a single row.
insert1 :: Insert [a] -> Query (Maybe a)
insert1 = fmap listToMaybe . insert

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
