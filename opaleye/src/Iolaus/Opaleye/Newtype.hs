{-# LANGUAGE TemplateHaskell       #-}

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

Use newtype wrappers as database columns in Opaleye.

-}
module Iolaus.Opaleye.Newtype
  ( makeNewtypeInstances
  ) where

--------------------------------------------------------------------------------
import qualified Language.Haskell.TH as TH
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Data.Profunctor.Product.Default (Default(def))

import Opaleye
  ( Constant(..)
  , Column
  , QueryRunnerColumnDefault(..)
  , fieldQueryRunnerColumn
  , toFields
  )

--------------------------------------------------------------------------------
-- | Generate all necessary instances so that a newtype wrapper can be
-- used as a column in a table (as long as it wraps something that can
-- be used as a column):
--
-- > newtype Name = Name { unName :: Text }
-- >
-- > makeNewtypeInstances ''Name ''SqlText
--
-- Now the @Name@ type can be used as a column via @SqlText@.
makeNewtypeInstances :: TH.Name -> TH.Name -> TH.Q [TH.Dec]
makeNewtypeInstances nt col = do
  info <- TH.reify nt

  case info of
    TH.TyConI (TH.NewtypeD _ _ _ _ c _) ->
      case c of
        TH.NormalC nc _  -> q nc
        TH.RecC nc _     -> q nc
        TH.InfixC _ nc _ -> q nc
        _               -> error "newtype is too fancy for me"
    _ -> error "expected newtype"

  where
    q :: TH.Name -> TH.Q [TH.Dec]
    q nc =
      [d|
        instance FromField $(TH.conT nt) where
          fromField f b = $(TH.conE nc) <$> fromField f b

        instance QueryRunnerColumnDefault $(TH.conT col) $(TH.conT nt) where
          queryRunnerColumnDefault = fieldQueryRunnerColumn

        instance Default Constant $(TH.conT nt) (Column $(TH.conT col)) where
          def = let unwrap $(TH.conP nc [pure (TH.VarP $ TH.mkName "x")]) = x
                in Constant (toFields . unwrap)
      |]
