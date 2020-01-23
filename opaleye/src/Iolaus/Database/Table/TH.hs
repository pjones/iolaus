{-# LANGUAGE TemplateHaskell #-}

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

Here be dragons.

-}
module Iolaus.Database.Table.TH
  ( makeTable
  ) where

--------------------------------------------------------------------------------
import Control.Monad (forM, foldM)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Profunctor (lmap)
import Data.Profunctor.Product (ProductProfunctor, (***$), (****))
import Data.Profunctor.Product.Default (Default(..))
import Iolaus.Database.Table.Internal
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Opaleye (Table, table, tableField)
import Opaleye.Map (Map)
import Opaleye.TypeFamilies ((:<*>), (:<$>), IMap, F)

--------------------------------------------------------------------------------
data TableInfo = TableInfo
  { tbl_tctor   :: Name               -- ^ Type constructor.
  , tbl_dctor   :: Name               -- ^ Data constructor.
  , tbl_fields  :: NonEmpty FieldInfo -- ^ Record fields.
  , tbl_adname  :: Name               -- ^ Adapter name.
  , tbl_fnname  :: Name               -- ^ Name of function that returns a table.
  , tbl_sqlname :: String             -- ^ Name of the database table.
  }

--------------------------------------------------------------------------------
data FieldInfo = FieldInfo
  { fld_name    :: Name -- ^ Name of the record field
  , fld_tyvar   :: Type -- ^ Type variable.
  , fld_colname :: Type -- ^ Column name (symbol).
  , fld_haskty  :: Type -- ^ Haskell type.
  , fld_sqlty   :: Type -- ^ SQL type.
  , fld_mode    :: Type -- ^ Write mode.
  }

--------------------------------------------------------------------------------
nameToTableInfo :: Name -> String -> Q TableInfo
nameToTableInfo name tbl = reify name >>= \case
    TyConI (DataD _ _ _ _ [ctor] _) -> fromCtor ctor
    _ -> fail "makeTable only supports type constructors with a single data constructor"
  where
    fromCtor :: Con -> Q TableInfo
    fromCtor = \case
      RecC n fs -> fromRec n fs
      _ -> fail "makeTable only supports data types that are records"

    fromRec :: Name -> [VarBangType] -> Q TableInfo
    fromRec dctor fs = do
      flds <- mapM (\(n, _, t) -> fieldToFieldInfo (n, t)) fs

      pure TableInfo
        { tbl_tctor   = name
        , tbl_dctor   = dctor
        , tbl_fields  = NonEmpty.fromList flds
        , tbl_adname  = mkName . ("p" <>) . nameBase $ name
        , tbl_fnname  = mkName tbl
        , tbl_sqlname = tbl
        }

--------------------------------------------------------------------------------
fieldToFieldInfo :: (Name, Type) -> Q FieldInfo
fieldToFieldInfo (name, (AppT (AppT (AppT (AppT (AppT (ConT t) v) n) h) d) m))
  | t == ''Col = pure (FieldInfo name v n h d m)
fieldToFieldInfo _ = fail "record field should have the form: Col f name hask sql write"

--------------------------------------------------------------------------------
fieldInfoToType :: FieldInfo -> Type
fieldInfoToType FieldInfo{..} =
  ConT ''Col `AppT` fld_tyvar
             `AppT` fld_colname
             `AppT` fld_haskty
             `AppT` fld_sqlty
             `AppT` fld_mode

--------------------------------------------------------------------------------
-- | Given a record type and a SQL table name, generate the following:
--
--   * The @ProductProfunctor@ adapter function
--   * An instance for the @Default@ class
--   * A type family instance for the @Map@ family
--   * A function to create a @Table@ value.
--
-- This replaces the @makeAdaptorAndInstance@ function.
makeTable :: Name -> String -> Q [Dec]
makeTable name tbl = do
  t <- nameToTableInfo name tbl

  concat <$> sequence
    [ makeAdapter t
    , makeDefaultInstance t
    , makeMapInstance t
    , makeTableDefinition t
    ]

--------------------------------------------------------------------------------
-- | Creates the @ProductProfunctor@ adapter.
makeAdapter :: TableInfo -> Q [Dec]
makeAdapter TableInfo{..} = do
    asig <- adapterSig tbl_adname
    adef <- adapterDef tbl_adname
    pure [asig, adef]
  where
    adapterSig :: Name -> Q Dec
    adapterSig fname = do
      p <- newName "p"
      a <- newName "a"
      b <- newName "b"

      let scope = map PlainTV [p, a, b]
          -- ProductProfunctor p =>
          ctx  = [AppT (ConT ''ProductProfunctor) (VarT p)]

          -- (p :<$> a :<*> b)
          pmap = AppT (AppT (ConT ''(:<*>))
                   (AppT (AppT (ConT ''(:<$>)) (VarT p)) (VarT a)))
                     (VarT b)

          -- Person (p :<$> a :<*> b)
          ctor = AppT (ConT tbl_tctor) pmap

          -- p (Person a, Person b)
          ptup = AppT (AppT (VarT p)
                        (AppT (ConT tbl_tctor) (VarT a)))
                        (AppT (ConT tbl_tctor) (VarT b))

          -- Person (...) -> p (Person a, Person b)
          aType = AppT (AppT ArrowT ctor) ptup
      pure (SigD fname (ForallT scope ctx aType))

    adapterDef :: Name -> Q Dec
    adapterDef fname = do
      var  <- newName "x"
      body <- pmaps var
      pure (FunD fname [Clause [VarP var] (NormalB body) []])

    -- Applies the @lmap@ function to the table field:
    --
    -- @
    --   lmap field (field arg)
    -- @
    applyLmap :: Name -> Name -> Exp
    applyLmap arg field =
      VarE 'lmap `AppE`
        VarE field `AppE`
          (VarE field `AppE` VarE arg)

    -- Applies the data constructor to the product profunctor map and
    -- apply operators for each field.
    --
    -- @
    --   -- Infix
    --   Person ***$ lmap1 **** lmap2 **** lmap3
    --
    --   -- Postfix
    --   ((****) ((****) ((***$) Person lmap1) lmap2) lmap3)
    -- @
    pmaps :: Name -> Q Exp
    pmaps arg =
      let lmaps     = fmap (applyLmap arg . fld_name) tbl_fields
          inner     = VarE '(***$) `AppE` ConE tbl_dctor `AppE` NonEmpty.head lmaps
          embed f s = VarE '(****) `AppE` s `AppE` f
      in pure (foldr embed inner (reverse (NonEmpty.tail lmaps)))

--------------------------------------------------------------------------------
-- | Generate the instance for the @Default@ class.
makeDefaultInstance :: TableInfo -> Q [Dec]
makeDefaultInstance TableInfo{..} = do
    p <- newName "p"
    a <- newName "a"
    b <- newName "b"
    fcxt <- fieldCxt p a b

    let cxts  = prodproCxt p <> fcxt
        itype = ConT ''Default `AppT` VarT p `AppT`
                  (ConT tbl_tctor `AppT` VarT a) `AppT`
                  (ConT tbl_tctor `AppT` VarT b)

    pure [InstanceD Nothing cxts itype [defMethod]]

  where
    prodproCxt :: Name -> Cxt
    prodproCxt p = [AppT (ConT ''ProductProfunctor) (VarT p)]

    fieldCxt :: Name -> Name -> Name -> Q Cxt
    fieldCxt p a b = forM (NonEmpty.toList tbl_fields) $ \field -> do
      constraintA <- fieldConstraint a field
      constraintB <- fieldConstraint b field
      pure (AppT (AppT (AppT (ConT ''Default) (VarT p)) constraintA) constraintB)

    fieldConstraint :: Name -> FieldInfo -> Q Type
    fieldConstraint var fld = pure (fieldInfoToType fld {fld_tyvar = VarT var})

    defMethod :: Dec
    defMethod = FunD 'def [Clause [] (NormalB body) []]
      where body = VarE tbl_adname `AppE` ctor
            ctor = foldr (flip AppE) (ConE tbl_dctor)
                     (replicate (NonEmpty.length tbl_fields)
                                (VarE 'def))

--------------------------------------------------------------------------------
-- | Generate the function that is used with @selectTable@ function.
makeTableDefinition :: TableInfo -> Q [Dec]
makeTableDefinition TableInfo{..} = do
      body <- mkTable

      pure [ SigD tbl_fnname sigType
           , FunD tbl_fnname [Clause [] (NormalB body) []]
           ]
  where
    mkTable :: Q Exp
    mkTable = do
      ctor <- foldM (\b a -> AppE b <$> apfield a) (ConE tbl_dctor) tbl_fields
      let adapter = VarE tbl_adname `AppE` ctor
      pure (VarE 'table `AppE` LitE (StringL tbl_sqlname) `AppE` adapter)

    sigType :: Type
    sigType = ConT ''Table `AppT`
                (ConT tbl_tctor `AppT` ConT ''SqlWrite) `AppT`
                (ConT tbl_tctor `AppT` ConT ''SqlRead)

    apfield :: FieldInfo -> Q Exp
    apfield FieldInfo{..} = do
      lit <- colLit fld_colname
      pure (VarE 'tableField `AppE` lit)

    colLit :: Type -> Q Exp
    colLit (LitT (StrTyLit s)) = pure (LitE (StringL s))
    colLit _ = fail "expected Col to contain a symbol"

--------------------------------------------------------------------------------
-- | Generate the @Map@ type family instance.
makeMapInstance :: TableInfo -> Q [Dec]
makeMapInstance TableInfo{..} = do
  f <- newName "f"
  g <- newName "g"

  let lvals = [ VarT g
              , ConT tbl_tctor `AppT` (ConT ''F `AppT` VarT f)
              ]

      rval = ConT tbl_tctor `AppT`
               (ConT ''F `AppT` (ConT ''IMap `AppT`
                                   VarT g `AppT`
                                   VarT f))

  pure [ TySynInstD ''Map (TySynEqn lvals rval)
       ]
