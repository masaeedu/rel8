{-# language LambdaCase #-}

module Rel8.Query.Opaleye
  ( fromOpaleye
  , toOpaleye
  , mapOpaleye
  , zipOpaleyeWith
  , withSymbols
  )
where

-- base
import Control.Applicative ( liftA2 )
import Data.Bifunctor ( first )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye

-- rel8
import {-# SOURCE #-} Rel8.Query ( Query( Query ) )


fromOpaleye :: Opaleye.Select a -> Query a
fromOpaleye = Query . const


toOpaleye :: Query a -> Opaleye.Select a
toOpaleye (Query a) = a []


mapOpaleye :: (Opaleye.Select a -> Opaleye.Select b) -> Query a -> Query b
mapOpaleye f (Query a) = Query (fmap f a)


zipOpaleyeWith :: ()
  => (Opaleye.Select a -> Opaleye.Select b -> Opaleye.Select c)
  -> Query a -> Query b -> Query c
zipOpaleyeWith f (Query a) (Query b) = Query $ liftA2 f a b


withSymbols :: Opaleye.Select a -> Opaleye.Select ([Opaleye.Symbol], a)
withSymbols = fmap (first extractSymbols) . withPrimQuery


withPrimQuery :: Opaleye.Select a -> Opaleye.Select (Opaleye.PrimQuery, a)
withPrimQuery (Opaleye.QueryArr q) = Opaleye.QueryArr $ \(_, query, tag) ->
  case q ((), query, tag) of
    (a, query', tag') -> ((query', a), query', tag')


extractSymbols :: Opaleye.PrimQuery -> [Opaleye.Symbol]
extractSymbols = \case
  Opaleye.Unit -> []
  Opaleye.Empty _ -> []
  Opaleye.BaseTable _ bindings -> map fst bindings
  Opaleye.Product as _ -> foldMap (foldMap extractSymbols) as
  Opaleye.Aggregate bindings _ -> map fst bindings
  Opaleye.DistinctOnOrderBy _ _ query -> extractSymbols query
  Opaleye.Limit _ query -> extractSymbols query
  Opaleye.Join _ _ bindings bindings' query query' ->
    map fst bindings <>
    map fst bindings' <>
    extractSymbols query <>
    extractSymbols query'
  Opaleye.Exists _ query _ -> extractSymbols query
  Opaleye.Values symbols _ -> symbols
  Opaleye.Binary _ (query, query') ->
    extractSymbols query <>
    extractSymbols query'
  Opaleye.Label _ query -> extractSymbols query
  Opaleye.RelExpr _ bindings -> map fst bindings
  Opaleye.Rebind False bindings _ -> map fst bindings
  Opaleye.Rebind True bindings query ->
    map fst bindings <>
    extractSymbols query
  Opaleye.ForUpdate query -> extractSymbols query
