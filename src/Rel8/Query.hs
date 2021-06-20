{-# language StandaloneKindSignatures #-}

module Rel8.Query
  ( Query( Query )
  )
where

-- base
import Control.Applicative ( liftA2 )
import Control.Monad ( liftM2 )
import Data.Kind ( Type )
import Prelude

-- opaleye
import qualified Opaleye.Internal.HaskellDB.PrimQuery as Opaleye
import qualified Opaleye.Internal.QueryArr as Opaleye

-- rel8
import Rel8.Query.Set ( unionAll )
import Rel8.Query.Opaleye ( fromOpaleye, withSymbols )
import Rel8.Query.Values ( values )
import Rel8.Table.Alternative
  ( AltTable, (<|>:)
  , AlternativeTable, emptyTable
  )

-- semigroupoids
import Data.Functor.Apply ( Apply, (<.>) )
import Data.Functor.Bind ( Bind, (>>-) )


-- | The @Query@ monad allows you to compose a @SELECT@ query. This monad has
-- semantics similar to the list (@[]@) monad.
type Query :: Type -> Type
newtype Query a = Query ([Opaleye.PrimExpr] -> Opaleye.Select a)


instance Functor Query where
  fmap f (Query a) = Query (fmap (fmap f) a)


instance Apply Query where
  (<.>) = (<*>)


instance Applicative Query where
  pure = fromOpaleye . pure
  liftA2 = liftM2


instance Bind Query where
  (>>-) = (>>=)


instance Monad Query where
  Query q >>= f = Query $ \exprs -> do
    (symbols, a) <- withSymbols (q exprs)
    let
      exprs' =
        case symbols of
          [] -> exprs
          symbol : _ -> Opaleye.AttrExpr symbol : exprs
    case f a of
      Query q' -> q' exprs'


-- | '<|>:' = 'unionAll'.
instance AltTable Query where
  (<|>:) = unionAll


-- | 'emptyTable' = 'values' @[]@.
instance AlternativeTable Query where
  emptyTable = values []
