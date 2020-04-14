{-# language DataKinds #-}
{-# language DeriveFunctor #-}
{-# language DerivingVia #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}

module Data.Functor.FieldName where

import Data.Coerce ( coerce )
import Data.Distributive ( Distributive(..) )
import Data.Functor.Rep ( Representable(..), pureRep, apRep )
import Data.Kind ( Type )
import Data.Tagged.PolyKinded ( Tagged(..) )
import GHC.TypeLits ( Symbol )


-- | Tag a value with the name of the field it belongs to.
--
-- This is very similar to using 'Data.Tagged.Tagged', but has an alternative
-- representation (@FieldName name () -> a@) that carries slightly more
-- information.
newtype FieldName (name :: Symbol) (a :: Type) =
  FieldName { unFieldName :: a }
  deriving (Functor, Foldable) via Tagged name
  deriving (Semigroup) via Tagged name a


instance Applicative ( FieldName name ) where
  pure =
    pureRep

  (<*>) =
    apRep


instance Traversable (FieldName name) where
  traverse f (FieldName x) =
    FieldName <$> f x


instance Representable (FieldName name) where
  type Rep (FieldName name) =
    FieldName name ()

  index (FieldName name) _ =
    name

  tabulate f =
    FieldName (f (FieldName ()))


instance Distributive (FieldName name) where
  distribute y =
    FieldName $ fmap coerce y
