module Data.GADT.Vec ( Vec(..)
                     ) where

import           Data.GADT.Singleton as S
import           Data.GADT.Vector    as V
import           Data.Serialize
import           GHC.Generics

#if 0
newtype Vec n a = Vec { vec :: (Serialize a, Serialize (V.Vector a n)) => V.Vector a n }
  deriving (Show, Eq, Generic)
#endif

data Vec a where
  Vec :: forall n a . (Serialize a, SNatI n, Serialize (V.Vector a n)) => V.Vector a n -> Vec a

instance (Serialize a) => Serialize (Vec a) where
  put (Vec v) = put v
  get = undefined

#if 0
instance Functor (Vec) where
  fmap :: forall a b . Serialize b => (a -> b) -> Vec a -> Vec b
  fmap f (Vec v) = Vec (V.map f v)

instance Applicative (Vec) where
  pure a = Vec (S.replicate a :: V.Vector a (Succ Zero))
  (Vec f) <*> (Vec v) = Vec (V.vapply f v)

instance (Serialize a) => Serialize (Vec n a) where
  put (Vec v) = put v
  get = Vec <$> get

#endif
