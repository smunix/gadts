module Data.Heterogeneous.NP ( NP(..)
                             , I(..)
                             , K(..)
                             , collapse
                             , unI
                             , unK
                             , pure
                             , (<*>)
                             , (<$>)
                             , liftFn2
                             , liftFn3
                             , type (-.->) (..)
                             ) where

import Prelude hiding (map, pure, ap, (<$>), (<*>))
import Data.Heterogeneous.All

data NP (f :: k -> *) (xs :: [k]) where
  O :: NP f '[]
  (:->) :: f x -> NP f xs -> NP f (x ': xs)
infixr 5 :->

#if 0
instance Show (NP f '[]) where
  show _ = "O"
deriving instance (Show (f x), Show (NP f xs)) => Show (NP f (x ': xs))

instance Eq (NP f '[]) where
  (==) _ _ = True
deriving instance (Eq (f x), Eq (NP f xs)) => Eq (NP f (x ': xs))

#else
-- deriving instance ((Show ^=> f) |=> xs) => Show (NP f xs)
-- deriving instance ((Eq ^=> f) |=> xs) => Eq (NP f xs)
deriving instance (All Show f xs) => Show (NP f xs)
deriving instance (All Eq f xs) => Eq (NP f xs)
#endif

data I a where
  I :: a -> I a
  deriving (Show)

unI :: I a -> a
unI (I a) = a

data K a b where
  K :: a -> K a b
  deriving (Show)

unK :: K a b -> a
unK (K a) = a

collapse :: NP (K a) xs -> [a]
collapse O = []
collapse (K a :-> xs) = a : collapse xs

data SNP (f :: k -> *) (xs :: [k]) where
  SO :: SNP f '[]
  SCons :: (HasSNP f xs) => SNP f (x ': xs)

class HasSNP (f :: k -> *) (xs :: [k]) where
  getSNP :: SNP f xs

instance HasSNP f '[] where
  getSNP = SO

instance (HasSNP f xs) => HasSNP f (x ': xs) where
  getSNP = SCons

pure :: forall f xs . (HasSNP f xs) => (forall x. f x) -> NP f xs
pure fx = case getSNP :: SNP f xs of
  SO -> O
  SCons -> fx :-> (pure fx)

-- | Type-level transformer from Functor f to functor g
--   Lifted functions
data ((f :: k -> *) -.-> (g :: k -> *)) a where
  Fn :: (f a -> g a) -> (f -.-> g) a
infixr 1 -.->

unFn :: (f -.-> g) a -> (f a -> g a)
unFn (Fn f) = f

-- | Lift functions
(<*>) :: NP (f -.-> g) xs -> NP f xs -> NP g xs
(<*>) O O = O
(<*>) (f :-> fs) (x :-> xs) = unFn f x :-> (fs <*> xs)

#if 0
(<$>) :: (forall x. f x -> g x) -> NP f xs -> NP g xs
(<$>) _ O = O
(<$>) m (x :-> xs) = m x :-> m <$> xs
#else
(<$>) :: forall f g xs . (HasSNP (f -.-> g) xs) => (forall a. f a -> g a) -> NP f xs -> NP g xs
f <$> xs = pure (Fn f) <*> xs
#endif

liftFn2 :: (f a -> f' a -> f'' a) -> (f -.-> f' -.-> f'') a
liftFn2 f = Fn (\a -> Fn (\a' -> f a a'))

liftFn3 :: (f a -> f' a -> f'' a -> f''' a) -> (f -.-> f' -.-> f'' -.-> f''') a
liftFn3 f = Fn (\a -> Fn (\a' -> Fn (\a'' -> f a a' a'')))

