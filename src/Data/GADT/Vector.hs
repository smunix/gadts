module Data.GADT.Vector ( Nat(..)
                        , Vector(..)
                        , Replicate(..)
                        , ToInt(..)
                        , toList
                        , vapply
                        , Proxy(..)
                        , Data.GADT.Vector.map
                        , Data.GADT.Vector.tail
                        ) where
import GHC.Generics
#if 0
import Data.Serialize
#endif

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vector (a :: *) (n :: Nat) where
  VNil :: Vector a Zero
  VCons :: a -> Vector a n -> Vector a (Succ n)

infixr 5 `VCons`

deriving instance Show a => Show (Vector a n)
deriving instance Eq a => Eq (Vector a n)

data SSerialRTTI (n :: Nat) where
  SZeroRTTI :: SSerialRTTI Zero
  SSuccRTTI :: HasSSerialRTTI n => SSerialRTTI (Succ n)

class HasSSerialRTTI (n :: Nat) where
  serialRTTI :: SSerialRTTI n

instance HasSSerialRTTI Zero where
  serialRTTI = SZeroRTTI

instance (HasSSerialRTTI n) => HasSSerialRTTI (Succ n) where
  serialRTTI = SSuccRTTI

#if 0
instance (Serialize a) => Serialize (Vector a Zero) where
  put VNil = put ()
  get = return VNil

instance (Serialize a, Serialize (Vector a n), HasSSerialRTTI n) => Serialize (Vector a (Succ n)) where
  put (VCons x xs) = do
    put x
    put xs
  get = case (serialRTTI :: SSerialRTTI n) of
    SZeroRTTI -> VCons <$> get <*> (return VNil)
    SSuccRTTI -> VCons <$> get <*> get
#endif

vapply :: Vector (a -> b) n -> Vector a n -> Vector b n
vapply VNil VNil                 = VNil
vapply (VCons f fs) (VCons x xs) = f x `VCons` (vapply fs xs)

data Proxy (n :: Nat) where
  Proxy :: Proxy n

class ToInt (n :: Nat) where
  value :: Proxy n -> Int

instance ToInt Zero where
  value _ = 0

instance (ToInt n) => ToInt (Succ n) where
  value _ = 1 + value (Proxy :: Proxy n)

map :: (a -> b) -> Vector a n -> Vector b n
map f VNil = VNil
map f (VCons x xs) = f x `VCons` Data.GADT.Vector.map f xs

tail :: Vector a (Succ n) -> Vector a n
tail (VCons x xs) = xs

class Replicate (n :: Nat) where
  replicate :: a -> Vector a n

instance Replicate Zero where
  replicate _ = VNil

instance (Replicate n) => Replicate (Succ n) where
  replicate x = x `VCons` Data.GADT.Vector.replicate x

toList :: Vector a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

