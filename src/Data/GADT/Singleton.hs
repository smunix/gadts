module Data.GADT.Singleton ( Vector(..)
                           , Nat(..)
                           , Data.GADT.Singleton.replicate
                           , SNat(..)
                           , SNatI(..)
                           , toList
                           , Data.GADT.Singleton.length
                           ) where

import           Data.GADT.Vector (Nat (..), Vector (..))
import Data.Serialize

data SNat (n :: Nat) where
  SZero :: SNat Zero
  SSucc :: (SNatI n) => Int -> SNat (Succ n)
deriving instance Show (SNat n)

class SNatI (n :: Nat) where
  sNat :: SNat n

instance SNatI Zero where
  sNat = SZero

instance (SNatI n) => SNatI (Succ n) where
  sNat = case (sNat :: SNat n) of
    SZero -> SSucc 1
    (SSucc !n) -> SSucc (n+1)

instance (Serialize a) => Serialize (Vector a Zero) where
  put _ = return ()
  get = return VNil

instance (Serialize a, Serialize (Vector a n), SNatI n) => Serialize (Vector a (Succ n)) where
  put (VCons x xs) = put x >> put xs
  get = case (sNat :: SNat n) of
    SZero -> VCons <$> get <*> (return VNil)
    SSucc _ -> VCons <$> get <*> get

replicate :: forall a n . (SNatI n) => a -> Vector a n
replicate a = case sNat :: SNat n of
  SZero   -> VNil
  SSucc _ -> a `VCons` Data.GADT.Singleton.replicate a

toList :: Vector a n -> [a]
toList VNil = []
toList (VCons x xs) = x : toList xs

#if 0
toN :: forall n . (SNatI n) => Int -> SNat n
toN 0 = case (sNat :: SNat n) of
  SZero -> SZero
  _ -> error "fail"
toN i = case (sNat :: SNat n) of
  v@(SSucc i') -> if (i == i') then v else error "fail"
#endif

length :: forall a n . (SNatI n) => Vector a n -> Int
length _ = toI sNat
  where
    toI :: SNat n -> Int
    toI SZero = 0
    toI (SSucc i) = i

