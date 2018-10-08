module Data.Heterogeneous.All ( All(..)
                              , type (|=>) (..)
                              , type (^=>) (..)
                              ) where

import GHC.Exts (Constraint)

-- | C |=> [a, b, c] tells that all types a, b, c satisfy the constraint C
-- C |=> [a, b, c] <=> (C a, C b, C c)
type family (c :: k -> Constraint) |=> (xs :: [k]) :: Constraint where
  c |=> '[] = ()
  c |=> (x ': xs) = (c x, c |=> xs)

-- | c ^=> f adds a Constraint c to a functor/typeclass f. This is composition of a functor followed by a Constraint
class (c (f x)) => ((c :: * -> Constraint) ^=> (f :: k -> *)) (x :: k)
instance (c (f x)) => (c ^=> f) x

class ((c ^=> f) |=> xs) => All c f xs
instance ((c ^=> f) |=> xs) => All c f xs

