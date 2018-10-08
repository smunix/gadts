module Data.Heterogeneous.List ( List(..)
                               ) where

data List (xs :: [*]) where
  Nil :: List '[]
  Cons :: x -> List xs -> List (x ': xs)
infixr 5 `Cons`

instance Show (List '[]) where
  show _ = "Nil"
deriving instance (Show x, Show (List xs)) => Show (List (x ': xs))
