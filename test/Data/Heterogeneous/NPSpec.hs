module Data.Heterogeneous.NPSpec ( spec
                                 ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Heterogeneous.NP
import           Data.Heterogeneous.NP as N

spec :: Spec
spec = do
  describe "Environment / Heterogenous n-ary products" $ do
    let
      is :: NP I [Char, String, Int]
      is = I 'x' :-> I "Providence" :-> I (3 :: Int) :-> O

      ms :: NP Maybe [Char, Double, String, Int]
      ms = Just 'x' :-> Nothing :-> Just "Providence" :-> Just (3 :: Int) :-> O

      ks :: NP (K Double) [Char, Double, String, Int]
      ks = K 4.0 :-> K 3.0 :-> K 2.0 :-> K 1.0 :-> O

      ls :: NP [] [String, Int]
      ls = ["foo", "bar", "baz"] :-> [1..10] :-> O

      ns :: NP (K Int) [String, Int]
      ns = (K 2) :-> (K 1) :-> O

      take' :: (K Int -.-> [] -.-> []) a
      take' = liftFn2 (\(K n) xs -> take n xs)

    it "show" $ do
      show (ms) `shouldBe` "Just 'x' :-> (Nothing :-> (Just \"Providence\" :-> (Just 3 :-> O)))"
      show (is) `shouldBe` "I 'x' :-> (I \"Providence\" :-> (I 3 :-> O))"

    it "collapse" $ do
      collapse ks `shouldBe` [4.0, 3.0..1.0]

    it "applicative" $ do
      N.pure take' N.<*> ns N.<*> ls `shouldBe` (["foo", "bar"] :-> [1] :-> O)

    it "map" $ do
      (m N.<$> is) `shouldBe` (Just 'x' :-> Just "Providence" :-> Just 3 :-> O)
        where
          m :: forall a . I a -> Maybe a
          m = Just . unI
