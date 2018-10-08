module Data.Heterogeneous.ListSpec ( spec
                                   ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Heterogeneous.List

spec :: Spec
spec = do
  describe "List.tests" $ do
    it "test.1" $ do
      show ('x' `Cons` False `Cons` 3 `Cons` Nil) `shouldBe` "Cons 'x' (Cons False (Cons 3 Nil))"
