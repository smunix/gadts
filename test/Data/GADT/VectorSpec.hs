module Data.GADT.VectorSpec ( main
                            , spec
                            ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.GADT.Vector
import qualified Data.GADT.Vector as V

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "kinds" $ do
    it "promote data kinds" $ do
      value (Proxy :: Proxy Zero) `shouldBe` 0
      value (Proxy :: Proxy (Succ Zero)) `shouldBe` 1
      value (Proxy :: Proxy (Succ (Succ (Succ Zero)))) `shouldBe` 3
  describe "vectors" $ do
    it "toList : vector construction" $ do
      toList (VNil :: Vector Int Zero) `shouldBe` []
      toList (1 `VCons` VNil) `shouldBe` [1]
      toList (2 `VCons` 1 `VCons` VNil) `shouldBe` [2, 1]
    it "replicate vectors" $ do
      show (V.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` "VCons 0 (VCons 0 (VCons 0 (VCons 0 VNil)))"
      toList (V.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` Prelude.replicate 4 0
