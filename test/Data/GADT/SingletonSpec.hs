module Data.GADT.SingletonSpec ( main
                               , spec
                               ) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.GADT.Singleton
import           Data.GADT.Singleton as S
import           Data.GADT.Vector    as V

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "singletons" $ do
    it "replicate vectors" $ do
      show (S.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` "VCons 0 (VCons 0 (VCons 0 (VCons 0 VNil)))"
      S.toList (S.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` Prelude.replicate 4 0
      (S.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` (V.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero)))))
    it "length vectors" $ do
      S.length (S.replicate 0 :: Vector Int (Succ (Succ (Succ (Succ Zero))))) `shouldBe` 4
