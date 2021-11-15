module Model.UtilsSpec where

import qualified Model.Utils as SUT (listToTuples)
import Relude
import Test.Hspec

listSpec :: Spec
listSpec = do
  describe "### Model.Utils.listToTuples" $ do
    it "organizes by two on an even length list" $ do
      let expected = [(1, 2), (3, 4), (5, 6)]
      SUT.listToTuples ([1, 2, 3, 4, 5, 6] :: [Int]) `shouldBe` expected

    it "organizes by two on an odd length list" $ do
      let expected = [(1, 2), (3, 4), (5, 5)]
      SUT.listToTuples ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` expected

    it "organizes by two on an empty list" $ do
      let expected = []
      SUT.listToTuples ([] :: [Int]) `shouldBe` expected
