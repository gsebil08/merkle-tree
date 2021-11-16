module Model.UtilsSpec where

import Commons
import Data.Maybe (fromJust)
import qualified Model.Utils as SUT (combine, listToTuples, merkleRoot)
import Relude
import Test.Hspec

utilsSpec :: Spec
utilsSpec = do
  describe "TESTING: Model.Utils.listToTuples" $ do
    it "organizes by two on an even length list" $ do
      let expected = [(1, Just 2), (3, Just 4), (5, Just 6)]
      SUT.listToTuples ([1, 2, 3, 4, 5, 6] :: [Int]) `shouldBe` expected

    it "organizes by two on an odd length list" $ do
      let expected = [(1, Just 2), (3, Just 4), (5, Nothing)]
      SUT.listToTuples ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` expected

    it "organizes by two on an empty list" $ do
      let expected = []
      SUT.listToTuples ([] :: [Int]) `shouldBe` expected

    it "organizes by two on a list with one element" $ do
      let expected = [(1, Nothing)]
      SUT.listToTuples ([1] :: [Int]) `shouldBe` expected

  describe "TESTING: Merkle.combine" $
    it "combines two Digest SHA256" $ do
      let [digest1, digest2, mkRoot] =
            fromJust . parseSHA256
              <$> [ anyValidData,
                    anyValidDataBis, --anyValidDataBis
                    "4f7333e0a41d00da0abac20e4a4c3b5485ade6b74c7a766b1a6d334c23cc870c"
                  ]
      SUT.combine digest1 (Just digest2) `shouldBe` mkRoot

  describe "TESTING: Merkle.merkleRoot" $ do
    it "computes merkle tree root from an even list of digests" $ do
      let mkRoot = "1dfe43abc2b36ec77b296ec9ad353e85578fd09ebc6698778c342286c1302234"
          digests = [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater]
          rootDigest = fromJust $ traverse parseSHA256 digests
          expected = fromJust $ parseSHA256 mkRoot
      SUT.merkleRoot rootDigest `shouldBe` expected

    it "computes merkle tree root from an odd list of digests" $ do
      let mkRoot = "2d8ef06fece12f71ec958c91c07e26de51729c5582cd11bac264766ca5812871"
          digests = [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater, anyValidDataQuinquies]
          rootDigest = fromJust $ traverse parseSHA256 digests
          expected = fromJust $ parseSHA256 mkRoot
      SUT.merkleRoot rootDigest `shouldBe` expected
