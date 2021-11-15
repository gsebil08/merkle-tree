module MerkleSpec where

import Crypto.Hash (SHA256 (..), hashWith)
import Data.Maybe (fromJust)
import Merkle
import qualified Merkle as SUT
import Model.Types (MerkleTree (..))
import Relude
import Test.Hspec

anyValidData :: Text
anyValidData = "c67579305b91024bf849d4769c3110fac3f2d18678f20364e8f8b961cfcc8758" --Hex representation of "anyValidData"

anyValidDataBis :: Text
anyValidDataBis = "63976e06e21e475d03e227a04223293eb300b682dff506f5abbf981505bef520" --Hex representation of "anyValidDataBis"

anyValidDataTer :: Text
anyValidDataTer = "b3981997f43c7c06b8840f222e5789503982954b21fcb1b78ad4c27a246f8597" --Hex representation of "anyValidDataTer"

anyValidDataQuater :: Text
anyValidDataQuater = "9cc3cdb9ae29e2c2ae606601fccd546c4b767e7a07d5f8353f40484b20067304" --Hex representation of "anyValidDataQuater"

anyValidDataQuinquies :: Text
anyValidDataQuinquies = "d17f257be7cab8937bb0c063456156fae4a90362a6a5774faf7f0eec63028450" --Hex representation of "anyValidDataQuinquies"

merkleSpec :: Spec
merkleSpec = do
  describe "TESTING: Merkle.combine" $
    it "combines two Digest SHA256" $ do
      let [digest1, digest2, mkr] =
            fromJust . parseSHA256
              <$> [ anyValidData,
                    anyValidDataBis, --anyValidDataBis
                    "4f7333e0a41d00da0abac20e4a4c3b5485ade6b74c7a766b1a6d334c23cc870c"
                  ]
      SUT.combine digest1 digest2 `shouldBe` mkr

  describe "TESTING: Merkle.SUT.parseSHA256" $ do
    it "from Text hex representation to Digest" $ do
      let sha256 = "c67579305b91024bf849d4769c3110fac3f2d18678f20364e8f8b961cfcc8758"
          expected = Just $ hashWith SHA256 ("anyValidData" :: ByteString)
      SUT.parseSHA256 sha256 `shouldBe` expected
      SUT.parseSHA256 "invalid1" `shouldBe` Nothing
      SUT.parseSHA256 "invalid2" `shouldBe` Nothing

  describe "TESTING: Merkle.isSHA256" $ do
    it "test isSHA256 on valid and invalid data" $ do
      let valid = "ee6bc0e5f95a4ccd0f00784eab850ff8593f9045de96c6656df41c8f9f9c0888"
          invalid =
            [ "ee6bc0e5f95a4ccd0f00784eab850ff8593f9045de96c6656df41c8f9f9c088Z",
              "ee6bc0e5f95a4ccd0f00784ea",
              "invalid"
            ]
      SUT.isSHA256 valid `shouldBe` True
      SUT.isSHA256 <$> invalid `shouldBe` [False, False, False]

  describe "TESTING: Merkle.combineSHA256" $ do
    it "combines two Text into one" $ do
      let digest1 = anyValidData
          digest2 = anyValidDataBis
          expected = "4f7333e0a41d00da0abac20e4a4c3b5485ade6b74c7a766b1a6d334c23cc870c"
      SUT.combineSHA256 digest1 digest2 `shouldBe` Just expected

  describe "TESTING: Merkle.merkleRoot" $ do
    it "computes merkle tree root from an even list of SHA256 hex text" $ do
      let mkr = "1dfe43abc2b36ec77b296ec9ad353e85578fd09ebc6698778c342286c1302234"
          digests = [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater]
          rootDigest = fromJust $ traverse SUT.parseSHA256 digests
          expected = fromJust $ SUT.parseSHA256 mkr
      SUT.merkleRoot rootDigest `shouldBe` expected

    it "computes merkle tree root from an odd list of SHA256 hex text" $ do
      let mkRoot = "ccff14663f259b72941763eb7718f6f501076bf2128f424f9cd341db8a8ef770"
          digests = [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater, anyValidDataQuinquies]
          rootDigest = fromJust $ traverse SUT.parseSHA256 digests
          expected = fromJust $ SUT.parseSHA256 mkRoot
      SUT.merkleRoot rootDigest `shouldBe` expected

  describe "TESTING: Merkle.merkleTree" $ do
    it "Merkle tree from an even list of digests" $ do
      let toHash = fromJust . SUT.parseSHA256
          digests = toHash <$> [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater]
          expected =
            toHash
              <$> Node
                "1dfe43abc2b36ec77b296ec9ad353e85578fd09ebc6698778c342286c1302234"
                ( Node
                    "4f7333e0a41d00da0abac20e4a4c3b5485ade6b74c7a766b1a6d334c23cc870c"
                    (Node "c67579305b91024bf849d4769c3110fac3f2d18678f20364e8f8b961cfcc8758" Leaf Leaf)
                    (Node "63976e06e21e475d03e227a04223293eb300b682dff506f5abbf981505bef520" Leaf Leaf)
                )
                ( Node
                    "d5925fe7c4c2feb42074b0bca82cef0c07b1e5971f7453562dc1775fd36f23d0"
                    (Node "b3981997f43c7c06b8840f222e5789503982954b21fcb1b78ad4c27a246f8597" Leaf Leaf)
                    (Node "9cc3cdb9ae29e2c2ae606601fccd546c4b767e7a07d5f8353f40484b20067304" Leaf Leaf)
                )
      SUT.merkleTree digests `shouldBe` expected

    it "Merkle tree from an odd list of digests" $ do
      let toHash = fromJust . SUT.parseSHA256
          digests = toHash <$> [anyValidData, anyValidDataBis, anyValidDataTer, anyValidDataQuater, anyValidDataQuinquies]
          expected =
            toHash
              <$> Node
                "ccff14663f259b72941763eb7718f6f501076bf2128f424f9cd341db8a8ef770"
                ( Node
                    "1dfe43abc2b36ec77b296ec9ad353e85578fd09ebc6698778c342286c1302234"
                    ( Node
                        "4f7333e0a41d00da0abac20e4a4c3b5485ade6b74c7a766b1a6d334c23cc870c"
                        (Node "c67579305b91024bf849d4769c3110fac3f2d18678f20364e8f8b961cfcc8758" Leaf Leaf)
                        (Node "63976e06e21e475d03e227a04223293eb300b682dff506f5abbf981505bef520" Leaf Leaf)
                    )
                    ( Node
                        "d5925fe7c4c2feb42074b0bca82cef0c07b1e5971f7453562dc1775fd36f23d0"
                        (Node "b3981997f43c7c06b8840f222e5789503982954b21fcb1b78ad4c27a246f8597" Leaf Leaf)
                        (Node "9cc3cdb9ae29e2c2ae606601fccd546c4b767e7a07d5f8353f40484b20067304" Leaf Leaf)
                    )
                )
                ( Node
                    "2cfa899caf9d5609cc90bc68a537521fda51d2b9b5684f686a1f5234d3a11a84"
                    ( Node
                        "0ec673363fd0eadf55702f5b95f529a18d033e52d31b58382184b5930934dd8f"
                        (Node "d17f257be7cab8937bb0c063456156fae4a90362a6a5774faf7f0eec63028450" Leaf Leaf)
                        (Node "d17f257be7cab8937bb0c063456156fae4a90362a6a5774faf7f0eec63028450" Leaf Leaf)
                    )
                    ( Node
                        "0ec673363fd0eadf55702f5b95f529a18d033e52d31b58382184b5930934dd8f"
                        (Node "d17f257be7cab8937bb0c063456156fae4a90362a6a5774faf7f0eec63028450" Leaf Leaf)
                        (Node "d17f257be7cab8937bb0c063456156fae4a90362a6a5774faf7f0eec63028450" Leaf Leaf)
                    )
                )
      SUT.merkleTree digests `shouldBe` expected
