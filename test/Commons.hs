module Commons where

import Crypto.Hash (Digest, SHA256, digestFromByteString)
import qualified Data.Char as Char (isHexDigit)
import qualified Data.HexString as Hex (hexString, toBytes)
import qualified Data.Text as Text (all, length)
import Relude

-- These SHA 256 Hex values are generated thanks to https://emn178.github.io/online-tools/sha256.html
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

-- The following functions are only useful for tests.

-- | Add some type-safe, this was not in my mind in the first version. It is finally really useful.
parseSHA256 :: Text -> Maybe (Digest SHA256)
parseSHA256 text =
  if isSHA256 text
    then parse text
    else Nothing
  where
    parse :: Text -> Maybe (Digest SHA256)
    parse = digestFromByteString . Hex.toBytes . Hex.hexString . encodeUtf8

-- | Check if the given Text is a correct Hex value
isSHA256 :: Text -> Bool
isSHA256 text = isHex && hasLength
  where
    isHex :: Bool
    isHex = Text.all Char.isHexDigit text
    hasLength :: Bool
    hasLength = Text.length text == 64
