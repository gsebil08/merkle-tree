module Main where

import MerkleSpec (merkleSpec)
import Model.UtilsSpec (utilsSpec)
import Relude
import Test.Hspec

main :: IO ()
main = hspec $ do
  utilsSpec
  merkleSpec
