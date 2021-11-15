module Main where

import MerkleSpec (merkleSpec)
import Model.UtilsSpec (listSpec)
import Relude
import Test.Hspec

main :: IO ()
main = hspec $ do
  listSpec
  merkleSpec
