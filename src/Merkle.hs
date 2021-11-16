module Merkle where

import Crypto.Hash (Digest, SHA256 (..))
import Data.List (head)
import Model.Types (MerkleTree (..))
import Model.Utils (asNode, listToTuples)
import Relude hiding (concat, head, reverse)

-- | Build a 'MerkleTree' from the given list
merkleTree :: [Digest SHA256] -> MerkleTree (Digest SHA256)
merkleTree = buildTree . getLeavesFromDigests

getLeavesFromDigests :: [Digest SHA256] -> [MerkleTree (Digest SHA256)]
getLeavesFromDigests txs = (\x -> Node x Leaf (Just Leaf)) <$> txs

-- | Inner function to build the tree
buildTree :: [MerkleTree (Digest SHA256)] -> MerkleTree (Digest SHA256)
buildTree [] = Leaf
buildTree trees =
  let nodes :: [MerkleTree (Digest SHA256)] = map (uncurry asNode) $ listToTuples trees
   in case length nodes of
        1 -> head nodes
        _ -> buildTree nodes
