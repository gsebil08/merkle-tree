module Model.Utils where

import Crypto.Hash (Digest, HashAlgorithm, SHA256, digestFromByteString, hash)
import Data.ByteArray (concat)
import Data.List (head)
import Data.Maybe (fromJust)
import Model.Types (MerkleTree (..))
import Relude hiding (concat, head)

-- | Create list of tuples from a given list
-- When the list contains only one element, it will be on the two side of the tuple
listToTuples :: [a] -> [(a, a)]
listToTuples [] = []
listToTuples [x] = [(x, x)]
listToTuples (x : y : zs) = (x, y) : listToTuples zs

-- | Combine two Digests
combine :: forall a. HashAlgorithm a => Digest a -> Digest a -> Digest a
combine firstHash secondHash = fromJust . digestFromByteString . doubleHash $ merge firstHash secondHash
  where
    merge :: Digest a -> Digest a -> ByteString
    merge x y = concat [y, x] :: ByteString
    doubleHash :: ByteString -> Digest a
    doubleHash a = hash (hash a :: Digest a) :: Digest a

-- | Compute the Merkle Tree Root from the given list
-- While the length of the list is not equal to 1, it means this is not the root of the tree. Hence, continue.
-- TODO: What should I do with an empty list?
merkleRoot :: [Digest SHA256] -> Digest SHA256
merkleRoot digests =
  if length reduced == 1
    then head reduced
    else merkleRoot reduced
  where
    reduced :: [Digest SHA256]
    reduced = map (uncurry combine) $ listToTuples digests

-- | Helper function to get the nodes of the leaf in order to build the tree
-- The unique business case, is when we have elements in both 'side' of the tuple, it means it is a node
-- Let's return a Leaf in any other case
asNode :: MerkleTree (Digest SHA256) -> MerkleTree (Digest SHA256) -> MerkleTree (Digest SHA256)
asNode left@(Node x _ _) right@(Node y _ _) = Node (combine x y) left right
asNode _ _ = Leaf
