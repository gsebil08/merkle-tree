{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Merkle where

import Crypto.Hash
  ( Digest,
    HashAlgorithm,
    SHA256 (..),
    digestFromByteString,
    hash,
  )
import Data.ByteArray (concat, convert)
import qualified Data.Char as Char (isHexDigit)
import qualified Data.HexString as Hex (fromBytes, hexString, toBytes, toText)
import Data.Maybe (fromJust)
import qualified Data.Text as Text (all, length)
import Model.Types (MerkleTree (..))
import Model.Utils (listToTuples)
import Relude hiding (reverse, head, concat)
import Data.List (head)

-- | Combine two Digests
combine :: forall a. HashAlgorithm a => Digest a -> Digest a -> Digest a
combine firstHash secondHash =
  merge firstHash secondHash
    & doubleHash
    & digestFromByteString
    & fromJust
  where
    merge x y = concat [y, x] :: ByteString
    doubleHash a = hash (hash a :: Digest a) :: Digest a

-- | Simply parser to get real Hex from Text representation
parseSHA256 :: Text -> Maybe (Digest SHA256)
parseSHA256 t
  | isSHA256 t = parse t
  | otherwise = Nothing
  where
    parse = digestFromByteString . Hex.toBytes . Hex.hexString . encodeUtf8

-- | Check if the given Text is a correct Hex value
isSHA256 :: Text -> Bool
isSHA256 t = isHex && hasLength
  where
    isHex = Text.all Char.isHexDigit t
    hasLength = Text.length t == 64

-- | Given two Hex digests of a SHA256 it 'combine's them and return the result.
-- 'Nothing' is returned when the given 'Text' are not valid SHA256 digest.
combineSHA256 :: Text -> Text -> Maybe Text
combineSHA256 h g
  | not (isSHA256 h && isSHA256 g) = Nothing
  | otherwise = toHexText <$> maybeDigest
  where
    toHexText = Hex.toText . Hex.fromBytes . convert
    maybeDigest = combine <$> parseSHA256 h <*> parseSHA256 g

-- | Compute the Merkle Tree Root from the given list
-- What should I do if the given list is empty?
merkleRoot :: [Digest SHA256] -> Digest SHA256
merkleRoot digests
  | length reduced == 1 = head reduced
  | otherwise = merkleRoot reduced
  where
    reduced = map (uncurry combine) $ listToTuples digests

-- | Build a 'MerkleTree' from the given list
merkleTree :: [Digest SHA256] -> MerkleTree (Digest SHA256)
merkleTree = buildTree . getLeavesFromDigests

getLeavesFromDigests :: [Digest SHA256] -> [MerkleTree (Digest SHA256)]
getLeavesFromDigests txs = (\x -> Node x Leaf Leaf) <$> txs

-- | Inner function to build
buildTree :: [MerkleTree (Digest SHA256)] -> MerkleTree (Digest SHA256)
buildTree [] = Leaf
buildTree trees =
  let nodes = map (uncurry asNode) $ listToTuples trees
   in case length nodes of
        1 -> head nodes
        _ -> buildTree nodes

-- | Helper function to get the nodes of the leaf in order to build the tree
-- The unique business case, is when we have elements in both 'side' of the tuple, it means it is a node
-- Let's return a Leaf in any other case
asNode :: MerkleTree (Digest SHA256) -> MerkleTree (Digest SHA256) -> MerkleTree (Digest SHA256)
asNode left@(Node x _ _) right@(Node y _ _) = Node (combine x y) left right
asNode _ _ = Leaf
