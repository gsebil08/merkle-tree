module Model.Types where

import Relude

data MerkleTree a
  = Leaf
  | Node a (MerkleTree a) (Maybe (MerkleTree a))
  deriving (Eq, Show)

instance Functor MerkleTree where
  fmap _ Leaf = Leaf
  fmap f (Node x l (Just r)) = Node (f x) (fmap f l) (Just (fmap f r))
  fmap f (Node x l Nothing) = Node (f x) (fmap f l) Nothing
