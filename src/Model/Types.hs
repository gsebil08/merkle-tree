module Model.Types where

import Relude

data MerkleTree a
  = Leaf a
  | Node a (MerkleTree a) (Maybe (MerkleTree a))
  deriving (Eq, Show)

instance Functor MerkleTree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node x l (Just r)) = Node (f x) (fmap f l) (Just (fmap f r))
  fmap f (Node x l Nothing) = Node (f x) (fmap f l) Nothing
