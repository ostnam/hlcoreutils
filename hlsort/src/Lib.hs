{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib where

class Ord a => Sortable a where
  sort :: [a] -> ([a], [a], [a])
-- Return 3 lists storing the smaller, equal and larger values

instance (Ord a) => Sortable a where
  sort [] = ([], [], [])
  sort (x:[]) = ([], [x], [])
  sort (x:xs) = (smaller, equal, larger) where
    smaller = filter (<  x) xs
    equal   = x : (filter (== x) xs)
    larger  = filter (> x)  xs
-- We take advantage of the fact that String is instance of Ord


data TrinaryTree a = Nil | Node [a] (TrinaryTree a) (TrinaryTree a)
  deriving Show


concatTriTree :: TrinaryTree a -> [a]
concatTriTree Nil = []
concatTriTree (Node x smaller larger) =
  (concatTriTree smaller) ++ x ++ (concatTriTree larger)


insertTrinaryTree :: [a] -> TrinaryTree a -> TrinaryTree a
insertTrinaryTree [] b = b
insertTrinaryTree a Nil = Node a Nil Nil
insertTrinaryTree a (Node b smaller larger) = Node (a ++ b) smaller larger


sortString :: (Sortable a) => TrinaryTree a
                           -> TrinaryTree a
sortString Nil = Nil
sortString (Node [] _ _) = Nil
sortString (Node x oldSmaller oldLarger) =
  Node equal
    (sortString $ insertTrinaryTree smaller oldSmaller)
    (sortString $ insertTrinaryTree larger oldLarger)
    where (smaller, equal, larger) = sort x


getAllLines :: IO [String]
getAllLines = do
  x <- getContents
  return $ lines x


printStrs :: [String] -> IO ()
printStrs [] = return ()
printStrs (x:xs) = do
  putStrLn x
  printStrs xs
