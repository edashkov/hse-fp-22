-- one module per file
module Tree(Tree(..), myTree, AbsTree, myAbsTree, absToList, myTree1, myTree2, left) where
-- we export type Tree with ALL its constructors (Tree(..)) and AbsTree with none of them;
-- so, the latter is an ABSTRACT type for any module that imports module Tree 

import Data.Foldable (fold)

-- importing types with all their constructors either implicitly or explicitly
import Data.Monoid (Sum(..),Product(Product))

data Tree a = Nil | Node (Tree a) a (Tree a)
    deriving (Show,Eq)

myTree = Node (Node Nil 1 (Node Nil 3 Nil)) 7 (Node Nil 5 Nil)


data AbsTree a = Leaf a | Branch { left, right :: AbsTree a}
    deriving (Eq,Show)

myAbsTree = Branch {left = Branch (Leaf (-7)) (Leaf 3), right = Branch {left = Leaf 1, right = Leaf 11}}


instance Functor AbsTree where
    -- fmap :: (a -> b) -> (AbsTree a -> AbsTree b)
       fmap f (Leaf x) = Leaf $ f x
       fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Foldable Tree where
    foldMap f Nil = mempty
    foldMap f (Node l x r) = foldMap f l <> f x  <> foldMap f r

-- internal; not to be exported
absToTree :: (Monoid a) => AbsTree a -> Tree a
absToTree (Leaf x) = Node Nil x Nil
absToTree (Branch l r) = Node (absToTree l) mempty (absToTree r)

absToList :: AbsTree a -> [a]
absToList = fold . absToTree . fmap (:[]) 

myTree1 = absToTree $ fmap Sum myAbsTree
myTree2 = absToTree $ fmap Product myAbsTree

