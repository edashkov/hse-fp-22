module Main (main) where
-- this header is assumed if absent but main is defined in the module

-- imports immediately follow the header

-- standard Prelude is imported by default;
-- to redefine its exports, it is recommended
-- to import it qualified 


-- the function both is now available by its qualified aliased name U.both
import qualified Misc.Utils as U (both)
-- everything else requires the full prefix Misc.Utils;
-- this prevents ambiguities and namespace pollution
import qualified Misc.Utils

-- do not import myTree2 despite its being exported by module tree
import Tree hiding (myTree2)

-- more examples where we try to import just as many as we need to
--
-- this brings ONLY Data.Char.ord to the namespace
import qualified Data.Char (ord)

-- this brings BOTH transpose AND Data.List.transpose to the namespace,
-- since 'qualified' is omitted
import Data.List (transpose) 

import qualified Data.List as L (union, intersperse, intercalate)
--
-- the rest of the code follows imports
--

-- our 'local' version of catPairs
catPairs :: [(a,a)] -> [a]
catPairs = foldr (\(x,y) l -> x : y : l)  []

x1 = catPairs $ zip [1..5] [6..10]
x2 = Misc.Utils.catPairs $ zip [1..5] [6..10]
x3 = U.both (+1) (2, 3)

-- type Tree (mentioned explicitly here) has been exported by module Tree, so this compiles
cnst :: Tree a -> Bool
cnst _ = True

-- this requires the constructor Nil to be exported as well (cf. Tree(..)) in that module header
isNil :: Tree a -> Bool
isNil Nil = True
isNil _ = False

-- both the qualified and 'plain' version are visible,
-- since Tree is imported 'unqualified'
x3' = Tree.Nil

-- every method from class instances for
-- imported types is also available GIVEN
-- it is in the scope of this module:
-- say, the name show is imported from standard
-- libs via Prelude; but the particular implementation
-- is due to type Tree we take from module Tree
x4 = myTree
x4' = show myTree
x4'' = (myTree == myTree)

x5 = myAbsTree
x5' = show myAbsTree
x5'' = fmap (+1) myAbsTree

-- AbsTree is ABSTRACT here (whence the name comes); i.e., we have no direct access to its constructors
--x5''' = Leaf True
-- nevertheless, field name left (a 'destructor') is exported and is thus available 
x5_ = left myAbsTree


-- every function being both exported and imported is available
x6 = absToList myAbsTree
--x6' = absToTree

-- fold is imported by Tree but not exported therefrom;
-- this would require Data.Foldable here or an explicit export in module Tree 
-- (like module Tree (..., module Data.Foldable, ...))
--x7 = fold myTree1

-- Why is foldMap available? The foldMap identifier itself
-- is imported from Prelude; its particular implementation 
-- is associated with type Tree we import explicitly 
x7' = foldMap id myTree1

-- notice that the type of x7' (i.e. Sum Integer) is NOT
-- visible from here; nevertheless, this equation compiles;
-- but it would not if one made the type explicit:
--x7'' = foldMap id myTree1 :: Sum Integer

x8 = myTree1
-- hidden explicitly
--x9 = myTree2

x10 = Data.Char.ord 'Y'
-- only qualified version is allowed
--x11 = ord 'Y'

x12 = L.union "qwerty" "Haskell"
-- not in scope; only the alias qualified name is available 
--x12' = Data.List.union
x13 = L.intersperse 0 [1..9]
x14 = L.intercalate " kinda " ["I", "like", "coding in", "Haskell"]

-- both versions compile as the import was 'unqualified'
x15 = transpose [[10,11],[20],[],[30,31,32]]
x16 = Data.List.transpose [[10,11],[20],[],[30,31,32]]

--

main = putStrLn "Hello, world"


