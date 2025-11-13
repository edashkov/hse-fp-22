import Data.Monoid
import Data.Foldable

z0 = [1,2,3] ++ []
z1 = 123 + 0
z2 = 123 * 1
z3 = (^2) . id

--class Monoid a where
--    mempty :: a

-- the semigroup operation; in fact, one has Semigroup a => Monoid a
-- in recent versions.
--    (<>) :: a -> a -> a

-- should be the same as (<>); deprecated
--    mappend :: a -> a -> a
--    mappend = (<>)

-- {-# MINIMAL mempty, (<>) #-}

--    mconcat :: [a] -> a
--    mconcat = foldr mappend mempty

-- The following laws are expected but not enforced:
-- x <> mempty = x
-- mempty <> x = x
-- x <> (y <> z) = (x <> y) <> z 


--instance Monoid [a] where
--    mempty = []
--    (<>) = (++)

z4 = [1,2,3] <> [4,5]

--instance Monoid a => Monoid (Maybe a) where
--    mempty = Nothing
--    Nothing <> mx = mx
--    mx <> Nothing = mx
--    Just x <> Just y = Just (x <> y)

z5 = Just [1,2,3] <> Nothing
z6 = Just [1,2,3] <> Just [4,5]


--instance Monoid Ordering where
--    mempty = EQ
--    LT <> _ = LT
--    EQ <> y = y
--    GT <> _ = GT

--data Name = Name String String
data Name = Name { getFstN :: String, getSurN :: String }
    deriving Show

z7 = Name { getFstN = "Haskell", getSurN = "Curry" }
z8 = Name { getSurN = "Church", getFstN = "Alonzo" }
z9 = Name "Nicolaas" "Bruijn, de"
z10 = Name { getFstN = "Dick", getSurN = "Bruijn, de" }

s1 = getFstN z8

-- So, we effectively have multiplied the orders of
-- 'first names' and 'surnames'
cmpName :: Name -> Name -> Ordering
cmpName n1 n2 = compare (getSurN n1) (getSurN n2) <>
                compare (getFstN n1) (getFstN n2) 

{-- Sometimes, there is more than one natural way
 to define monoid over a type; we can label the
respective monoids via newtype.
--}

--newtype Product a =  Product { getProduct :: a }

--instance Num a => Monoid (Product a) where
--    mempty = Product 1
--    Product x <> Product y = Product (x * y)

n1 = getProduct $ Product 2 <> Product 3

--newtype Sum a = Sum { getSum :: a }
n2 = getSum $ Sum 2 <> Sum 3                


--newtype Any = Any { getAny :: Bool }
--instance Monoid Any where
--        mempty = Any False
--        Any x <> Any y = Any (x || y)

--newtype All = All { getAll :: Bool }        
--instance Monoid All where
--        mempty = All True
--        All x <> All y = All (x && y)

and' :: [Bool] -> Bool
and' = getAll . mconcat . map All

-- instance Monoid a => Monoid (IO a)
-- mempty = return mempty  
-- a1 <> a2 = do x <- a1
--               y <- a2
--               return  $ x <> y

--

--class Foldable t where
--    fold :: Monoid a => t a -> a
--    fold = foldMap id

--    foldMap :: Monoid b => (a -> b) -> t a -> b
--    foldMap f = foldr (<> . f) mempty

--    foldr :: (a -> b -> b) -> b -> t a -> b
--    foldr f z = foldr f z . toList
--                  where toList = foldMap (\x -> [x])

--    foldl :: (b -> a -> b) -> b -> t a -> b
--    foldl' :: (b -> a -> b) -> b -> t a -> b


-- {-# MINIMAL foldMap | foldr  #-}

-- null, length, elem, maximum, minimum, sum, product

--instance Foldable [] where
--    foldMap g [] = mempty
--    foldMap g (x:xs) = g x <> foldMap g xs


--instance Foldable Maybe where
--fold :: Monoid a => Maybe a -> a
--fold Nothing = mempty
--fold (Just x) = x

--foldMap :: Monoid b => (a -> b) -> Maybe a -> b
--foldMap _ Nothing = mempty
--foldMap f (Just x) = f x

--foldr :: (a -> b -> b) -> b -> Maybe a -> b
--foldr _ u Nothing = u
--foldr f u (Just x) = f x u

--foldl :: (b -> a -> b) -> b -> Maybe a -> b
--foldl _ u Nothing = u
--foldl f u (Just x) = f u x

n5 = foldr (^) 2 (Just 3)
n6 = foldl (^) 2 (Just 3)
n7 = foldr (^) 2 Nothing

--

sum' :: [Int] -> Int
sum' = getSum . fold . map Sum

n3 = sum' [1,2,3]

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

n4 = length' [1,2,3]

-- import Data.Foldable
-- toList

toList' :: Foldable t => t a -> [a]
toList' = foldMap (:[])

z11 = toList [1,2,3]
z12 = toList' (Just 1)

foldr_a :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr_a g u = foldr g u . toList

n8 = foldr_a (^) 2 (Just 3)

length_a :: Foldable t => t a -> Int
length_a = length . toList

n9 = length_a [1,2,3]

--

data Tree a = Nil | Node (Tree a) a (Tree a)
        deriving Show

instance Foldable Tree where
--    fold :: Monoid a => Tree a -> a
    fold Nil = mempty
    fold (Node l x r) = (fold l) <> x <> (fold r)

--    foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Nil = mempty
    foldMap f (Node l x r) = (foldMap f l) <> f x <> (foldMap f r)

--    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ u Nil = u
    foldr f u (Node l x r) = foldr f (f x (foldr f u r)) l

--    foldl :: (b -> a -> b) -> b -> Tree a -> b
    foldl _ u Nil = u
    foldl f u (Node l x r) = foldl f (f (foldl f u l) x) r

myTree = Node (Node Nil 2 (Node Nil 4 Nil)) 3 (Node (Node Nil 5 Nil) 4 (Node Nil 6 Nil))

n14 = foldr (+) 0 myTree

n15 = foldr (*) 1 myTree
n16 = foldl (*) 1 myTree
n16' = foldl' (*) 1 myTree

n17 = foldr (-) 0 myTree
n18 = foldl (-) 0 myTree

--

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p tx = foldMap (\x -> if p x then [x] else []) tx

l6 = filterF even [1,2,4,3]
l7 = filterF even myTree
l8 = filterF even Nothing
l9 = filterF even (Just 1)
l10 = filterF even (Just 2)

--

{-- What is the difference between foldl and foldl' exactly?  --}
