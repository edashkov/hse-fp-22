import Control.Monad
import Data.Functor.Contravariant
import Data.Bifunctor
import Control.Applicative hiding (ZipList(..))

{-- Categories and the 'category' of types
    (general definitions and discussion)
 --}

{-- Functors --}

{-- Functors in categories; a functor \P : Set -> Set --}

--class Functor (f :: * -> *) where
--  fmap :: (a -> b) -> f a -> f b
--
-- These laws are required but not enforced:
--  fmap id = id
--  fmap (g . h) = fmap g . fmap h

--instance Functor [] where
-- fmap = map

-- What is the kind of []?

data Tree a = Nil | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> (Tree a -> Tree b)
    fmap g Nil = Nil
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

myTree = Node (Node (Node Nil 33 (Node Nil 57 Nil)) 43 (Node Nil 62 Nil))
    74 (Node Nil 101 Nil) :: Tree Int

tr1 = fmap (+1) myTree

--instance Functor Maybe where
--  fmap _ Nothing = Nothing
--  fmap g (Just x) = Just (g x)

sqhead :: [Int] -> Maybe Int
sqhead xs = fmap (^2) $ if null xs then Nothing else Just (head xs)

-- notice the kind of Either a
--instance Functor (Either a) where
--  fmap :: (b -> c) -> Either a b -> Either a c
--  fmap g (Left x) = Left x
--  fmap g (Right y) = Right (g y)

f :: Int -> Either String Int
f n = if n == 0 then Left "Zero!" else Right n

g = fmap (+1) . f

--instance Functor ((->) a) where
---- NOTICE that (->) is a prefix operator,
---- so (->) x y means x -> y.
--  fmap :: (b -> c) -> (a -> b) -> (a -> c)
--  fmap  =  (.)

n1 = fmap (*3) (+100) 2
n1' = (*3) . (+100) $ 2
n2 = fmap (+1) (^3) 2

--instance Functor IO where
---- fmap :: (a -> b) -> IO a -> IO b
--fmap g act = act >>= return . g

t1 = getLine >>= return . reverse
t2 = fmap reverse getLine


x1 = fmap (replicate 3) "abc"
--x2 = fmap (replicate 3) 1
x3 = fmap (replicate 3) (Just 1)
x4 = fmap (replicate 3) Nothing
x5 = fmap (replicate 3) (Left "abc")
x6 = fmap (replicate 3) (Right 1)
x7 = fmap (replicate 3) (+1) 5
x8 = fmap (replicate 3) getChar
--

x9 = fmap (+) [1,2,3]

data Lst a = Nl | Apd a (Lst a) deriving (Show)

z = Apd 1 (Apd 2 (Apd 3 Nl))

{-- Functor laws do not hold here. --}
instance Functor Lst where
    fmap g Nl = Nl
    fmap g (Apd x Nl) = Apd (g x) Nl
    fmap g (Apd x (Apd y xs)) = Apd (g y) (Apd (g x) (fmap g xs))

z1 = fmap id z
z2 = fmap ((+1) . (+1)) z
z3 = fmap (+1) . fmap (+1) $ z

{-- Contravariant Functors --}

{--
class Contravariant (f :: * -> *) where
    contramap :: (a -> b) -> (f b -> f a)
--}

-- contramap (g . f) = contramap f . contramap g

-- newtype Predicate a = Predicate {getPredicate :: a -> Bool}

-- instance Contravariant Predicate where
--  contramap :: (a -> b) -> (Predicate b -> Predicate a)    
--  contramap f (Predicate p) = Predicate (p . f)

w1 = contramap (length :: [a] -> Int) (Predicate even)
w2 = getPredicate w1 [1..5]

{-- Bifunctors --}

{--
class Bifunctor (p :: * -> * -> *) where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
--}

-- covariant in both its arguments

-- bimap id id = id
-- bimap (g . f) (k . h) = bimap g k . bimap f h

-- instance (Bifunctor (,)) where
--  bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
--  bimap f g (x, y) = (f x, g y)

w3 = bimap even (replicate 3) (4,5)


--------------------------------------------------------------------
{-- Applicative functors --}

{--

We have fmap :: (a -> b) -> f a -> f b, but is there a natural way to
tranform it to fmap2 :: (a -> c -> b) -> f a -> f b -> f c? Say, for
fmap2 (+) :: Maybe Int -> Maybe Int -> Maybe Int.
Unfortunately, currying does not work:
fmap (+) :: Maybe Int -> Maybe (Int -> Int)
and we have no nice means to handle the value returned---we rather need
something of the type Maybe (Int -> Int) -> Maybe Int -> Maybe Int here.
Such a combinator could automatically give us fmap2, fmap3, etc.
--}

--class Functor f => Applicative f where
--pure :: a -> f a
--(<*>) :: f (a -> b) -> f a -> f b

-- The laws:
-- pure id <*> fx = fx
-- pure (g $ x) = pure g <*> pure x
---- What are the types of these terms?
-- fg <*> pure x = pure ($ x) <*> fg
-- fg <*> (fh <*> fx) = (pure (.) <*> fg  <*> fh) <*> fx

---- This suffices for the functor axioms:
-- fmap g fx = pure g <*> fx

--instance Applicative Maybe where
---- pure :: a -> Maybe a
--pure = Just
---- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--Nothing <*> _ = Nothing
--(Just g) <*> fx = fmap g fx

n3 = pure (+) <*> Just 2 <*> Just 3
n3' = (+) <$> Just 2 <*> Just 3
n4 = pure (+) <*> Just 2 <*> Nothing
n4'= pure (+) <*> Nothing <*> Just 3


--instance Applicative [] where
---- pure :: a -> [a]
--pure x = [x]
---- (<*>) :: [a -> b] -> [a] -> [b]
--gs <*> xs = [g x | g <- gs, x <- xs]

n5 = pure (+1) <*> [1,2,3]
n6 = [(+1),(+2),(+3)] <*> [1,2,3]
n7 = pure (*) <*> [1,2,3] <*> [4,5]

---- import Control.Applicative
-- liftA, liftA2,...

cartsq :: [a] -> [(a,a)]
cartsq xs = (,) <$> xs <*> xs

--

newtype ZipList a = Z { getZipList :: [a] }
    deriving Show

instance Functor ZipList where
--    fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
--    pure :: a -> ZipList a
    pure x = Z (repeat x)

--    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (<*>) (Z gs) (Z xs) = Z (map (\(u,v) -> u v) (zip gs xs))

-- Do the applicative laws hold for ZipList?

map' :: (a -> b) -> ZipList a -> ZipList b
map' f xs = pure f <*> xs

--getZipList :: ZipList a -> [a]
--getZipList (Z xs) = xs

scsum :: [Int] -> [Int] -> Int
scsum xs ys = sum . getZipList $ (*) <$> Z xs <*> Z ys


--instance Applicative ((->) a) where
---- pure :: b -> (a -> b)
---- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
--   pure x = \_ -> x
--   h <*> g = \x -> h x (g x)

x10 = pure (+) <*> (*3)
--x10 = \x -> (+) (x*3)
x11 = x10 <*> (^2)
--x11 = \y -> (+) (y*3) (y^2)
n8 = x11 $ 5


--instance Applicative IO where
---- pure :: a -> IO a
--pure = return
---- (<*>) :: IO (a -> b) -> IO a -> IO b
--mg <*> mx = do {g <- mg; x <- mx; return (g x)}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

t3 = (++) <$> getLine <*> getLine
-- What is the effect of the following?
-- (try t4 with join from Control.Monad as well)
t4 = putStrLn <$> getLine
t4' = t4 >>= id
-- So monads are necessary for IO at least...

-- What if we try emulating bind with <*>?

mybind' :: (Monad m) => m a -> (a -> m b) -> m (m b)
mybind' mx g = pure g <*> mx

mybind :: (Monad m) => m a -> (a -> m b) -> m b
mybind mx g = join ( mybind' mx g )
--mybind mx g = mybind' mx g >>= id

{-- sequenceA --}

sequenceA' :: Applicative f => [f a] -> f [a]
--sequenceA' = foldr (liftA2 (:)) (pure [])
sequenceA' [] = pure []
sequenceA' (fx : fxs) = pure (:) <*> fx <*> sequenceA' fxs

x12 = sequenceA [Just 1, Just 2, Just 3]
x12' = sequenceA' [Just 1, Just 2, Just 3]
x13 = sequenceA [Just 1, Nothing, Just 3]

x14 = sequenceA [[1,2,3]]
x15 = sequenceA [[1,2,3],[4,5]]
x15' = sequenceA [[1,2,3],[],[4,5]]

x16 = sequenceA [[3]]
x17 = sequenceA [[2,3],[3]]
x18 = sequenceA [[1,2,3],[5,6],[7,8]]

cartsq' :: [a] -> [(a,a)]
cartsq' xs = map (\[u,v] -> (u, v)) $ sequenceA [xs, xs]

x19 = sequenceA [(+1),(*2),(^3)] $ 2

x20 = sequenceA [Z [1,2,3], Z [4, 5], Z [6, 7], Z [10,11,12]]
--cf. zip3

mzip :: [[a]] -> [[a]]
mzip xss = getZipList . sequenceA $ map Z xss


t5 = sequenceA [getChar,getChar]
t5'= sequence [getChar,getChar]

{-- Monads --}

--class Applicative m => Monad m where
--    return :: a -> m a
--    (>>=) :: m a -> (a -> m b) -> m b
--    return = pure

-- return x >>= f = f x 
-- mx >>= return = mx
-- (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

-- cf. (=<<) :: (a -> m b) -> m a -> m b 
{-- Discuss closure operators, monads in categories. --}
{-- Monadic laws may be easily seen as commutative diagrams:
    (=<<) f . return = f
    (=<<) return = id
    (=<<) g . (=<<) f = (=<<) ((=<<) g . f)
--}

-- cf. (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
{-- Define Kleisli category with f (>=>) g = (=<<) g . f 
    as composition and return as id. --}

--instance Monad Maybe where
---- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--  Nothing >>= _ = Nothing
--  Just x >>= g = g x

n9 = (+) <$> Just 2 <*> Just 3
n9' = (+) <$> Just 2 <*> Nothing
n9'' = (+) <$>  Nothing <*> Just 3

n10 = div <$> Just 5 <*> Just 2
n10' = div <$> Just 5 <*> Nothing

n11 = div <$> Just 5 <*> Just 0

sdiv :: Int -> Int -> Maybe Int
sdiv n m | m == 0       = Nothing
         | otherwise    = Just $ div n m

n12 = sdiv <$> Just 5 <*> Just 0

n13 = div  5 (div 12 6)

n14 = div <$> Just 5 <*> (div <$> Just 12 <*> Just 6)

n15 = sdiv <$> Just 12 <*> Just 0
--n16' = sdiv <$> Just 5 <*> n15
n16 = (sdiv <$> Just 5 <*> (n15 >>= id)) >>= id
n16'' = join (sdiv <$> Just 5 <*> join n15)

n17 = do mx <- n15
         x <- mx
         sdiv 5 x

n18 = n15 >>= (\mx -> mx >>= \x -> sdiv 5 x)

mybind1 :: (Monad m) => m a -> (a -> m b) -> m b
mybind1 mx g = join ( pure g <*> mx )

myjoin1 :: (Monad m) => m (m a) -> m a
myjoin1 mmx = mmx >>= id

myjoin2 mmx = do mx <- mmx
                 mx

t6 = join Nothing

----

--instance Monad [] where
---- (>>=) :: [a] -> (a -> [b]) -> [b]
--    xs >>= g = [y | x <- xs, y <- g x]

z4 = [1,2,3] >>= (replicate 5)

z5 = [[1,2],[],[3,4,5],[6]] >>= id

concat' :: [[a]] -> [a]
concat' = join

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = xs >>= \x -> if p x then [x] else []

z6 = filter' even [1,2,4,5,6,7,11]

z7 = [y | y <- [1,2,4,5,6,7,11], even y]

z8 = [1,2,3] >> [4,5]
z8' = "qwerty" >> [4,5]
z8'' = [()] >> [4,5]
z8''' = [(),()] >> [4,5]

-- guard
-- roughly speaking, consider a monoid structure on a monad (MonadPlus)...
-- assume mempty >>= mx = mempty (as we have for [] and Maybe)

-- guard :: Bool -> m ()
-- guard True = return ()
-- guard False = mempty

filter'' p xs = xs >>= (\x -> guard (p x) >> return x)
z9 = filter'' even [1,2,4,5,6,7,11]

-- cf. z7
z9' = do x <- [1,2,4,5,6,7,11]
         guard (even x)
         return x
z9'' = [1,2,4,5,6,7,11] >>= (\x -> guard (even x) >> [x])


sdiv' :: Int -> Int -> Maybe Int
sdiv' n m = do guard (m /= 0)
               return $ div n m

sdiv'1 n m = guard (m /=0) >> return (div n m) 
               
-- when

-- when :: Bool -> m () -> m ()

-- when True mx = mx
-- when False mx = return ()

sdiv'' n m = do when (m == 0) (putStrLn "Exception ahead!!!")
                return $ div n m

sdiv''1 n m =  when (m == 0) (putStrLn "Exception ahead!!!") >> return (div n m)


--

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = do b <- p x
                       ys <- filterM' p xs
                       return (if b then x:ys else ys)

z10 = [[2,3],[5],[1,2],[3],[],[4,7],[1]] :: [[Int]]

shead :: [a] -> Maybe a
shead xs = do guard $ not (null xs)
              return $ head xs


evenheads :: [[Int]] -> Maybe [[Int]]
evenheads = filterM (\xs -> even <$> shead xs)

powset :: [a] -> [[a]]
powset = filterM (\_ -> [True,False])

--

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do y <- f x
                    ys <- mapM' f xs
                    return (y:ys)

question :: Int -> IO Int
question n = do putStrLn $ "Please square the number "
                        ++ show n
                s <- getLine
                return (read s)

z11 = [3,4,(-7),2,11] :: [Int]

quiz :: [Int] -> IO Bool
quiz ns = let test = and . zipWith (==) (map (^2) ns) in
          fmap test $ mapM question ns

{-- ap and liftM --}

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = mx >>= return . f

l1 = liftM (^2) [1,2,3]
l2 = liftM' (^2) [1,2,3]
l3 = fmap (^2) [1,2,3]

{- Deduce the functor laws for liftM' from 
 - the monad laws for >>= and return.
 -}

ap' :: Monad m => m (a -> b) -> m a -> m b
ap' mf mx = mf >>= \f -> (mx >>= return . f)

l4 = ap [(+1),(^2)] [1,2,3]
l5 = ap' [(+1),(^2)] [1,2,3]
l6 = [(+1),(^2)] <*> [1,2,3]


