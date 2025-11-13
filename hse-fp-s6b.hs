import Control.Monad
import Control.Monad.ListM (sortByM)
import System.Random

{-- A (limitedly) useful monad --}

{-
 - We want to make a counter
 - holding some integer value.
 -}
 
type Ctr = Int

newtype Upd a = U {getUpd :: Ctr -> (Ctr,a)}

-- Upd a is isomorphic to State Int a

--getUpd :: Upd a -> Ctr -> (Ctr,a)
--getUpd (U f) = f

instance Functor Upd where
--fmap :: (a -> b) -> (Upd a) -> (Upd b)
    fmap f (U g) = U $ (\(c,x) -> (c, f x)) . g

-- the same with Functor ((,) Ctr)
--     fmap f (U g) = U $ fmap f . g

--    fmap = liftM      

instance Applicative Upd where
-- pure :: a -> Upd a
    pure x = U $ \c -> (c,x)
-- <*> :: Upd (a -> b) -> Upd a -> Upd b
    (<*>) (U f) (U g) = U $ \c -> let (c', h) = f c
                                      (c'', x) = g c'
                                  in (c'', h x)
--    (<*>) = ap


instance Monad Upd where
-- (>>=) :: Upd a -> (a -> Upd b) -> Upd b
    (U f) >>= g = U $ \c -> let
                             (c',x)   = f c
                            in getUpd (g x) c'

----

incctr :: Upd ()
incctr = U $ \c -> (c + 1, ())

decctr :: Upd ()
decctr = U $ \c -> (c - 1, ())

-- cf. get and put
getctr :: Upd Ctr
getctr = U $ \c -> (c,c)

putctr :: Ctr -> Upd ()
putctr n = U $ \_ -> (n,())

a1 = putctr 0 >> incctr >> incctr >> decctr >> getctr
-- cf. evalState and execState
x1 = getUpd a1 2018

-- cf. evalState
usectr :: Ctr -> Upd a -> a
usectr initval action = snd $ getUpd action initval

-- cf. modify
modctr :: (Ctr -> Ctr) -> Upd ()
modctr f = U $ \c -> (f c, ())

x2 = usectr 5 a1
x3 = usectr 2 (incctr >> modctr (*12) >> decctr >> getctr)

----

{- Some parser: test a string if it is
 - "well-formed" via calculacting its
 - bracket balance.
 -}

is_wf' :: String -> Upd Bool
is_wf' [] = do bal <- getctr
               return  (bal == 0)

is_wf' (x:xs) = do bal <- getctr
                   if (bal < 0) then
                    return False
                   else
                    do case x of
                        '(' -> incctr
                        ')' -> decctr
                        _   -> return ()
                       is_wf' xs   

is_wf :: String -> Bool
is_wf xs = usectr 0 (is_wf' xs)

{-- Cf. the State monad from  Control.Monad.State.Lazy --}

-- Count comparisons in sorting algorithms

qsort :: (Ord a) => [a] -> Upd [a]
qsort [] = return []
qsort (x:xs) = do lesser <- qsort [y | y <-xs, y <= x]
                  greater <- qsort [y | y <-xs, y > x]
                  modctr (+(2 * length xs))
                  return $ lesser ++ [x] ++ greater

trace :: (Ord a) => ([a] -> Upd [a]) -> [a] -> ([a],Ctr)
trace sort xs =  usectr 0 (do {s <- sort xs; n <- getctr; return (s,n)})

trqsort :: (Ord a) => [a] -> ([a], Ctr)
trqsort = trace qsort

doTest :: Int -> ([Int] -> ([Int],Ctr)) -> IO (Int,Int,Int)
doTest n sort = do gen <- newStdGen
                   let xs = take n $ randomRs ((-999),999) gen :: [Int]
                   let z = sort xs
                   let z' = sort $ fst z
                   let z'' = sort $ reverse (fst z)
                   return (snd z, snd z', snd z'')

--

insert :: (Ord a) => a -> [a] -> Upd [a]
insert x [] = return [x]
insert x (y:ys) = do  incctr
                      if (x <= y) then
                        return $ x : y : ys
                      else
                        do zs <- insert x ys
                           return $ y : zs

inssort :: (Ord a) => [a] -> Upd [a]
inssort [] = return []
inssort (x:xs) = inssort xs >>= insert x

trinssort :: (Ord a) => [a] -> ([a], Ctr)
trinssort = trace inssort

--

merge :: Ord a => [a] -> [a] -> Upd [a]
merge [] ys = return ys
merge xs [] = return xs
merge (x:xs) (y:ys) = do incctr
                         if (x <= y) then
                          do zs <- merge xs (y:ys)
                             return $ x : zs
                         else
                          do zs <- merge (x:xs) ys  
                             return $ y : zs

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

mrgsort :: (Ord a) => [a] -> Upd [a]
mrgsort xs = do if (length xs < 2) then
                    return xs
                else
                 do let (ys',zs') = halve xs
                    ys <- mrgsort ys'
                    zs <- mrgsort zs'
                    merge ys zs

trmrgsort :: (Ord a) => [a] -> ([a], Ctr)
trmrgsort = trace mrgsort

doTests :: Int -> ([Int] -> ([Int],Ctr)) -> IO ()
doTests n sort = do s <- sequence ([1..100] >> [doTest n sort])
                    let n1 = sum $ map (\(x,_,_) -> x) s
                    let n2 = sum $ map (\(_,y,_) -> y) s
                    let n3 = sum $ map (\(_,_,z) -> z) s
                    putStr "Random: " >> (print $ fromIntegral(n1) / 100.0)
                    putStr "Sorted: " >> (print $ fromIntegral(n2) / 100.0)
                    putStr "Sorted reversely: " >> (print $ fromIntegral(n3) / 100.0)
               
-- Let's optimize sortings a little...    

qsort' :: (Ord a) => [a] -> Upd [a]
qsort' [] = return []
qsort' (x:xs) = do let (lesser', greater', len) = partition (<=x) xs
                   modctr (+len)            
                   lesser <- qsort' lesser'
                   greater <- qsort' greater'
                   return $ lesser ++ [x] ++ greater

partition :: (a -> Bool) -> [a] -> ([a], [a], Int)
partition p = foldr op ([], [], 0)
              where op x (ys, zs, l) | p x         = (x:ys, zs, l+1)
                                     | otherwise   = (ys, x:zs, l+1)

-- Can we avoid modifying the sorting algorithm internals
-- when counting comparison?
-- Surely!
-- Yet this requires a monadic version of sort (or sortBy) as monadlist library
-- provides. Indeed, the sort routine must chain effects (>>=) internally.

-- It seems that monadic versions are *strictly* more general. Clearly, one can
-- emulate the pure version with the Identity monad. On the other hand, if there
-- were a way to 'lift' an arbitrary sorting function to an arbitrary monad, then
-- an effect might reveal some implementation details of sorting (say, the elements
-- being compared to each other), which may differ for different algorinthms.
-- This would violate the 'referential transparency' principle implying that any two
-- extensionally equal PURE functions must be interchangable in terms of extensional
-- equality. Indeed, one could then obtain
--  reveal . lift sort1 $ [1..100] /= reveal . lift sort2 $ [1..100]
-- whereas
--  sort1 [1..100] == sort2 [1..100]

compareUpd :: (Ord a) => a -> a -> Upd Ordering
compareUpd x y = incctr >> return (compare x y)

w1 = trace (sortByM compareUpd) [10,5,3,1,8,5,0,2,7,5,1,7,7,41,0,5,2,6,9,1]

      
