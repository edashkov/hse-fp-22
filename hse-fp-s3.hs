{-- More practice with lists  --}
-- For ord :: Char -> Int
import Data.Char (ord,chr)

l0 = [1,2,3,4,5]

l1 = [x^2 | x <- l0]

l2 = [x^2 | x <- [1..]]

l2' = take 100 [(x,y) | x <- l0, y <- [1..]]
l2'' = take 100 [(x,y) | y <- [1..], x <- l0]

l3 = [(x,y,z) | x <- l0, y <- l0, let z = x + y]

l4 = [(x,y,z) | x <- l0, y <- l0, let z = x + y, even z]

l6 = [ z | (_,_,z) <- l3 ]

f1 :: Int -> [(Int,Int)]
f1 n = [ (x,y) | x <- [0..n], y <- [0..x] ]

f2 :: Int -> [[(Int, Int)]]
f2 n = [ [ (x,y) | y <- [0..x] ] | x <- [0..n] ]

f3 :: Int -> [(Int,Int)]
f3 = concat . f2

----

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

primes :: [Int]
primes = [x | x <- [2..], factors x == [1,x] ]

nPrime n = primes !! n

perfects :: [Int]
perfects = [x | x <- [1..], 2*x == sum (factors x)]

--
--

l7 = zip l0 l1
l7' = zip l1 l2

positions :: (Eq a) => a -> [a] -> [Integer]
positions x xs = [i | (y,i) <- zip xs [0..], y == x]

--scalprod :: [Int] -> [Int] -> Int
scalprod xs ys = sum [ a*b | (a,b)  <- (zip xs ys)]

twins :: [(Int,Int)]
twins = [ (p,q) | (p,q) <- zip primes (tail primes), q == p + 2 ]
 
--

let2int :: Char -> Int
let2int c = ord c - ord ' '

int2let :: Int -> Char
int2let n = chr (n + ord ' ')

shift :: Int -> Char -> Char
shift n c = int2let (((let2int c) + n) `mod` (ord '~' - ord ' ' + 1))

encode ::  Int -> String -> String
encode n cs = [shift n c | c <- cs]

decode ::  Int -> String -> String
decode n cs = [shift (-n) c | c <- cs]

--
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x :  map' f xs

b0 f n = take n (map f [0..]) == take n (map' f [0..]) 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : ys else ys
        where ys = filter' p xs

l0a = filter even (map (\x->x^2) [0..100])

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) | p x         = all' p xs
              | otherwise   = False

b1 = all' even (map (2*) [0..100])
b2 = all' even [0..100]
b3 = any even [0..100]

isPrime :: Int -> Bool
isPrime n = n > 1 && and [ n `mod` x /= 0 | x <- [2..(n-1)] ]

l1a = takeWhile isPrime (filter odd [2..])
l2a = dropWhile (\x -> x < 10) [0..100]
l3a = dropWhile isPrime [2,3,5,19,7,23,91,17,13,31]

--

foldr_ :: (a -> b -> b) -> b -> [a] -> b
foldr_ f x [] = x
foldr_ f x (y:ys) = f y (foldr_ f x ys)

foldl_ :: (b -> a -> b) -> b -> [a] -> b
foldl_ f x [] = x
foldl_ f x (y:ys) = foldl_ f (f x y)  ys

l4a = [1,2,3]

n0 = foldr_ (+) 0 l4a
n1 = foldl_ (+) 0 l4a

n2 = foldr (\u v -> u^v) 2 l4a
n3 = foldl (\u v -> u^v) 2 l4a

l5 = scanr (\u v -> u^v) 2 l4a
l6a = scanl (\u v -> u^v) 2 l4a

scanr_ :: (a -> b -> b) -> b -> [a] -> [b]
scanr_ f x [] = [x]
scanr_ f x (y:ys) = f y (head z) : z
                where z = scanr_ f x ys

listid :: [a] -> [a]
listid = foldr (:) []

conc' :: [a] -> [a] -> [a]
conc' ys zs   = foldr (:) zs ys

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\u v -> f u : v) []

sffx :: [a] -> [[a]]
sffx = scanr (:) []

--

l7a = take 10 (iterate (^2) 3)
l8a = take 10 (iterate (*2) 1)

iterate' :: (a -> a) -> a -> [a]
iterate' f x =  scanl (\u _ -> f u) x (repeat 0)

-- replicate; repeat
--

type Bin = [Int]

bin2int :: Bin -> Int
bin2int bits = foldl (\x y -> 2*x + y) 0 bits

int2bin :: Int -> Bin
int2bin 0 = []
int2bin n = (int2bin (n `div` 2)) ++ [n `mod` 2]

{-- Remove consecutive duplicates from a list --}

duprem :: (Eq a) => [a] -> [a]
duprem = foldr (\u vs -> if null vs || u /= head vs then u : vs else vs) []

{-- Group consecutive duplicates together  --}

grp :: (Eq a) => a -> [[a]] -> [[a]]
grp x [] = [[x]]
grp x (ls:lss) | x == head ls   = (x : ls) : lss
               | otherwise      = [x] : ls : lss

dupgrp :: (Eq a) => [a] -> [[a]]
dupgrp = foldr grp []

--

comp' :: (b -> c) -> (a -> b) -> a -> c
comp' g f x  = g (f x)

odd' = not . even

f10 :: [Int] -> Int
f10 =  sum . (++) [1,2,3] . map (^2) . filter even

itcomp :: Int -> (a -> a) -> (a -> a)
itcomp 0 f = id
itcomp n f = f . (itcomp (n - 1) f)

--itcomp' :: Int -> (a -> a) -> (a -> a)
itcomp' n f = (iterate ((.) f) id) !! n

compose :: [a -> a] -> a -> a
compose = foldr (.) id

itcomp'' n  = compose . (replicate n)

--

splitAt' _ [] = ([],[])
splitAt' n xs | n <= 0 = ([],xs)
splitAt' n (x:xs) = (x:p, s)
                    where (p,s) = splitAt' (n-1) xs 

splitAt'' n  = foldr (\(i,x) pair -> if i <= n then (x:fst pair,snd pair) else ([],x:snd pair)) ([],[]) . zip ([1..] :: [Int])

-- notice that length is based on foldl'; hence it never overflows.
-- Morover, these length-based implementation work DO NOT store
-- the whole list in memory, as length;

-- Also, library functions are compiled to object code, while GHCi 'compiles' things
-- to some intermediate bytecode by default; one can try changing this with
-- > :set -fobject-code
-- (you can notice an *.o file generated as a result).
halve0 xs = splitAt (length xs `div` 2) xs
halve1 xs = splitAt' (length xs `div` 2) xs
halve2 xs = splitAt'' (length xs `div` 2) xs

{- In theory, halve3 may be twice faster than halve0 (at least than halve2) as
 - it traverses the list xs just once. In fact, it is MUCH worse than halve0
 - since we HAVE to run over the whole list until we know len; before that moment,
 - we cannot decide between the branches of if, hence we have to store a giant
 - thunk in the memory: 
 - foldr f ([],[],0) [(1,'a'),(2,'b'),(3,'c')] = 
 -  if 1 <= len then ('a':p,s,len) else ([],'a':s,1+len)
 -    where (p,s,len) = if 2 <= len' then ...
 - This readily overflows the stack.
 -}
halve3 xs = (p,s) where
            helper :: [a] -> ([a],[a],Int)
            f (i,x) = \(p,s,len) -> if i <= len then (x:p,s,len) else ([],x:s,1+len)
            helper = foldr f ([],[],0) . zip ([1..] :: [Int])
            (p,s,_) = helper xs

