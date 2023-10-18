import GHC.Float
-- for float2Double

{-- Declarative programming---making equations  --}

abc = 5
ab_e = 7 :: Int
_ab :: Int
_ab = 4
{-- Explicit lambda  --}
f1 = (\x y -> x) :: a -> b -> a
f2_ = \x y -> y
fNc n = (+) 3 n
fNb = \n -> (3 + n) `mod` 5
myList = [1,2,3] :: [Int]
{-- Functions are first-class objects --}
f2 = f1 f1 (+) (*) div 
ab'c = f2 5 (-7)

--abc = 5 
--f3 = \x -> x x
--n4abc = f2 5 -7
--A = 7

{-- let in --}
gh1 = let x = 3 in x^2
gh2 = x^2 
--where x = 3
 where x = 3 
{-- Notice the indent! --}

u = u
v = f1 124 u

{-- Built-in types --}

{-- Bool --}

x = True :: Bool
y = False

{-- It is not that easy...  --}
x0 :: Bool
x0 = x0
x1 = not x1
x2 = undefined :: Bool
x3 = True && x2
x4 = False && x2

z x y = if (&&) ((x || (not y)) && x) (not y) then x else y
myAnd = \x y -> if x then y else x
x' = True `myAnd` False :: Bool

{-- Char --}

c1 = 'H'
c2 = '\n' :: Char
{-- putting a char is not the same thing as that char itself --}
v0 = putChar c2

{-- Int (from [-2^63..2^63-1]) and Integer --}

n0 = 3 :: Int
n0' = (-3)
n1 = 3 
n2 = n0 + n1

mb1 = maxBound :: Int
--mb2 = maxBound :: Integer
mb3 = minBound :: Int

n3 = 2^63
n4 = 2^63 :: Int
-- What is the type of n3?

n3' = 2^63
n4' = 2^63 :: Int
-- What is the type of n3'?
{-- a LHS hole signifies we just do not need this value  --}
_ = n3' + n4'

{- NOTE on Int overflow.
 - Let Int be the range [-M..M-1] (currently M = 2^63). Let L = 2M.
 - On overflow, the value 'wraps around', so M is mapped to -M, M+1 to
 - (-M-1) etc. Thus, for any x \in \Z one has
 -      I(x) = (x + M) mod L - M,
 - where I(x) :: Int is an Int presentation of x, and all arithmetical
 - operations are 'absolute' (i.e. performed in \Z). Obiously, I(x) = x
 - for x :: Int. Hence, I(I(x)) = I(x) for all x \in \Z.
 -
 - E.g., one obtains I(2^63) = I(M) = (2M) mod (2M) - M = -M, as is the
 - case. For x = -M + (-M), we get I(x) = (-M) mod (2M) - M = M - M = 0.
 -
 - It makes no difference whether summands 'wrap around' before addition,
 - if the sum does. So one has
 -      I(x + y) = I(I(x) + I(y))
 - for all x, y \in \Z. Indeed, I(I(x) + I(y)) = ((x + M) mod L +
 - (y + M) mod L - M) mod L - M = ((x + y + 2M) mod L - M) mod L - M.
 - Note that 2M mod L = 0 and -M mod L = M. Thus,
 -  I(I(x) + I(y)) = ((x + y) mod L - (-M) mod L) mod L - M =
 -      = ((x + y + M) mod L) mod L - M = ((x + y) + M) mod L - M =
 -          = I(x + y).
 - Also, it is easy to see that
 -      I(x * y) = I(I(x) * I(y))
 -}

n6 = 10 :: Int
n7 = 11 :: Integer
--n8' = n6 + n7
n8 = n6 + fromIntegral(n7)


{-- Float and Double --}

fl = pi :: Float
db = pi :: Double

--v1 = fl + n6
--v2 = fl * db

v3 = db + fromIntegral(n6)
v4 = float2Double(fl) * db 

v5 = floor(sqrt(2))

{-- Pairs and tuples  --}

p0 = (True,(12,"Haskell B. Curry")) :: (Bool,(Int,String))
p1 = fst p0
p2 = fst (snd p0)
p3 = snd (snd p0)
t4 = (1,2,True,3.5)

{-- Lists and Strings --}

e0 = [] :: [b]
e1 = [] :: (Eq a) => [a]
e2 = [] 
e3 = [] :: [Int]

bl0 = e0 == e1
bl0'' = e1 == e2
bl1 = e2 == e3
bl2 = e1 == e3

bl2' = (1 : 2 : 3 : [] == [1,2,3])

bools = [True,False,True] :: [Bool]
nums = [[],[1,2,3],[7]] :: [[Int]]
chars = ['a','b','c'] :: [Char]

s1 = "Qwerty!" :: String
s2 = "Qwerty!"
s3 = ['Q','w','e','r','t','y','!']

bl3 = s1 == s2 && s2 == s3

l1 = [(False,'O'),(True,'1')] :: [(Bool,Char)]

p5 = ([False,True],['0','1']) :: ([Bool],[Char])

-- What is the most general type for l2?
l2 = [\x y-> x, \x y -> y]
l3 = [tail,init,reverse] :: [[a] -> [a]]

{-- More operations over these types --}

bl4 = (1,2) < (1,3)
bl5 = "Are" < "Arena"

s4 = show 15
--n10' = read "16"
n10 = read "16"

n11 = n10 - read s4

{-- 'Functional programming'  --}

af1 :: (a -> b -> c) -> (a -> b) -> a -> c
af1 = \x y z ->  x z (y z)
--af2 :: (Num a) => (a -> a) -> a -> a 
af2 = af1 (+)
af3 = af2 id
af4 = af2 (\x -> (3 * x + 1)^2)
af5 = af2 (+1)

af6 = af2 (af2 id)
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b 
af7 = af2 . af2 $ id

{-- Pattern matching --}
bf0 :: Bool -> String
bf0 = \b -> case b of
                 True -> "yes"
                 False -> "no"

bf0' b = case b of
                 True -> "yes"
                 _ -> "no"

bf0'' = \b -> if b then "yes" else "no"


bf0''' False = "no"
bf0''' _ = "yes"

bf1 :: Int -> Int
bf1 n = case n of
            1           -> 5
            2           -> 4
            3           -> 3
            otherwise -> 2018

-- Guards are allowed here to specify patterns further.
-- See Haskell Report, 3.13.
bf1' n = case n of
            1              -> 5
-- testing a Boolean expression for being True after we know n = 2 
            2 | 2*n == 4   -> 4
-- matching n with 4 (which fails) after we know n = 3 
            3 | 4 <- n     -> 3
-- 'otherwise' here is a local 'VARIABLE', not the defined term being equal
-- to True (this term is shadowed in this context);
-- mathcing with a variable always succedes so we have the desired effect we
-- quite (misleadingly) signify with this name.
            others_may_be_wiser -> 2018 + others_may_be_wiser

bf2 :: Int -> Int
bf2 n = if n < 0 then
                    (-1)
                else 
                    if n == 0 then
                        2016
                    else
                        2018
bf3 :: Int -> Int
bf3 = \n -> if n < 0 then 3 else if n > 0 then 2 else (-6)

bf4 :: (Int,Char) -> Int
bf4 x = case x of 
            (4,_)   -> 18
            (_,'u') -> 19
--            (4,'v') -> (-1)
            (5,'A')   -> 12
            

bf5 :: [Int] -> [Int]
bf5 [] = []
bf5 [_,2,_] = [-2]
bf5 [1,3,7] = [(-1)]
-- as-pattern binds a local 'variable' to the expression being matched
bf5 list@(x:xs) = x * sum xs : list

bf6 :: Int -> Int -> Bool
{-- Local 'variables' are assigned some values during matching;
 - nothing more is done essentially.   --}
--bf6 x x = True
--bf6 _ _ = False
bf6 x y = x == y

bf7 :: (a,b) -> (a,(a,b))
bf7 (x,y) = (x,(x,y))

{-- Matching in lambda --}
bf7' = \(x, y) -> (x,(x,y))

bf7'' = \p@(x,_) -> (x, p)

{- A type synonym  -}
type Triple a = (a,a,a)
type R3 = Triple Float

vsum :: R3 -> R3 -> R3
vsum (x,y,z) (u,v,w) = (x + u, y + v, z + w)

vsum1 :: R3 -> R3 -> R3
vsum1 p q = (pr 1 p + pr 1 q, pr 2 p + pr 2 q, pr 3 p + pr 3 q)
   
pr :: Int -> Triple a -> a
pr 1 (x,_,_)  = x 
pr 2 (_,y,_) = y
pr 3 (_,_,z) = z

{-- Not a recommended way of error handling  --}
err = error "Too short list"
-- 'Safe third'
sthird :: [a] -> a
sthird (_:_:x:_) = x
sthird _ = err

{-- Guards  --}
sthird1 :: [a] -> a
sthird1 xs | length xs >= 3     = xs !! 2
           | otherwise          = err

sgn :: Int -> Int
sgn n | n < 0   = (-1)
      | n == 0  = 0
      | n > 0   = 1

shead :: [a] ->  a
shead xs | null xs      = err
         | otherwise    = head xs

stail :: [a] -> [a]        
stail [] = err
stail (_:xs) = xs

{-- A modicum of recursion  --}

fac_a :: Int -> Int
-- The clause order is essential since the first wins,
-- so we do not go below zero.
fac_a 0 = 1
fac_a n = n * fac_a (n-1)

fac_b :: Int -> Int
fac_b n | n > 0      = n * fac_b (n-1)
        | otherwise  = 1

binom :: Int -> Int -> Int
binom _ 0 = 1
binom 0 _ = 0
binom n k = binom (n - 1)  k + binom (n - 1) (k - 1)

acker :: Integer -> Integer -> Integer
acker 0 n = n + 1
acker m 0 = acker (m - 1) 1
acker m n = acker (m - 1) (acker m (n - 1))

bracket :: Integer -> Integer
bracket n | odd n        = 0
          | n == 0       = 1
          | otherwise    = sum [ bracket (i*2) * bracket (m - (i*2)) 
                                | let m = n - 2, i <- [0 .. (m `div` 2)] ]

catalan n = bracket (n * 2)



cf1 :: Int -> Int
cf1 n = cf1 (n + 1)


{-- Mutual recursion --}
even' :: Int -> Bool
odd' :: Int -> Bool

even' 0 = True
even' n = odd' (n - 1)

odd' 0 = False
odd' n = even' (n - 1)


{-- Recursion on lists  --}
length' :: [a] -> Int
length' [] = 0
-- (:) binds weaker than a function application
length' (x : xs) = 1 + length' xs

-- (:) binds stronger than =
last' :: [a] -> a
last' [] = error "Empty list!"
last' [x] = x
last' (_:xs) = last' xs  


init' :: [a] -> [a]
init' [] = []
init' (x:xs) | null xs    = []
             | otherwise  = x : init' xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys



{-- "Bubble sorting" --}

bubble :: (Ord a) => a -> [a] -> [a]
bubble x [] = [x]
bubble x (y:ys) | x <= y        = x : (bubble y ys)
                | otherwise     = y : (bubble x ys)

isSort :: (Ord a) => [a] -> Bool
isSort xs = and [ x <= y | (x,y) <- zip xs (tail xs) ]

bubsort :: (Ord a) => [a] -> [a]
bubsort xs = if isSort xs then
                xs
             else
                bubsort (bubble (head xs) (tail xs))

{-- "Insertion sorting" --}

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y    = x : y : ys
                | otherwise = y : (insert x ys)

inssort :: (Ord a) => [a] -> [a]
inssort [] = []
inssort (x:xs) = insert x (inssort xs)

{-- Merge sorting --}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take l xs, drop l xs)
   where l = div (length xs) 2

mrgsort :: Ord a => [a] -> [a]
mrgsort xs | length xs < 2  = xs
           | otherwise      = merge (mrgsort(fst(halve xs))) 
                                (mrgsort(snd(halve xs)))

{-- "Quick sorting" --}

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = lesser ++ [x] ++ greater
                where   lesser  = qsort [y | y <-xs, y <= x]
                        greater = qsort [y | y <-xs, y > x]


{-- Largest common subsequence "via Dynamic Programming" --}

lcs' :: (Eq a) => [a] -> [a] -> Int -> Int -> [a]
lcs' xs ys i j | i < 0 || j < 0    = []
              | x == y              = x : lcs' xs ys (i - 1) (j - 1)
              | otherwise           = if (length u >= length v) then
                                            u
                                        else
                                            v
              where x = xs !! i; y = ys !! j
                    u = lcs' xs ys i (j - 1)
                    v = lcs' xs ys (i - 1) j

lcs :: (Eq a) => [a] -> [a] -> [a]
lcs xs ys = reverse (lcs' xs ys (length xs - 1) (length ys - 1))



